(ns ^{:pattern-expansion-context :emf}
  ttc-2013-live.core
  (:use funnyqt.protocols
        funnyqt.emf
        funnyqt.query.emf
        funnyqt.query
        funnyqt.in-place)
  (:require [funnyqt.visualization :as viz])
  (:import (org.eclipse.emf.ecore.util EcoreUtil)))

;;* Load the metamodel and model

(load-metamodel "models/bpmn20.ecore")
(load-metamodel "models/bpmn20exec.ecore")

(def model (load-model "models/process1.xmi"))

;;* The transformation

;;** Helpers

(defn start-event
  "Gets the StartEvent of Process proc."
  [proc]
  (the (reachables proc [p-seq [p-+ <>--] [p-restr 'StartEvent]])))

(defn end-event
  "Gets the EndEvent of Process proc."
  [proc]
  (the (reachables proc [p-seq [p-+ <>--] [p-restr 'EndEvent]])))

(defn running?
  "Returns true iff the ProcessInstance proc-inst is in the RUNNING state."
  [proc-inst]
  (= (eget proc-inst :state)
     (eenum-literal 'ProcessState.RUNNING)))

(defn set-token!
  "Sets the Token t to FlowElement el."
  [t el]
  (eset! t :element el))

(defn get-token
  "Gets the Token that's currently attached to el.
  If there's none, nil is returned."
  [pi el]
  (first (filter #(= (eget % :element) el) (eget pi :tokens))))

(defn show
  "Visualizes the model."
  []
  (viz/print-model model ".gtk"))

(defn matches?
  "Returns true if the given rule is applicable on the given model right now."
  [model rule]
  (let [m (atom false)]
    (binding [*on-matched-rule-fn* (fn [& args]
                                     (swap! m (constantly true))
                                     false)]
      (rule model))
    @m))

;;** Process instantiation

(defrule ^:debug instantiate-process
  "Matches a Process that
    1. has no or only TimerEventDefinitions
    2. has not been executed already

  and then creates a new ProcessInstance with Token targeting the Process's
  StartEvent.  The ProcessInstance gets the state RUNNING."
  [m]
  [p<Process>
   :let [se (start-event p)
         event-defs (eget se :eventDefinitions)]
   :when (and (or (empty? event-defs)
                  (every? #(has-type? % 'TimerEventDefinition)))
              (not (exists? #(= p (eget % :process))
                            (eallobjects m 'ProcessInstance))))]
  (let [pi (ecreate! m 'ProcessInstance)
        t (ecreate! 'bpmn20exec.Token)]
    (eadd! pi :tokens t)
    (eset! pi :process p)
    (eset! pi :state (eenum-literal 'ProcessState.RUNNING))
    (set-token! t se)))

(defrule ^:debug terminate-normally
  "Matches a RUNNING ProcessInstance whose Tokens all either point to the
  Processes' EndEvent or all point to FlowNodes without outgoing
  SequenceFlows.
  It then deletes all tokens and sets the ProcessInstance state to FINISHED."
  [m]
  [pi<ProcessInstance>
   :when (running? pi)
   :let [p (eget pi :process)
         ee (end-event p)]
   :when (or (and ee (forall? #(= ee (eget % :element))
                              (eget pi :tokens)))
             (and (not ee)
                  (forall? #(empty? (eget % :outgoing))
                           (eget pi :tokens))))]
  (doseq [t (eget pi :tokens)]
    (edelete! t))
  (eset! pi :state (eenum-literal 'ProcessState.FINISHED)))

;;** Starting and ending

(defrule ^:debug start-process
  "Matches a RUNNING ProcessInstance whose token points to a StartEvent.
  It then moves the token to the outgoing SequenceFlow."
  [m]
  [pi<ProcessInstance> -<tokens>-> t<bpmn20exec.Token>
   -<element>-> se<StartEvent>
   :when (running? pi)]
  (set-token! t (first (eget se :outgoing))))

(defrule ^:debug end-process
  "Matches a RUNNING ProcessInstance whose token points to a SequenceFlow
  ending in an EndEvent.
  It then moves the token to the EndEvent."
  [m]
  [pi<ProcessInstance> -<tokens>-> t<bpmn20exec.Token>
   -<element>-> sf<SequenceFlow> -<targetRef>-> ee<EndEvent>
   :when (running? pi)]
  (set-token! t ee))

;;** Entering and leaving Tasks

(defrule ^:debug enter-task
  "Matches a RUNNING ProcessInstance whose token points to a SequenceFlow
  ending in a Task.
  It then moves the token to the Task."
  [m]
  [pi<ProcessInstance> -<tokens>-> t<bpmn20exec.Token>
   -<element>-> sf<SequenceFlow> -<targetRef>-> tsk<Task>
   :when (running? pi)]
  (set-token! t tsk))

(defrule ^:debug leave-task
  "Matches a RUNNING ProcessInstance whose token points to a Task.
  This Task needs to have at least one outgoing SequenceFlow.
  It then deletes the Token and creates a new Token attached to each
  of those outgoing SequenceFlows."
  [m]
  [pi<ProcessInstance> -<tokens>-> t<bpmn20exec.Token>
   -<element>-> tsk<Task>
   :let [osfs (eget tsk :outgoing)]
   :when (and (seq osfs) (running? pi))]
  (edelete! t)
  (doseq [of osfs
          :let [t (ecreate! 'bpmn20exec.Token)]]
    (eadd! pi :tokens t)
    (set-token! t of))
  true)

;;** Parallel gateways

(defrule ^:debug enter-parallel-gateway
  "Matches a RUNNING ProcessInstance whose token points to a SequenceFlow that
  targets a ParallelGateway, and where all other SequenceFlows targeting this
  ParallelGateway also have tokens.
  It then deletes all tokens and creates a new Token targeting the
  ParallelGateway."
  [m]
  [pi<ProcessInstance> -<tokens>-> t<bpmn20exec.Token>
   -<element>-> sf<SequenceFlow> -<targetRef>-> pg<ParallelGateway>
   :let [isfs (eget pg :incoming)]
   :when (and (running? pi)
              (forall? (partial get-token pi) isfs))]
  (doseq [isf isfs
          :let [t (get-token pi isf)]]
    (edelete! t))
  (let [t (ecreate! 'bpmn20exec.Token)]
    (eadd! pi :tokens t)
    (set-token! t pg)))

(defrule ^:debug leave-parallel-gateway
  "Matches a RUNNING ProcessInstance whose token targets a ParallelGateway
  with at least one outgoing SequenceFlow.
  It then deletes the token and creates one new Token for each outgoing
  SequenceFlow (targeting that)."
  [m]
  [pi<ProcessInstance> -<tokens>-> t<bpmn20exec.Token>
   -<element>-> pg<ParallelGateway> -<outgoing>-> sf<SequenceFlow>
   :let [osfs (eget pg :outgoing)]
   :when (running? pi)]
  (edelete! t)
  (doseq [osf osfs
          :let [t (ecreate! 'bpmn20exec.Token)]]
    (eadd! pi :tokens t)
    (set-token! t osf))
  true)

;;* Rule execution strategies

(def all-rules [instantiate-process terminate-normally
                start-process end-process
                enter-task leave-task
                enter-parallel-gateway leave-parallel-gateway])

(defn print-rule-name [r args match]
  (println "executing" r)
  true)

(defn execute-randomly
  "Iteratively randomly chooses some applicable rule and executes that until
  there's no applicable rule anymore."
  [model]
  (binding [*on-matched-rule-fn* print-rule-name]
    (iteratively #(choose all-rules %) model)))

(defn execute-interactively
  "Iteratively prints applicable rules and lets the user decide which one to
  choose."
  [model]
  (let [matching-rules (vec (filter (partial matches? model) all-rules))]
    (if (seq matching-rules)
      (do
        (println "These rules match:")
        (dotimes [i (count matching-rules)]
          (println (format "  %s. %s" i (matching-rules i))))
        (println "Select one: ")
        (let [s (java.util.Scanner. *in*)
              answer (loop [x (.nextInt s)]
                       (if (or (< x 0) (>= x (count matching-rules)))
                         (recur (.nextInt s))
                         x))]
          ((matching-rules answer) model))
        (recur model))
      (println "No rule matches anymore.  Finished!"))))

(defn execute-prioritized
  "Pretty much like execute-randomly (which see), but prioritizes the
  enter-task rule."
  [model]
  (when (binding [*on-matched-rule-fn* print-rule-name]
          (or (iteratively #(enter-task %) model)
              (choose all-rules model)))
    (recur model)))

;; That's one sequence of rule applications that executes the example process.
#_(do
  (instantiate-process model)
  (start-process model)
  (enter-task model)
  (leave-task model)
  (enter-parallel-gateway model)
  (leave-parallel-gateway model)
  (show)
  (enter-task model)
  (enter-task model)
  (leave-task model)
  (leave-task model)
  (enter-parallel-gateway model)
  (leave-parallel-gateway model)
  (enter-task model)
  (leave-task model)
  (end-process model)
  (terminate-normally model)
  (show))

;;* State space exploration

;; I've given up on this since I'd need to build the complete machinery needed
;; for that from scratch... :-(
