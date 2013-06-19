(ns ^{:pattern-expansion-context :emf}
  ttc-2013-live.core
  (:use funnyqt.protocols
        funnyqt.emf
        funnyqt.query.emf
        funnyqt.pmatch
        funnyqt.query
        funnyqt.in-place)
  (:require [funnyqt.visualization :as viz]))

;;* Load the metamodel and model

(load-metamodel "models/bpmn20.ecore")
(load-metamodel "models/bpmn20exec.ecore")

(def model (load-model "models/process1.xmi"))

;;* The transformation

;;** Helpers

(defn start-event [proc]
  (the (reachables proc [p-seq [p-+ <>--] [p-restr 'StartEvent]])))

(defn end-event [proc]
  (the (reachables proc [p-seq [p-+ <>--] [p-restr 'EndEvent]])))

(defn running? [proc-inst]
  (= (eget proc-inst :state)
     (eenum-literal 'ProcessState.RUNNING)))

(defn set-token! [t el]
  (eset! t :element el))

(defn get-token [pi el]
  (the (filter #(= (eget % :element) el) (eget pi :tokens))))

(defn has-token? [pi el]
  (exists? #(= (eget % :element) el) (eget pi :tokens)))

(defn show []
  (viz/print-model model ".gtk"))

;;** Process instantiation

(defrule instantiate-process [m]
  [p<Process>
   :let [se (start-event p)
         event-defs (eget se :eventDefinitions)]
   :when (or (empty? event-defs)
             (every? #(has-type? % 'TimerEventDefinition)))]
  (let [pi (ecreate! m 'ProcessInstance)
        t (ecreate! 'bpmn20exec.Token)]
    (eadd! pi :tokens t)
    (eset! pi :process p)
    (eset! pi :state (eenum-literal 'ProcessState.RUNNING))
    (set-token! t se)))

(defrule terminate-normally [m]
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

(defrule start-process [m]
  [pi<ProcessInstance> -<tokens>-> t<bpmn20exec.Token>
   -<element>-> se<StartEvent>
   pi -<process>-> p<Process>
   :when (running? pi)]
  (set-token! t (first (eget se :outgoing))))

(defrule end-process [m]
  [pi<ProcessInstance> -<tokens>-> t<bpmn20exec.Token>
   -<element>-> sf<SequenceFlow> -<targetRef>-> ee<EndEvent>
   :when (running? pi)]
  (set-token! t ee))

;;** Entering and leaving Tasks

(defrule enter-task [m]
  [pi<ProcessInstance> -<tokens>-> t<bpmn20exec.Token>
   -<element>-> sf<SequenceFlow> -<targetRef>-> tsk<Task>
   :when (running? pi)]
  (set-token! t tsk))

(defrule leave-task [m]
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

(defrule enter-parallel-gateway [m]
  [pi<ProcessInstance> -<tokens>-> t<bpmn20exec.Token>
   -<element>-> sf<SequenceFlow> -<targetRef>-> pg<ParallelGateway>
   :let [isfs (eget pg :incoming)]
   :when (and (running? pi)
              (forall? (partial has-token? pi) isfs))]
  (doseq [isf isfs
          :let [t (get-token pi isf)]]
    (edelete! t))
  (let [t (ecreate! 'bpmn20exec.Token)]
    (eadd! pi :tokens t)
    (set-token! t pg)))

(defrule leave-parallel-gateway [m]
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

#_(do
  (instantiate-process model)
  (start-process model)
  (enter-task model)
  (leave-task model)
  (enter-parallel-gateway model)
  (leave-parallel-gateway model)
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
  (viz/print-model model ".gtk"))

;(viz/print-model model ".gtk")

