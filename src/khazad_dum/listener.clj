(ns khazad-dum.listener)

(defprotocol Listener
  (on-message [this message])
  (on-unit [this unit])
  (on-namespace [this namespace])
  (on-report [this report]))

(declare ^:dynamic *listener*)

(defn report-message [message]
  (on-message *listener* message))

(defn report-unit [unit]
  (on-unit *listener* unit))

(defn report-namespace [namespace]
  (on-namespace *listener* namespace))

(defn report-run [report]
  (on-report *listener* report))

;;
;; Identity listener
;; 

(declare identity-report-message identity-report-unit identity-report-namespace identity-report-run)
     
(defrecord IdentityListener [messages units namespaces reports]
  Listener
  (on-message [this message] (identity-report-message this message))
  (on-unit [this unit] (identity-report-unit this unit))
  (on-namespace [this namespace] (identity-report-namespace this namespace))
  (on-report [this report] (identity-report-run this report)))

(defn messages [listener]
  @(:messages listener))

(defn units [listener]
  @(:units listener))

(defn namespaces [listener]
  @(:namespaces listener))

(defn reports [listener]
  @(:reports listener))

(defn identity-report-message [listener message]
  (swap! (:messages listener) #(conj % message))
  message)

(defn identity-report-unit [listener unit]
  (let [messages (messages listener)
        unit (if (or (seq messages) (:messages unit))
               (assoc unit :messages (vec (concat messages (:messages unit))))
               unit)]
    (reset! (:messages listener) [])
    (swap! (:units listener) #(conj % unit))
    unit))

(defn identity-report-namespace [listener namespace]
  (let [units (units listener)
        namespace (if (seq units) (assoc namespace :units (vec (concat units (:units namespace))))
                    namespace)]
    (reset! (:units listener) [])
    (swap! (:namespaces listener) #(conj % namespace))
    namespace))

(defn identity-report-run [listener report]
  (let [namespaces (namespaces listener)
        report (if (seq namespaces) (assoc report :namespaces
                                           (vec (concat namespaces (:namespaces report))))
                   report)]
    (reset! (:namespaces listener) [])
    (swap! (:reports listener) #(conj % report))
    report))

(defn identity-listener [] (IdentityListener. (atom []) (atom []) (atom []) (atom [])))

;;
;; Test listener
;;

(defn- test-report-message [message]
  (when (#{:failure} (:type message))
    (print (:message message))
    (when (or (:line message) (:file message))
      (println (str "In"
                    (if-let [line (:line message)] (str " line " line) "")
                    (if-let [file (:file message)] (str " \"" file "\""))))))
  message)

(defn- test-report-unit [unit] unit)

(defn- test-report-namespace [namespace] namespace)

(defn- test-report-run [report]
  (letfn [(success? [unit]
            (not (some #(= (:type %) :failure) (:messages unit))))
          (report-failure- [unit]
            (println (format "  %s FAILED" (:name unit))))
          (report-namespace- [report]
            (let [failures (remove success? (:units report))
                  success (count (filter success? (:units report)))
                  failed (count failures)
                  time (reduce + (map #(or (:time %) 0) (:units report)))]
              (println (format "--%s-- %d/%d%s" (:name report)  success (+ success failed)
                               (if (zero? time) ""
                                   (format " in %ss" time))))
              (when (seq failures)
                (doall (map report-failure- failures))
                (println))
              {:success success :failed failed :time time}))]
     (let [results (doall (map report-namespace- (:namespaces report)))
           success (reduce + (map :success results))
           failed (reduce + (map :failed results))
           time (reduce + (map :time results))]
       (println)
       (println (format "%d tests of %d success%s" success (+ success failed)
                        (if (zero? time) "" (format " in %ss" time))))))
  report)

(defn test-listener []
  (reify Listener
    (on-message [this message] (test-report-message message))
    (on-unit [this unit] (test-report-unit unit))
    (on-namespace [this namespace] (test-report-namespace namespace))
    (on-report [this report] (test-report-run report))))

(def ^:dynamic *listener* (test-listener))

(defn merge-listeners [parent child]
  (reify Listener
    (on-message [this message]
      (on-message parent message) 
      (on-message child message))
    (on-unit [this unit]
      (on-unit parent unit) 
      (on-unit child unit))
    (on-namespace [this namespace]
      (on-namespace parent namespace)
      (on-namespace child namespace))
    (on-report [this report]
      (on-report parent report)
      (on-report child report))))
