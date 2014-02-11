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

(defn- test-report-unit [unit]
  unit)

(defn- test-report-namespace [namespace]
  namespace)

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
                        (if (zero? time) "" (format " in %ss" time)))))))

(defn test-listener []
  (reify Listener
    (on-message [this message] (test-report-message message))
    (on-unit [this unit] (test-report-unit unit))
    (on-namespace [this namespace] (test-report-namespace namespace))
    (on-report [this report] (test-report-run report))))

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
  (swap! (:units listener) #(conj % unit))
  unit)

(defn identity-report-namespace [listener namespace]
  (swap! (:namespaces listener) #(conj % namespace))
  namespace)

(defn identity-report-run [listener report]
  (swap! (:reports listener) #(conj % report))
  report)

(defn identity-listener [] 
  (IdentityListener. (atom []) (atom []) (atom []) (atom [])))

(def ^:dynamic *listener* (test-listener))
