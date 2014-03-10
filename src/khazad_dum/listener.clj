(ns khazad-dum.listener
  (:require [clojure.repl]))

(defprotocol Listener
  (on-message [this message])
  (on-unit [this unit])
  (on-namespace [this namespace])
  (on-report [this report]))

(declare ^:dynamic *listener*)

(defn report-message
  ([message] (on-message *listener* message))
  ([listener message] (on-message listener message)))

(defn report-unit
  ([unit] (on-unit *listener* unit))
  ([listener unit] (on-unit listener unit)))

(defn report-namespace
  ([namespace] (on-namespace *listener* namespace))
  ([listener namespace] (on-namespace listener namespace)))

(defn report-run
  ([report] (on-report *listener* report))
  ([listener report] (on-report listener report)))

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

(defmacro with-err-str [& body]
  `(let [s# (new java.io.StringWriter)]
     (binding [*err* s#]
       ~@body
       (str s#))))

(defn- test-report-message [message]
  (when (#{:failure} (:type message))
    (println (:message message))
    (when (or (:line message) (:file message))
      (println (str "In"
                    (if-let [line (:line message)] (str " line " line) "")
                    (if-let [file (:file message)] (str " \"" file "\""))))))
  (when (#{:error} (:type message))
    (let [e (:exception message)]
      (println (:unit message) "died with" (.toString e))
      (print (with-err-str (clojure.repl/pst e)))))
  message)

(defn- test-report-unit [unit] unit)

(defn- test-report-namespace [namespace] namespace)

(defn- time-string [time]
  (letfn [(big-unit [time name seconds]
            (let [val (int (/ time seconds))]
              (str val name " " (time-string (int (- time (* val seconds)))))))
          (unit [time name seconds]
            (let [bigval (int (* (/ time seconds) 10))
                  int (quot bigval 10)
                  float (first (drop-while #(and (not (zero? %)) (= (mod % 10) 0))
                                           (iterate #(/ % 10) (mod bigval 10))))]
              (str int (if (zero? float) "" (str "." float)) name)))]
    (cond (>= time 3600) (big-unit time "h" 3600)
          (>= time 60) (big-unit time "m" 60)
          (>= time 0.1) (unit time "s" 1)
          (>= time 0.0001) (unit time "ms" 0.001)
          (>= time 0.0000001) (unit time "mus" 0.000001)
          :else time)))

(defn- test-report-run [report]
  (letfn [(success? [unit]
            (not (some #(#{:failure :error} (:type %)) (:messages unit))))
          (report-failure- [unit]
            (println (format "  %s FAILED" (-> unit :name))))
          (report-namespace- [report verbose]
            (let [failures (remove success? (:units report))
                  success (count (filter success? (:units report)))
                  failed (count failures)
                  time (reduce + (map #(or (:time %) 0) (:units report)))]
              (when verbose
                (println (format "--%s-- %d/%d%s" (:name report)  success (+ success failed)
                                 (if (zero? time) ""
                                     (format " in %s" (time-string time)))))
                (when (seq failures)
                  (doall (map report-failure- failures))
                  (println)))
              {:success success :failed failed :time time :failures failures}))] 
    (println)
    (let [results (doall (map #(report-namespace- % (> (count (:namespaces report)) 1))
                              (:namespaces report)))
          success (reduce + (map :success results))
          failed (reduce + (map :failed results))
          time (reduce + (map :time results))]
      (when (> (count (:namespaces report)) 1) (println))
      (println (format "%d tests of %d success%s" success (+ success failed)
                       (if (zero? time) "" (format " in %s" (time-string time)))))
      (seq (map :unit (mapcat :failures results))))))

(defn test-listener []
  (reify Listener
    (on-message [this message] (test-report-message message))
    (on-unit [this unit] (test-report-unit unit))
    (on-namespace [this namespace] (test-report-namespace namespace))
    (on-report [this report] (test-report-run report))))

(def ^:dynamic *listener* nil)

(defn merge-listeners [parent child]
  (reify Listener
    (on-message [this message]
      (on-message child (on-message parent message)))
    (on-unit [this unit]
      (on-unit child (on-unit parent unit)))
    (on-namespace [this namespace]
      (on-namespace child (on-namespace parent namespace)))
    (on-report [this report]
      (on-report child (on-report parent report)))))

