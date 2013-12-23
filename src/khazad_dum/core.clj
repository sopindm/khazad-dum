(ns khazad-dum.core)

(defn ^:dynamic report-success []
  true)

(def ^:dynamic *test-results* (atom []))

(defn ^:dynamic report-failure [message]
  (swap! *test-results* #(conj % message))
  true)

(defmacro ?false [expr]
  `(let [v# ~expr]
     (if (false? v#)
       (report-success)
       (report-failure (format "%s is %s. Expected false." '~expr v#)))))

(defmacro ?= [val1 val2]
  `(let [v1# ~val1, v2# ~val2]
     (if (= v1# v2#)
       (report-success)
       (report-failure (format "%s is %s.%nExpected %s that is %s"
                               '~val1 v1# '~val2 v2#)))))

(defonce ^:dynamic *tests* (atom {}))

(defn add-test [name body]
  (let [ns (-> name meta :ns)
        sym (-> name meta :name)]
    (if-let [ns-tests (get @*tests* ns)]
      (swap! *tests* (fn [m] (conj m [ns (conj (vec (remove #(= (first %) sym) 
                                                            ns-tests))
                                               [sym body])])))
      (swap! *tests* #(conj % [ns [[sym body]]])))))

(defn get-test [name]
  (if-let [[[_ test]] (filter #(= (first %) (-> name meta :name)) 
                              (get @*tests* (-> name meta :ns)))]
    (test)
    (throw (java.lang.IllegalArgumentException. 
            (print-str "Wrong test name" name)))))

(defn get-tests [ns]
  (if-let [ns (find-ns ns)]
    (get @*tests* ns)
    (throw (java.lang.IllegalArgumentException. 
            (print-str "Wrong namespace name" ns)))))

(defmacro deftest [name & body]
  (let [internal (intern *ns* name)]
    `(add-test ~internal (fn [] ~@body))))

(defmacro with-test-environment [& body]
  `(binding [*test-results* (atom [])]
     ~@body
     @*test-results*))

(defn- do-run-test [name form]
  (with-test-environment 
    (try (form)
         (catch Exception e
           (report-failure (println-str name "died with" (.getMessage e)))
           false))))

(defn run-test [test]
  (let [true-test (if (var? test) (get-test test) test)]
    (when (nil? true-test) (throw (java.lang.IllegalArgumentException.
                                   (format "Wrong test %s" test))))
    (let [result (do-run-test test true-test)]
      (println)
      (if (empty? result)
        (println "Test" test "OK.")
        (println "Test" test "failed."))
      (empty? result))))

(defn run-tests [ns]
  (let [tests (get-tests ns)
        results (doall (map (fn [[key val]] [key (do-run-test key val)])
                            tests))
        failed (filter (fn [[_ result]] (not (empty? result))) results)]
    (println)
    (println (count results) "tests of" (count results) "success")))

