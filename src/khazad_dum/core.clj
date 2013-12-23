(ns khazad-dum.core)

;;
;; Tests storing
;;

(defonce ^:dynamic *tests* (atom {}))

(defn- update-tests [tests name body]
  (conj (vec (remove #(= (first %) name) tests))
        [name body]))

(defn- update-ns-tests [tests ns name body]
  (update-in tests [ns] #(update-tests % name body)))

(defn add-test [name body]
  (let [{ns :ns sym :name} (meta name)]
    (swap! *tests* #(update-ns-tests % ns sym body))))

(defn get-tests [ns]
  (get @*tests* (find-ns ns)))

(defn get-test [name]
  (let [{sym :name ns :ns} (meta name)]
    (if-let [[[_ test]] (filter #(= (first %) sym) (get-tests ns))]
      (test)
      (throw (java.lang.IllegalArgumentException. 
              (print-str "Wrong test name" name))))))

(defmacro deftest [name & body]
  (let [internal (intern *ns* name)]
    `(add-test ~internal (fn [] ~@body))))

;;
;; Reports and results
;;

(defn ^:dynamic report-success []
  true)

(def ^:dynamic *test-results* (atom []))

(defn ^:dynamic report-failure [message]
  (swap! *test-results* #(conj % message))
  true)

(defmacro with-test-environment [& body]
  `(binding [*test-results* (atom [])]
     ~@body
     @*test-results*))

;;
;; Running tests
;;

(defn- do-run-test [name form]
  (with-test-environment 
    (try (form)
         (catch Exception e
           (report-failure (println-str name "died with" (.getMessage e)))
           false))))

(defn- do-run-tests [tests]
  (let [results (doall (map (fn [[key val]] [key (do-run-test key val)]) tests))
        failed (remove (fn [[_ result]] (empty? result)) results)]
    (println)
    (println (count results) "tests of" (count results) "success")))

(defn run-test [name]
  (if-let [form (if (var? name) (get-test name) name)]
    (do-run-tests [[name form]])
    (throw (java.lang.IllegalArgumentException. (format "Wrong test %s" name)))))

(defn run-tests [ns]
  (do-run-tests (get-tests ns)))

;;
;; Test predicates
;;

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

