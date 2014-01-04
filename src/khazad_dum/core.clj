(ns khazad-dum.core
  (:use [clojure.string :only [join]]))

;;
;; Tests storing
;;

(defonce ^:dynamic *tests* (atom {}))

(defn- update-test [tests name body]
  (conj (vec (remove #(= (first %) name) tests))
        [name body]))

(defn- update-ns-test [tests ns name body]
  (update-in tests [ns] #(update-test % name body)))

(defn add-test [name body]
  (let [ns (:ns (meta name))]
    (swap! *tests* #(update-ns-test % ns name body))))

(defn get-tests [ns]
  (get @*tests* ns))

(defn get-test [name]
  (let [ns (:ns (meta name))]
    (if-let [[[_ test]] (filter #(= (first %) name) (get-tests ns))]
      test
      (throw (java.lang.IllegalArgumentException. 
              (print-str "Wrong test name" name))))))

(defmacro deftest [name & body]
  (let [nname (intern *ns* name)]
    `(#'khazad-dum.core/add-test '~nname (fn [] ~@body))))

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
           (report-failure (print-str name "died with" (.toString e)))
           false))))

(defn- do-run-tests [tests]
  (let [results (doall (map (fn [[key val]] [key (do-run-test key val)]) tests))
        failed (remove (fn [[_ result]] (empty? result)) results)]
    (doall (map #(doall (map println (second %))) failed))
    (println)
    (doall (map #(println (if (var? (first %))
                            (let [{ns :ns name :name} (meta (first %))]
                              (str ns "/" name))
                            (first %)) "failed")
                failed))
    (println (- (count results) (count failed)) "tests of" 
             (count results) "success")))

(defn run-test [name]
  (if-let [form (if (var? name) (get-test name) name)]
    (do-run-tests [[name form]])
    (throw (IllegalArgumentException. (format "Wrong test %s" name)))))

(defn run-tests [& namespaces]
  (do-run-tests (mapcat (comp get-tests find-ns) namespaces)))

;;
;; Test predicates
;;

(defmacro ?false [expr]
  `(let [v# ~expr]
     (if (false? v#)
       (report-success)
       (report-failure (format "%s is %s. Expected false" '~expr v#)))))

(defmacro ?true [expr]
  `(let [v# ~expr]
     (if (true? v#)
       (report-success)
       (report-failure (format "%s is %s. Expected true" '~expr v#)))))

(defmacro ?= [val1 val2]
  `(let [v1# ~val1, v2# ~val2]
     (if (= v1# v2#)
       (report-success)
       (report-failure (format "%s is%n%s%nExpected %s that is%n%s"
                               '~val1 v1# '~val2 v2#)))))

(defn- lines-to-string [lines]
  (join (map println-str lines)))

(defn- lines-diff [str1 str2]
  (let [lines1 (clojure.string/split-lines str1)
        lines2 (clojure.string/split-lines str2)
        trim (fn [l1 l2] (count (take-while (fn [[f s]] (= f s)) 
                                            (map vector l1 l2))))
        begin (trim lines1 lines2)
        end (trim (reverse (drop begin lines1)) 
                  (reverse (drop begin lines2)))]
    (str (if (pos? begin) (format "<<<...%s...>>>%n" begin) "")
         (join (map println-str (drop-last end (drop begin lines1))))
         (if (pos? end) (format "<<<...%s...>>>%n" end) ""))))

(defmacro ?lines= [expr & lines]
  `(let [v1# ~expr
         v2# (#'lines-to-string (list ~@lines))]
     (if (= v1# v2#)
       (report-success)
       (report-failure (format "%s is:%n%s%nExpected:%n%s" 
                               '~expr 
                               (#'lines-diff v1# v2#)
                               (#'lines-diff v2# v1#))))))

(defmacro ?throws 
  ([form exception] `(?throws ~form ~exception nil))
  ([form exception message & args]
     (let [exc-sym (gensym "exc")]
       `(try ~form
             (report-failure (print-str (pr-str '~form) "failed to die"))
             (catch ~exception ~exc-sym
               ~(if message
                  `(let [got# (.getMessage ~exc-sym)
                        expected# (format ~message ~@args)]
                     (if-not (= got# expected#)
                       (report-failure (format "Died with: %s%nExpected: %s"
                                               got#
                                               expected#))
                       (report-success)))
                  `(report-success)))))))

