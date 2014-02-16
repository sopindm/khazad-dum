(ns khazad-dum
  (:require [khazad-dum.storage :as s]
            [khazad-dum.listener :as l]
            [clojure.string :refer [join]]))

(defmacro deftest [name & body]
  `(s/defunit ~(vary-meta name #(assoc % :unit-type :test :test true))
     ~@body))

;;
;; Reports and results
;;

(def ^:dynamic *test-results* (atom []))

(defn- file-and-line []
  (let [stacktrace (.getStackTrace (Throwable.))]
    (let [^StackTraceElement s (nth stacktrace 2)]
      {:file (.getFileName s) :line (.getLineNumber s)})))

(defn report-success [message]
  (l/report-message (merge {:type :success :message message}
                           (file-and-line))))

(defn report-failure [message]
  (l/report-message (merge {:type :failure :message message}
                           (file-and-line))))

;;
;; Running tests
;;

(defn run-test [name]
  (binding [l/*listener* (l/test-listener)]
    (s/run-unit name)))

(def ^:dynamic *listen-with* l/test-listener)

(defn run-tests [& namespaces]
  (binding [l/*listener* (*listen-with*)]
    (apply s/run-units namespaces)))

;;
;; Test predicates
;;

(defmacro ?false [expr]
  `(let [v# ~expr]
     (if (false? v#)
       (report-success (format "%s is %s" '~expr v#))
       (report-failure (format "%s is %s. Expected false" '~expr v#)))))

(defmacro ?true [expr]
  `(let [v# ~expr]
     (if (true? v#)
       (report-success (format "%s is %s" '~expr v#))
       (report-failure (format "%s is %s. Expected true" '~expr v#)))))

(defmacro ?= [val1 val2]
  `(let [v1# ~val1, v2# ~val2]
     (if (= v1# v2#)
       (report-success (format "%s and %s is%n%s" '~val1 '~val2 v1#))
       (report-failure (format "%s is%n%s%nExpected %s that is%n%s"
                               '~val1 v1# '~val2 v2#)))))

(defmacro ?matches [expr regexpr]
  `(let [v# ~expr]
     (if (.matches v# ~regexpr)
       (report-success (format "%s matches %s" '~expr ~regexpr))
       (report-failure (format "%s is%n%s%nExpected #\"%s\""
                               '~expr v# ~regexpr)))))

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
       (report-success (format "%s is %s" '~expr v1#))
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
                       (report-success "OK")))
                  `(report-success "OK")))))))


