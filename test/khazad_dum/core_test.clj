(ns khazad-dum.core-test
  (:require [khazad-dum.core :refer :all]))

(deftest tests-are-running
  (println "Tests are running"))

(defn check-tests []
  (run-test #'tests-are-running))

(deftest simple-test-output
  (let [test #(println "Hello from test")]
    (assert (= (with-out-str (run-test test))
               (format "Hello from test%n%n1 tests of 1 success%n" 
                       (print-str test))))))

;running named test

(ns khazad-dum.test-ns
  (:require [khazad-dum.core :refer :all]))

(deftest dummy-test-1
  (println "Dummy 1"))

(deftest dummy-test-2
  (println "Dummy 2"))

(in-ns 'khazad-dum.core-test)

(deftest run-tests-test
  (assert (= (with-out-str (run-tests 'khazad-dum.test-ns))
             (format "Dummy 1%nDummy 2%n%n2 tests of 2 success%n"))))

;?false, ?true form
;?= form
;(?lines= form & lines)
;(run-tests namespace & more-namespaces)
;?throws
;test dies with exception
             

