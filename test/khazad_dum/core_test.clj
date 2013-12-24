(ns khazad-dum.core-test
  (:require [khazad-dum.core :refer :all])
  (:use [clojure.string :only [join] :as str]))

(deftest tests-are-running
  (println "Tests are running"))

(defn check-tests []
  (run-test #'tests-are-running))

(deftest simple-test-output
  (let [test #(println "Hello from test")]
    (assert (= (with-out-str (run-test test))
               (format "Hello from test%n%n1 tests of 1 success%n" )))))

(deftest running-named-test
  (assert (= (with-out-str (run-test #'tests-are-running))
             (format "Tests are running%n%n1 tests of 1 success%n"))))
  
(ns khazad-dum.test-ns
  (:require [khazad-dum.core :refer :all]))

(deftest dummy-test-1
  (println "Dummy 1"))

(deftest dummy-test-2
  (println "Dummy 2"))

(deftest dummy-test-3
  (?true false))

(in-ns 'khazad-dum.core-test)

(deftest run-tests-test
  (assert (= (with-out-str (run-tests 'khazad-dum.test-ns))
             (format (str "Dummy 1%n"
                          "Dummy 2%n"
                          "false is false. Expected true%n"
                          "%n"
                          "khazad-dum.test-ns/dummy-test-3 failed%n"
                          "2 tests of 3 success%n")))))

(deftest ?true-test
  (letfn [(test1 [] (?true true))
          (test2 [] (?true false))]
    (assert (= (with-out-str (run-test test1))
               (format "%n1 tests of 1 success%n")))
    (assert (= (with-out-str (run-test test2))
               (format "false is false. Expected true%n%n%s failed%n0 tests of 1 success%n"
                       (print-str test2))))))

;?false

(deftest ?=-test
  (letfn [(test1 [] (?= 2 2))
          (test2 [] (?= (+ 2 2) 5))]
    (?true (= (with-out-str (run-test test1))
           (format "%n1 tests of 1 success%n")))
    (?true (= (with-out-str (run-test test2))
              (format (str "(+ 2 2) is%n"
                           "4%n"
                           "Expected 5 that is%n"
                           "5%n%n"
                           (println-str test2 "failed")
                           "0 tests of 1 success%n"))))))

(deftest ?lines=-test
  (letfn [(test1 [] (?lines= (format "abc%ndef%nghi%n")
                             "abc"
                             "def"
                             "ghi"))
          (test2 [] (?lines= (format "abc%nfed%nghi%n")
                             "abc"
                             "def" 
                             "ghi"))]
    (?= (with-out-str (run-test test1))
        (format "%n1 tests of 1 success%n"))
    (?= (with-out-str (run-test test2))
        (str (println-str "(format \"abc%nfed%nghi%n\") is:")
             (format (str "<<<...1...>>>%n"
                          "fed%n"
                          "<<<...1...>>>%n%n"
                          "Expected:%n"
                          "<<<...1...>>>%n"
                          "def%n"
                          "<<<...1...>>>%n%n%n"
                          (println-str test2 "failed")
                          "0 tests of 1 success%n")))))
  (letfn [(test3 [] (?lines= (println-str "abc")
                             "abc"
                             "def"))
          (test4 [] (?lines= (println-str "def")
                             "abc"
                             "def"))]
    (?= (with-out-str (run-test test3))
        (format (str "(println-str \"abc\") is:%n"
                     "<<<...1...>>>%n%n"
                     "Expected:%n"
                     "<<<...1...>>>%n"
                     "def%n%n%n"
                     (println-str test3 "failed")
                     "0 tests of 1 success%n")))
    (?= (with-out-str (run-test test4))
        (format (str "(println-str \"def\") is:%n"
                     "<<<...1...>>>%n%n"
                     "Expected:%n"
                     "abc%n"
                     "<<<...1...>>>%n%n%n"
                     (println-str test4 "failed")
                     "0 tests of 1 success%n")))))

;(run-tests namespace & more-namespaces)
;?throws
;test dies with exception
             

