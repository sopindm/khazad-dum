(ns khazad-dum-test
  (:require [khazad-dum :refer :all])
  (:use [clojure.string :only [join] :as str]))

(deftest tests-are-running
  (println "Tests are running"))

(defn check-tests []
  (run-test #'tests-are-running))

(deftest simple-test-output
  (let [test #(println "Hello from test")]
    (assert (.matches (with-out-str (run-test test))
                      (format "Hello from test%n%n1 tests of 1 success in .*%n")))))

(deftest running-named-test
  (assert (.matches (with-out-str (run-test #'tests-are-running))
                    (format "Tests are running%n%n1 tests of 1 success in .*%n"))))
  
(ns khazad-dum.test-ns
  (:require [khazad-dum :refer :all]))

(deftest dummy-test-1
  (println "Dummy 1"))

(deftest dummy-test-2
  (println "Dummy 2"))

(deftest dummy-test-3
  (?true false))

(in-ns 'khazad-dum-test)

(deftest run-tests-test
  (assert (.matches (with-out-str (run-tests 'khazad-dum.test-ns))
                    (join (map println-str
                               ["Dummy 1"
                                "Dummy 2"
                                "false is false. Expected true"
                                "In line .* \"khazad_dum_test\\.clj\""
                                ""
                                "2 tests of 3 success in .*"])))))

(deftest ?true-test
  (letfn [(test1 [] (?true true))
          (test2 [] (?true false))]
    (assert (.matches (with-out-str (run-test test1))
                      (format "%n1 tests of 1 success in .*%n")))
    (assert (.matches (with-out-str (run-test test2))
                      (join (map println-str
                                 ["false is false. Expected true"
                                  "In line .* \"khazad_dum_test\\.clj\""
                                  ""
                                  "0 tests of 1 success in .*"]))))))

(deftest ?false-test
  (letfn [(test1 [] (?false true))
          (test2 [] (?false false))]
    (?true (.matches (with-out-str (run-test test1))
                     (join (map println-str
                                ["true is true. Expected false"
                                 "In line .* \"khazad_dum_test.clj\"" 
                                 ""
                                 "0 tests of 1 success in .*"]))))
    (?true (.matches (with-out-str (run-test test2))
                     (format "%n1 tests of 1 success in .*%n")))))
    
(deftest ?=-test
  (letfn [(test1 [] (?= 2 2))
          (test2 [] (?= (+ 2 2) 5))]
    (?true (.matches (with-out-str (run-test test1))
                     (format "%n1 tests of 1 success in .*%n")))
    (?true (.matches (with-out-str (run-test test2))
                     (format (str "\\(\\+ 2 2\\) is%n"
                                  "4%n"
                                  "Expected 5 that is%n"
                                  "5%n"
                                  "In line .* \"khazad_dum_test.clj\"%n%n"
                                  "0 tests of 1 success in .*%n"))))))

(deftest ?matches-test
  (?true (.matches (with-out-str (run-test #(?matches "abc" "a.c")))
                   (format "%n1 tests of 1 success in .*%n")))
  (?true (.matches (with-out-str (run-test #(?matches "abc" "a.")))
                   (format (str "abc is%n"
                                "abc%n"
                                "Expected #\"a.\"%n"
                                "In line .* \"khazad_dum_test.clj\"%n%n"
                                "0 tests of 1 success in .*%n")))))

(deftest dying-with-exception
  (letfn [(test [] (throw (java.lang.Exception. "test")))]
    (?= (first (line-seq (java.io.BufferedReader. (java.io.StringReader. (with-out-str (run-test test))))))
        (print-str test "died with java.lang.Exception: test"))))

(deftest ?lines=-test
  (letfn [(test1 [] (?lines= (format "abc%ndef%nghi%n")
                             "abc"
                             "def"
                             "ghi"))
          (test2 [] (?lines= (format "abc%nfed%nghi%n")
                             "abc"
                             "def" 
                             "ghi"))]
    (?matches (with-out-str (run-test test1))
              (format "%n1 tests of 1 success in .*%n"))
    (?matches (with-out-str (run-test test2))
              (str (println-str "\\(format \"abc%nfed%nghi%n\"\\) is:")
                   (format (str "<<<...1...>>>%n"
                                "fed%n"
                                "<<<...1...>>>%n%n"
                                "Expected:%n"
                                "<<<...1...>>>%n"
                                "def%n"
                                "<<<...1...>>>%n%n"
                                "In line .* \"khazad_dum_test.clj\"%n%n"
                                "0 tests of 1 success in .*%n")))))
    (letfn [(test3 [] (?lines= (println-str "abc")
                             "abc"
                             "def"))
          (test4 [] (?lines= (println-str "def")
                             "abc"
                             "def"))]
    (?matches (with-out-str (run-test test3))
              (format (str "\\(println-str \"abc\"\\) is:%n"
                           "<<<...1...>>>%n%n"
                           "Expected:%n"
                           "<<<...1...>>>%n"
                           "def%n%n"
                           "In line .* \"khazad_dum_test.clj\"%n%n"
                           "0 tests of 1 success in .*%n")))
    (?matches (with-out-str (run-test test4))
              (format (str "\\(println-str \"def\"\\) is:%n"
                           "<<<...1...>>>%n%n"
                           "Expected:%n"
                           "abc%n"
                           "<<<...1...>>>%n%n"
                           "In line .* \"khazad_dum_test.clj\"%n%n"
                           "0 tests of 1 success in .*%n")))))

(defmacro ?test= [form & lines]
  `(let [~'test #(~@form)]
     (?matches (with-out-str (run-test ~'test))
               (join (map println-str [~@lines])))))

(deftest ?throws-test
  (?test= (?throws (throw (RuntimeException. "something"))
                   RuntimeException)
          ""
          "1 tests of 1 success in .*")
  (?test= (?throws "everything fine" Exception)
          "\"everything fine\" failed to die"
          "In line .* \"khazad_dum_test.clj\""
          ""
          "0 tests of 1 success in .*")
  (?test= (?throws (throw (RuntimeException. "something"))
                   UnsupportedOperationException)
          ".* died with java.lang.RuntimeException: something"
          "(.|\n)*"
          ""
          "0 tests of 1 success")
  (?test= (?throws (throw (RuntimeException. "something")) Exception
                   "some%s" 'thing)
          ""
          "1 tests of 1 success in .*")
  (?test= (?throws (throw (RuntimeException. "something")) Exception
                   "some other %s" 'thing)
          "Died with: something"
          "Expected: some other thing"
          "In line .* \"khazad_dum_test.clj\""
          ""
          "0 tests of 1 success in .*"))

;;interactive runs ('run all' *-> 'run failures' -> 'run all')
;(run-tests <ns1> <ns2> :interactive)

;?contains test

