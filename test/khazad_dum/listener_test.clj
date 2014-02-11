(ns khazad-dum.listener-test
  (:require [khazad-dum.core :refer :all]
            [khazad-dum.listener :as l]
            [clojure.string :refer [join]]))

;;
;; Default listener
;;

(deftest default-listener-success-message
  (let [message {:type :success :message "(+ 2 2) is 4"}]
    (?= (with-out-str (?= (l/report-message message) message))
        "")))

(deftest default-listener-fail-message
  (let [text "(+ 2 2) is 5. Expected 4 that is 4"
        message {:type :failure :message text}]
    (?= (with-out-str (?= (l/report-message message) message)) text)))

(deftest default-listener-fail-with-line-info
  (let [text "failure\n"
        message {:type :failure :message text :line 123 :file "test.clj"}]
    (?= (with-out-str (l/report-message message)) 
        (format "failure%nIn line 123 \"test.clj\"%n"))
    (?= (with-out-str (l/report-message (dissoc message :line)))
        (format "failure%nIn \"test.clj\"%n"))
    (?= (with-out-str (l/report-message (dissoc message :file)))
        (format "failure%nIn line 123%n"))))
  
(deftest default-listener-test-reporting
  (let [test {:type :test :name "my test" :messages [{:type :something} {} {:type :success}]}]
    (?= (with-out-str (?= (l/report-unit test) test)) "")))

(deftest default-listener-namespace-reporting
  (let [report {:type :ns :units [{:type :test :name "test1"} {:type :benchmark :name "bench"}]}]
    (?= (with-out-str (?= (l/report-namespace report) report)) "")))

(deftest default-listener-global-success-reporting
  (let [report {:type :report :namespaces [{:type :ns :name "some namespace" :units [{} {} {}]}
                                           {:type :ns :name "other namespace" :units [{} {}]}]}]
    (?= (with-out-str (?= (l/report-run report) report))
        (join (map println-str ["--some namespace-- 3/3"
                                "--other namespace-- 2/2"
                                ""
                                "5 tests of 5 success"])))))

(deftest default-listener-global-failures-reporting
  (let [ns1 {:name "ns1" :units [{} {}
                                 {:name "t1" :messages [{} {} {:name "f1" :type :failure} {}]} 
                                 {}
                                 {:name "t2" :messages [{} {:name "f2" :type :failure} {} {} {}
                                                        {:name "f3" :type :failure}]}]}
        ns2 {:name "success namespace" :units [{} {} {}]}]
    (?= (with-out-str (l/report-run {:namespaces [ns1 ns2]}))
        (join (map println-str ["--ns1-- 3/5"
                                "  t1 FAILED"
                                "  t2 FAILED"
                                ""
                                "--success namespace-- 3/3"
                                ""
                                "6 tests of 8 success"])))))

(deftest default-listener-global-report-with-timings
  (let [ns1 {:name "ns1" :units [{:time 1} {:time 0.5} {}]}
        ns2 {:name "ns2" :units [{:time 10} {} {} {}]}
        ns3 {:name "ns3" :units [{} {} {}]}]
    (?= (with-out-str (l/report-run {:namespaces [ns1 ns2 ns3]}))
        (join (map println-str ["--ns1-- 3/3 in 1.5s"
                                "--ns2-- 4/4 in 10s"
                                "--ns3-- 3/3"
                                ""
                                "10 tests of 10 success in 11.5s"])))))

;;
;; Identity listener
;;

(defmacro with-identity-listener [name & body]
  `(let [~name (l/identity-listener)]
     (binding [l/*listener* ~name]
       ~@body)))

(deftest identity-listener-message-reporting
  (with-identity-listener l
    (let [message {:message "hello" :type :failure}]
      (?= (with-out-str (?= (l/report-message message) message)) "")
      (?= (l/messages l) [message])
      (l/report-message (assoc message :time 123))
      (?= (l/messages l) [message (assoc message :time 123)]))))

(deftest identity-listener-unit-reporting
  (with-identity-listener l
    (let [unit1 {:name "u1"}
          unit2 {:name "u2"}]
      (?= (with-out-str (?= (l/report-unit unit1) unit1)) "")
      (?= (l/units l) [unit1])
      (l/report-unit unit2)
      (?= (l/units l) [unit1 unit2]))))

(deftest merging-identity-listener-messages-in-unit
  (with-identity-listener l
    (let [m1 {:name "m1"} m2 {:name "m2"}]
      (l/report-message m1)
      (l/report-message m2)
      (?= (l/report-unit {:name "u" :messages [{:name "m3"}]})
          {:name "u" :messages [m1 m2 {:name "m3"}]})
      (?= (l/messages l) []))))

(deftest identity-listener-namespace-reporting
  (with-identity-listener l
    (let [ns1 {:name "ns1"}
          ns2 {:name "ns2"}]
      (?= (with-out-str (?= (l/report-namespace ns1) ns1)) "")
      (?= (l/namespaces l) [ns1])
      (l/report-namespace ns2)
      (?= (l/namespaces l) [ns1 ns2]))))

(deftest identity-listener-units-in-namespace
  (with-identity-listener l
    (let [u1 {:name "u1"} u2 {:name "u2"}]
      (l/report-unit u1)
      (l/report-unit u2)
      (?= (l/report-namespace {:name "ns" :units [{:name "u3"}]})
          {:name "ns" :units [u1 u2 {:name "u3"}]})
      (?= (l/units l) []))))

(deftest identity-listener-run-reporting
  (with-identity-listener l
    (let [r {:namespaces {:ns1 :ns2}}]
      (?= (with-out-str (?= (l/report-run r) r)) "")
      (?= (l/reports l) [r]))))

(deftest identity-listener-reporting-run-with-namespaces
  (with-identity-listener l
    (let [ns1 {:name "ns1"} ns2 {:name "ns2"}]
      (l/report-namespace ns1)
      (l/report-namespace ns2)
      (?= (l/report-run {:name "run" :namespaces [{:name "ns3"}]})
          {:name "run" :namespaces [ns1 ns2 {:name "ns3"}]})
      (?= (l/namespaces l) []))))
      
(deftest merging-listeners
  (let [parent (reify l/Listener
                 (on-message [_ _] (print "parent message-") false)
                 (on-unit [_ _] (print "parent unit-") false)
                 (on-namespace [_ _] (print "parent namespace-") false)
                 (on-report [_ _] (print "parent run-") false))
        child (reify l/Listener
                 (on-message [_ _] (print "child message") true)
                 (on-unit [_ _] (print "child unit") true)
                 (on-namespace [_ _] (print "child namespace") true)
                 (on-report [_ _] (print "child run") true))]
    (binding [l/*listener* (l/merge-listeners parent child)]
      (?= (with-out-str (?true (l/report-message {}))) "parent message-child message")
      (?= (with-out-str (?true (l/report-unit {}))) "parent unit-child unit")
      (?= (with-out-str (?true (l/report-namespace {}))) "parent namespace-child namespace")
      (?= (with-out-str (?true (l/report-run {}))) "parent run-child run"))))

