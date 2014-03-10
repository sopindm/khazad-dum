(ns khazad-dum.listener-test
  (:require [khazad-dum :refer :all]
            [khazad-dum.listener :as l]
            [clojure.string :refer [join]]))

;;
;; Default listener
;;

(defmacro with-default-listener [name & body]
  `(let [~name (l/test-listener)]
     ~@body))

(deftest default-listener-success-message
  (with-default-listener listener
    (let [message {:type :success :message "(+ 2 2) is 4"}]
      (?= (with-out-str (?= (l/report-message listener message) message))
          ""))))

(deftest default-listener-fail-message
  (with-default-listener listener
    (let [text "(+ 2 2) is 5. Expected 4 that is 4"
          message {:type :failure :message text}]
      (?= (with-out-str (?= (l/report-message listener message) message)) (println-str text)))))

(deftest default-listener-fail-with-line-info
  (with-default-listener listener
    (let [text "failure"
          message {:type :failure :message text :line 123 :file "test.clj"}]
      (?= (with-out-str (l/report-message listener message)) 
          (format "failure%nIn line 123 \"test.clj\"%n"))
      (?= (with-out-str (l/report-message listener (dissoc message :line)))
          (format "failure%nIn \"test.clj\"%n"))
      (?= (with-out-str (l/report-message listener (dissoc message :file)))
          (format "failure%nIn line 123%n")))))

(deftest default-listener-with-errors
  (with-default-listener listener
    (let [exception (RuntimeException. "something")]
      (?= (-> (l/report-message listener {:type :error :exception exception :unit "some unit"})
              with-out-str java.io.StringReader. java.io.BufferedReader. 
              line-seq first)
          "some unit died with java.lang.RuntimeException: something"))))
          
(deftest default-listener-test-reporting
  (with-default-listener listener
    (let [test {:type :test :name "my test" :messages [{:type :something} {} {:type :success}]}]
      (?= (with-out-str (?= (l/report-unit listener test) test)) ""))))

(deftest default-listener-namespace-reporting
  (with-default-listener listener
    (let [report {:type :ns :units [{:type :test :name "test1"} {:type :benchmark :name "bench"}]}]
      (?= (with-out-str (?= (l/report-namespace listener report) report)) ""))))

(deftest default-listener-global-success-reporting
  (with-default-listener listener
    (let [report {:type :report :namespaces [{:type :ns :name "some namespace" :units [{} {} {}]}
                                             {:type :ns :name "other namespace" :units [{} {}]}]}]
      (?= (with-out-str (?= (l/report-run listener report) nil))
          (join (map println-str [""
                                  "--some namespace-- 3/3"
                                  "--other namespace-- 2/2"
                                  ""
                                  "5 tests of 5 success"]))))))

(deftest default-listener-global-failures-reporting
  (with-default-listener listener
    (let [ns1 {:name "ns1" :units [{} {}
                                   {:name "t1" :unit "t1" :messages [{} {} {:name "f1" :type :failure} {}]} 
                                   {}
                                   {:name "t2" :unit "t2" :messages [{} {:name "f2" :type :failure} {} {} {}
                                                          {:name "f3" :type :failure}]}]}
          ns2 {:name "success namespace" :units [{} {} {}]}]
      (?= (with-out-str (?= (l/report-run listener {:namespaces [ns1 ns2]})
                            ["t1" "t2"]))
          (join (map println-str [""
                                  "--ns1-- 3/5"
                                  "  t1 FAILED"
                                  "  t2 FAILED"
                                  ""
                                  "--success namespace-- 3/3"
                                  ""
                                  "6 tests of 8 success"]))))))

(deftest default-listener-global-report-with-timings
  (with-default-listener listener
    (let [ns1 {:name "ns1" :units [{:time 1} {:time 0.5} {}]}
          ns2 {:name "ns2" :units [{:time 10} {} {} {}]}
          ns3 {:name "ns3" :units [{} {} {}]}]
      (?= (with-out-str (l/report-run listener {:namespaces [ns1 ns2 ns3]}))
          (join (map println-str [""
                                  "--ns1-- 3/3 in 1.5s"
                                  "--ns2-- 4/4 in 10s"
                                  "--ns3-- 3/3"
                                  ""
                                  "10 tests of 10 success in 11.5s"]))))))

(deftest default-listener-priting-time
  (with-default-listener listener
    (let [ns1 {:name "ns1" :units [{:time 1.23456}]}
          ns2 {:name "ns2" :units [{:time 0.0123456789}]}
          ns3 {:name "ns3" :units [{:time 0.0000123456789}]}
          ns4 {:name "ns4" :units [{:time 61.23456}]}
          ns5 {:name "ns5" :units [{:time 3652.123456}]}]
      (?= (with-out-str (l/report-run listener {:namespaces [ns1 ns2 ns3 ns4 ns5]}))
          (join (map println-str [""
                                  "--ns1-- 1/1 in 1.2s"
                                  "--ns2-- 1/1 in 12.3ms"
                                  "--ns3-- 1/1 in 12.3mus"
                                  "--ns4-- 1/1 in 1m 1s"
                                  "--ns5-- 1/1 in 1h 52s"
                                  ""
                                  "5 tests of 5 success in 1h 1m 54s"]))))))

(deftest default-listener-has-no-namespace-info-for-one-namespace-run
  (with-default-listener listener
    (let [ns {:name "ns" :units [{} {} {} {:messages [{:type :failure}]}]}]
      (?lines= (with-out-str (l/report-run listener {:namespaces [ns]}))
               ""
               "3 tests of 4 success"))))

;;
;; Identity listener
;;

(defmacro with-identity-listener [name & body]
  `(let [~name (l/identity-listener)]
     ~@body))

(deftest identity-listener-message-reporting
  (with-identity-listener l
    (let [message {:message "hello" :type :failure}]
      (?= (with-out-str (?= (l/report-message l message) message)) "")
      (?= (l/messages l) [message])
      (l/report-message l (assoc message :time 123))
      (?= (l/messages l) [message (assoc message :time 123)]))))

(deftest identity-listener-unit-reporting
  (with-identity-listener l
    (let [unit1 {:name "u1"}
          unit2 {:name "u2"}]
      (?= (with-out-str (?= (l/report-unit l unit1) unit1)) "")
      (?= (l/units l) [unit1])
      (l/report-unit l unit2)
      (?= (l/units l) [unit1 unit2]))))

(deftest merging-identity-listener-messages-in-unit
  (with-identity-listener l
    (let [m1 {:name "m1"} m2 {:name "m2"}]
      (l/report-message l m1)
      (l/report-message l m2)
      (?= (l/report-unit l {:name "u" :messages [{:name "m3"}]})
          {:name "u" :messages [m1 m2 {:name "m3"}]})
      (?= (l/messages l) []))))

(deftest identity-listener-namespace-reporting
  (with-identity-listener l
    (let [ns1 {:name "ns1"}
          ns2 {:name "ns2"}]
      (?= (with-out-str (?= (l/report-namespace l ns1) ns1)) "")
      (?= (l/namespaces l) [ns1])
      (l/report-namespace l ns2)
      (?= (l/namespaces l) [ns1 ns2]))))

(deftest identity-listener-units-in-namespace
  (with-identity-listener l
    (let [u1 {:name "u1"} u2 {:name "u2"}]
      (l/report-unit l u1)
      (l/report-unit l u2)
      (?= (l/report-namespace l {:name "ns" :units [{:name "u3"}]})
          {:name "ns" :units [u1 u2 {:name "u3"}]})
      (?= (l/units l) []))))

(deftest identity-listener-run-reporting
  (with-identity-listener l
    (let [r {:namespaces {:ns1 :ns2}}]
      (?= (with-out-str (?= (l/report-run l r) r)) "")
      (?= (l/reports l) [r]))))

(deftest identity-listener-reporting-run-with-namespaces
  (with-identity-listener l
    (let [ns1 {:name "ns1"} ns2 {:name "ns2"}]
      (l/report-namespace l ns1)
      (l/report-namespace l ns2)
      (?= (l/report-run l {:name "run" :namespaces [{:name "ns3"}]})
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
    (let [l (l/merge-listeners parent child)]
      (?= (with-out-str (?true (l/report-message l {}))) "parent message-child message")
      (?= (with-out-str (?true (l/report-unit l {}))) "parent unit-child unit")
      (?= (with-out-str (?true (l/report-namespace l {}))) "parent namespace-child namespace")
      (?= (with-out-str (?true (l/report-run l {}))) "parent run-child run"))))

