(ns khazad-dum.storage-test
  (:require [khazad-dum.core :refer :all]
            [khazad-dum.storage :as s]
            [khazad-dum.listener :as l])
  (:use [clojure.core.match :only [match]]))

(deftest empty-storage
  (let [units (s/units-storage)]
    (?true (empty? (s/namespaces units)))
    (?= (s/units units) [])))

(deftest adding-unit-to-storage
  (let [units (s/units-storage)
        my-ns (find-ns 'khazad-dum.storage-test)
        unit-var (intern my-ns 'test-unit)
        unit-value "a test value"
        unit {:name unit-var :value unit-value}]
    (s/conj-unit! units unit-var unit-value)
    (?= (s/namespaces units) [my-ns])
    (?= (s/units units) [unit])
    (?= (s/units units my-ns) [unit])
    (?= (s/units units 'khazad-dum.storage-test) [unit])))

(deftest units-for-wrong-namespace
  (let [units (s/units-storage)
        sym (gensym)]
    (?throws (s/units units sym) IllegalArgumentException
             (format "Unknown namespace %s" sym))))

(declare test-var1 test-var2)

(deftest adding-several-units-to-storage
  (let [units (s/units-storage)]
    (s/conj-unit! units #'test-var1 "test value")
    (s/conj-unit! units #'test-var2 "other test value")
    (let [my-ns (find-ns 'khazad-dum.storage-test)
          my-units [{:name #'test-var1 :value "test value"}
                    {:name #'test-var2 :value "other test value"}]]
      (?= (s/namespaces units) [my-ns])
      (?= (s/units units) my-units)
      (?= (s/units units my-ns) my-units))))

(deftest replacing-unit
  (let [units (s/units-storage)]
    (s/conj-unit! units #'test-var1 "test value")
    (s/conj-unit! units #'test-var1 "other test value")
    (?= (s/units units) [{:name #'test-var1 :value "other test value"}])))

(deftest adding-units-from-several-namespaces
  (let [ns1 (create-ns 'test-ns1)
        ns2 (create-ns 'test-ns2)
        var1 (intern ns1 'var1)
        var2 (intern ns1 'var2)
        var3 (intern ns2 'var1)
        units (s/units-storage)]
    (s/conj-unit! units var1 "t1")
    (s/conj-unit! units var2 "t2")
    (s/conj-unit! units var3 "t3")
    (?= (set (s/namespaces units)) #{ns1 ns2})
    (?= (set (s/units units)) #{{:name var1 :value "t1"}
                                {:name var2 :value "t2"}
                                {:name var3 :value "t3"}})
    (?= (s/units units ns1) [{:name var1 :value "t1"}
                             {:name var2 :value "t2"}])
    (?= (s/units units ns2) [{:name var3 :value "t3"}])))

(deftest removing-namespace-from-storage
  (let [ns1 (create-ns 'test-ns1)
        ns2 (create-ns 'test-ns2)
        var1 (intern ns1 'var1)
        var2 (intern ns2 'var2)
        units (s/units-storage)]
    (s/conj-unit! units var1 "v1")
    (s/conj-unit! units var2 "v2")
    (s/dissoc-namespace! units ns1)
    (?= (s/namespaces units) [ns2])
    (?= (s/units units) [{:name var2 :value "v2"}])))

(deftest getting-unit
  (let [ns (create-ns 'test-ns)
        var1 (intern ns 'var)
        units (s/units-storage)]
    (s/conj-unit! units var1 "vvv")
    (?= (s/unit units var1) {:name var1 :value "vvv"})
    (?= (s/unit units #'getting-unit) nil)))
    
(deftest removing-units
  (let [ns1 (create-ns 'test-ns1)
        ns2 (create-ns 'test-ns2)
        var1 (intern ns1 'var1)
        var2 (intern ns1 'var2)
        var3 (intern ns2 'var1)
        units (s/units-storage)]
    (s/conj-unit! units var1 "v1")
    (s/conj-unit! units var2 "v2")
    (s/conj-unit! units var3 "v3")
    (s/dissoc-unit! units var1)
    (?= (set (s/namespaces units)) #{ns1 ns2})
    (?= (set (s/units units)) #{{:name var2 :value "v2"}
                                {:name var3 :value "v3"}})
    (?= (s/units units ns1) [{:name var2 :value "v2"}])
    (s/conj-unit! units var1 "v4")
    (?= (s/units units ns1) [{:name var2 :value "v2"} {:name var1 :value "v4"}])
    (s/dissoc-unit! units var3)
    (?= (s/namespaces units) [ns1])
    (?= (seq (s/units units)) [{:name var2 :value "v2"} {:name var1 :value "v4"}])))

(deftest filtering-units
  (let [ns (create-ns 'test-ns)
        var1 (intern ns (with-meta 'var1 {:test true :always true}))
        var2 (intern ns (with-meta 'var2 {:test true}))
        var3 (intern ns (with-meta 'var3 {:always true}))
        unit1 {:name var1 :value "v1"}
        unit2 {:name var2 :value "v2"}
        unit3 {:name var3 :value "v3"}
        units (s/units-storage)]
    (s/conj-unit! units var1 "v1")
    (s/conj-unit! units var2 "v2")
    (s/conj-unit! units var3 "v3")
    (?= (s/filter :test (s/units units)) [unit1 unit2])
    (?= (s/filter :always (s/units units)) [unit1 unit3])
    (?= (s/filter (complement :test) (s/units units)) [unit3])))

(defmacro with-identity-listener [& body]
  `(binding [l/*listen-with* nil]
     ~@body))

(deftest running-units
  (let [test ^{:unit-type :test-unit} #(l/report-message {:name "test"})
        result (with-identity-listener
                 (s/run-unit test))]
    (?true (match result
                  {:type report
                   :namespaces [{:name nil :units 
                                 [{:name test :type :test-unit
                                   :messages [{:name "test"}] 
                                   :time _}]}]}
                  true
                  :else false))))

(deftest running-unit-by-name
  (let [units (s/units-storage)
        ns (create-ns 'test-ns)
        var (intern ns 'var)]
    (s/conj-unit! units var #(l/report-message {:name "test"}))
    (binding [s/*units* units]
      (?true (match (with-identity-listener (s/run-unit var))
                    {:type report
                     :namespaces [{:name ns :units
                                   [{:name var :type nil
                                     :messages [{:name "test"}]
                                     :time _}]}]} true
                    :else false)))))
                                    
(deftest unit-excepiton-failure
  (let [exception (RuntimeException. "something")
        unit #(throw exception)
        result (with-identity-listener (s/run-unit unit))]
    (?= (get-in result [:namespaces 0 :units 0 :messages 0])
        {:type :error :exception exception :unit unit})))

(deftest running-namespace-units
  (let [namespace (create-ns 'test-ns)
        var1 (intern namespace 'var1)
        var2 (intern namespace 'var2)
        var3 (intern namespace 'var3)]
    (binding [s/*units* (s/units-storage)]
      (s/conj-unit! s/*units* var1 #(l/report-message {:name "m1"}))
      (s/conj-unit! s/*units* var2 #(l/report-message {:name "m2"}))
      (s/conj-unit! s/*units* var3 #(l/report-message {:name "m3"}))
      (?true (match (with-identity-listener (s/run-units namespace))
                    {:type :report
                     :namespaces [{:name namespace :units
                                   [{:name var1 :type nil :time _
                                     :messages [{:name "m1"}]}
                                    {:name var2 :type nil :time _
                                     :messages [{:name "m2"}]}
                                    {:name var3 :type nil :time _
                                     :messages [{:name "m3"}]}]}]} true
                    :else false)))))

(deftest running-several-namespaces
  (let [ns1 (create-ns 'ns1)
        ns2 (create-ns 'ns2)
        var1 (intern ns1 'var1)
        var2 (intern ns2 'var2)]
    (binding [s/*units* (s/units-storage)]
      (s/conj-unit! s/*units* var1 #(l/report-message {:name "m1"}))
      (s/conj-unit! s/*units* var2 #(l/report-message {:name "m2"}))
      (?true (match (with-identity-listener (s/run-units ns1 ns2))
                    {:type :report
                     :namespaces [{:name ns1 :units
                                   [{:name var1 :type nil :time _
                                     :messages [{:name "m1"}]}]}
                                  {:name ns2 :units
                                   [{:name var2 :type nil :time _
                                     :messages [{:name "m2"}]}]}]} true
                    :else false)))))

(deftest running-namespaces-recursive
  (let [ns1 (create-ns 'ns)
        ns2 (create-ns 'ns.subns)
        ns3 (create-ns 'nn-test)
        ns4 (create-ns 'nn.subnn-test)
        ns5 (create-ns 'nn.subnn.subsub-test)]
    (binding [s/*units* (s/units-storage)]
      (dorun (map #(s/conj-unit! s/*units* (intern % 'var)
                                 {:name "name" :value (constantly nil)})
                  [ns1 ns2 ns3 ns4 ns4]))
      (?= (set (map :name (:namespaces (with-identity-listener
                                         (s/run-units [ns1 :recursive])))))
          #{ns1 ns2})
      (?= (set (map :name (:namespaces (with-identity-listener
                                         (s/run-units [ns3 :recursive])))))
          #{ns3 ns4 ns5}))))

