(ns khazad-dum.storage-test
  (:require [khazad-dum.core :refer :all]
            [khazad-dum.storage :as s]))

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
