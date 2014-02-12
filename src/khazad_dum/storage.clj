(ns khazad-dum.storage
  (:refer-clojure :exclude [filter]))

(defn units-storage []
  (atom {}))

(defonce ^:dynamic *units* (units-storage))

(defn namespaces [storage]
  (keys @storage))

(defn units
  ([storage] (mapcat second @storage))
  ([storage ns] (get @storage ns [])))

(defn conj-unit! [units name value]
  (let [ns (-> name meta :ns)]
    (swap! units (fn [m] (assoc m ns (conj (get m ns [])
                                           {:name name :value value}))))))

(defn unit [storage v]
  (first (clojure.core/filter #(= (:name %) v) (units storage (-> v meta :ns)))))

(defn dissoc-namespace! [units namespace]
  (swap! units #(dissoc % namespace)))

(defn dissoc-unit! [units var]
  (letfn [(update-ns [m ns]
            (let [units (vec (remove #(= (:name %) var) (get m ns)))]
              (if (empty? units) (dissoc m ns)
                  (assoc m ns units))))]
    (let [ns (-> var meta :ns)]
      (swap! units #(update-ns % ns)))))

(defn filter [pred units]
  (seq (clojure.core/filter #(if (pred (-> % :name meta)) %) units)))

(defonce ^:dynamic *units* {})

(defmacro defunit [name body]
  `(do (def ~name)
       (conj-unit! *units* (var ~name) (fn [] ~@body))))

