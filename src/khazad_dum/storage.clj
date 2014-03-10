(ns khazad-dum.storage
  (:refer-clojure :exclude [filter])
  (:require [khazad-dum.listener :as l]
            [bultitude.core :as b]))

(defn units-storage []
  (atom {}))

(defonce ^:dynamic *units* (units-storage))

(defn namespaces [storage]
  (keys @storage))

(defn units
  ([storage] (mapcat second @storage))
  ([storage ns]
     (let [ns (if (symbol? ns)
                (or (find-ns ns)
                    (throw (IllegalArgumentException.
                            (format "Unknown namespace %s" ns))))
                ns)]
       (get @storage ns []))))

(defn conj-unit! [units name value]
  (let [ns (-> name meta :ns)]
    (swap! units (fn [m] (assoc m ns (conj (vec (remove #(= (:name %) name)
                                                        (get m ns)))
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

(defmacro defunit [name & body]
  `(do (def ~name)
       (conj-unit! *units* (var ~name) (fn [] ~@body))))

;;
;;Running units
;;

(defmacro ^:private with-time [& forms]
  (let [time-form (fn [] '(. System  nanoTime))]
    `(let [start# ~(time-form)]
       ~@forms
       (/ (double (- ~(time-form) start#)) 1e9))))

(defn run-unit- [{:keys [name value]}]
  (let [run-time (try (with-time (value))
                      (catch Exception e
                        (l/report-message {:type :error
                                           :exception e
                                           :unit name})
                        nil))
        report {:name (or (-> name meta :name) name)
                :unit name
                :type (-> name meta :unit-type)}]
    (l/report-unit (if run-time (assoc report :time run-time)
                       report))))

(defmacro ^:private with-run-context [& body]
  `(binding [l/*listener* (if l/*listener* (l/merge-listeners (l/identity-listener) l/*listener*)
                            (l/identity-listener))]
     ~@body))

(defn run-unit [u]
  (with-run-context
    (let [unit (if (var? u) (unit *units* u) {:name u :value u})]
      (run-unit- unit)
      (l/report-namespace {:name (-> u meta :ns)})
      (l/report-run {:type :report}))))

(defn- run-namespace- [namespace]
  (let [units (units *units* namespace)]
    (dorun (map run-unit- units))
    (l/report-namespace {:name namespace})))

(defn- subnamespaces [ns classpath?]
  (let [ns-sym (if (instance? clojure.lang.Namespace ns) (ns-name ns) ns)
        ns-str (name ns-sym)
        [ns-str test?] (if (.endsWith ns-str "-test")
                          [(apply str (drop-last 5 ns-str)) true]
                          [ns-str false])
        base-name (str ns-str \.)
        namespaces (if classpath? (b/namespaces-on-classpath)
                       (map ns-name (all-ns)))]
    (letfn [(ns-filter [namespace]
              (let [ns-name (name namespace)]
                (if test? (and (.endsWith ns-name "-test")
                               (.startsWith ns-name base-name))
                    (.startsWith ns-name base-name))))]
      (conj (clojure.core/filter ns-filter namespaces) ns-sym))))

(defn parse-namespace-form [form]
  (let [[name options] (if (sequential? form)
                         [(first form) (set (rest form))]
                         [form #{}])]
    (cond (contains? options :recursive)
          (map find-ns
               (mapcat #(parse-namespace-form (cons % (disj options
                                                            :recursive)))
                       (subnamespaces name (some #{:require :reload :reload-all}
                                                 options))))
          (contains? options :require) (do (require name) [name])
          (contains? options :reload) (do (dissoc-namespace! *units* (find-ns name))
                                          (require name :reload)
                                          [name])
          :else [name])))

(defn run-units [& namespaces]
  (with-run-context
    (dorun (map run-namespace- (mapcat parse-namespace-form namespaces)))
    (l/report-run {:type :report})))
    
  
