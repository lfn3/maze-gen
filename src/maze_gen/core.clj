(ns maze-gen.core
  (:gen-class)
  (:require [clojure.spec.alpha :as s]
            [clojure.test.check.generators :as gen]
            [clojure.spec.test.alpha :as s.test]
            [rhizome.viz]))

(s/def ::start int?)
(s/def ::end int?)
(s/def ::connected-nodes (s/with-gen (s/and (s/coll-of int?)
                                            set?
                                            (fn [s] (= 2 (count s))))
                                     #(gen/fmap set (s/gen (s/tuple int? int?)))))
(s/def ::connected (s/with-gen (s/and (s/coll-of ::connected-nodes)
                                      set?)
                               #(gen/fmap set (s/gen (s/coll-of ::connected-nodes)))))

(s/def ::maze (s/keys :req [::start ::end ::connected]))

(def kernel {::start 0 ::end 0 ::connected #{}})

(defn should-have-more-or-same-nodes-fspec [{:keys [args ret]}]
  (let [maze (:maze args)]
    (<= (count (::connected maze)) (count (::connected ret)))))

(defn get-max-node [maze]
  (->> maze
       ::connected
       (map (partial reduce max 0))
       (reduce max 0)))

(defn grow [maze]
  (let [old-end (::end maze)
        new-end (inc (get-max-node maze))]
    (-> maze
        (update ::end inc)
        (update ::connected conj #{old-end new-end}))))

(s/fdef grow
  :args (s/cat :maze ::maze)
  :ret ::maze
  :fn should-have-more-or-same-nodes-fspec)

(defn add-leaf [maze]
  (let [max-node (get-max-node maze)
        connect-to (rand-int max-node)]
    (if (= 0 max-node)
      (update maze ::connected conj #{max-node 1})
      (update maze ::connected conj #{max-node connect-to}))))

(s/fdef add-leaf
  :args (s/cat :maze ::maze)
  :ret ::maze
  :fn should-have-more-or-same-nodes-fspec)

(defn split-edge [maze]
  (-> maze
      (update ::connected conj #{1 2})))

(s/fdef split-edge
  :args (s/cat :maze ::maze)
  :ret ::maze
  :fn should-have-more-or-same-nodes-fspec)

(def operations [grow add-leaf])

(defn generate-maze [complexity]
  (loop [maze kernel
         counter complexity]
    (let [operation (get operations (rand-int (count operations)))]
      (if (< 0 counter)
        (recur (operation maze)
               (dec counter))
        maze))))

(s/fdef generate-maze
  :args (s/cat :complexity int?)
  :ret ::maze)

(defn viz [maze]
  (rhizome.viz/view-graph (range (get-max-node maze))
                          (->> maze
                               ::connected
                               ((juxt (partial group-by first)
                                      (partial group-by last)))
                               (reduce merge)
                               (map (fn [[k nodes]] [k (->> nodes
                                                            (mapcat (partial filter (partial not= k))))]))
                               (into {}))
                          :node->descriptor (fn [n] {:label      (str n)
                                                     :fillcolor (cond (= (::start maze) n) "green"
                                                                      (= (::end maze) n) "red"
                                                                      :default "blue")})
                          :options {:node {:style "filled"}}
                          :directed? false))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
