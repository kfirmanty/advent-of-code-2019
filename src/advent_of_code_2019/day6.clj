(ns advent-of-code-2019.day6)

;;{
;;:name :parent
;;
;;
;;}

(defn all-parents [tree from]
  (loop [from from parents 0]
    (if (get tree from)
      (recur (get tree from) (inc parents))
      parents)))

(defn parents-path [tree from]
  (loop [from from parents []]
    (let [parent (get tree from)]
      (if parent
        (recur parent (conj parents parent) )
        parents))))

(defn ->tree [rels]
  (reduce (fn [s [parent child]] (assoc s child parent)) {} rels))

(defn solve1 [input]
  (let [rels (->> (clojure.string/split input #"\n")
                  (map #(clojure.string/split % #"\)")))
        tree (->tree rels)]
    (->> (keys tree)
         (map (partial all-parents tree))
         (reduce +))))

(defn index-of [coll v]
  (loop [i 0 el (first coll) coll (rest coll)]
    (if (= el v)
      i
      (recur (inc i) (first coll) (rest coll)))))

(defn solve2 [input]
  (let [rels (->> (clojure.string/split input #"\n")
                  (map #(clojure.string/split % #"\)")))
        tree (->tree rels)
        from (parents-path tree "YOU")
        san (parents-path tree "SAN")
        rsan (reverse san)
        rfrom (reverse from)
        diverge-step (->> (map #(if (= %1 %2)
                                  %1
                                  nil) rfrom rsan)
                        (filter some?)
                        last)]
    (+ (- (count rsan) (index-of rsan diverge-step) 1)
       (- (count rfrom) (index-of rfrom diverge-step) 1))))
