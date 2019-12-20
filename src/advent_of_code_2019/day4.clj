(ns advent-of-code-2019.day4)

(def predicates [#(= (count %) 6)
                 #(<= 256310 (Long/valueOf %) 732736)
                 #(->> % (partition-by identity) (map count) (filter (partial = 2)) not-empty boolean)
                 #(= (apply str (sort %)) %)])

(defn valid-pass [pass]
  (->> predicates
       (map #(% pass))
       (reduce #(and %1 %2))))

(defn solve-1 []
  (->> (range 256310 (inc 732736))
       (map str)
       (map valid-pass)
       (filter true?)
       count))
