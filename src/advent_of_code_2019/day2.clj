(ns advent-of-code-2019.day2)

(defn execute [program position]
  (let [[opcode from1 from2 dest] (drop position program)
        program (into [] program)
        exec-fn (fn [op]
                  (assoc program dest
                         (op (nth program from1) (nth program from2))))]
    (condp = opcode
      1 (exec-fn +)
      2 (exec-fn *))))

(def input "1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,1,10,19,1,6,19,23,1,13,23,27,1,6,27,31,1,31,10,35,1,35,6,39,1,39,13,43,2,10,43,47,1,47,6,51,2,6,51,55,1,5,55,59,2,13,59,63,2,63,9,67,1,5,67,71,2,13,71,75,1,75,5,79,1,10,79,83,2,6,83,87,2,13,87,91,1,9,91,95,1,9,95,99,2,99,9,103,1,5,103,107,2,9,107,111,1,5,111,115,1,115,2,119,1,9,119,0,99,2,0,14,0")

(defn solve-1 [i]
  (let [program (->> (clojure.string/split i #",")
                     (map #(Integer/parseInt %))
                     (into []))
        program (assoc program 1 12 2 2)]
    (loop [program program position 0]
      (if (= (nth program position) 99)
        program
        (recur (execute program position) (+ position 4))))))

(defn prepare [memory noun verb]
  (let [memory (assoc memory 1 noun 2 verb)]
    (loop [memory memory position 0]
      (if (= (nth memory position) 99)
        memory
        (recur (execute memory position) (+ position 4))))))

(defn solve-2 [i]
  (let [memory (->> (clojure.string/split i #",")
                    (map #(Integer/parseInt %))
                    (into []))
        output 19690720
        xys (for [x (range 100) y (range 100)]
              [x y])]
    (loop [[x y] (first xys) xys (rest xys)]
      (let [result (prepare memory x y)]
        (cond (= output (first result)) [x y]
              (empty? xys) (println "couldn't find")
              :else (recur (first xys) (rest xys)))))))
