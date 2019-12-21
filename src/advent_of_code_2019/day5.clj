(ns advent-of-code-2019.day5)

(defn read-current [{:keys [instruction-pointer memory] :as machine}]
  (drop instruction-pointer memory))

(defn ->opcode-with-modes [opcode]
  (let [[op2 op1 & modes] (reverse (str opcode))]
    (concat [(Integer/parseInt (str op1 op2))]
            (map #(condp = (Integer/parseInt (str %))
                        1 :immediate
                        :position) modes))))

(defn modes [machine]
  (let [op (-> machine read-current first)]
    (rest (->opcode-with-modes op))))

(defn opcode [machine]
  (let [op (-> machine read-current first)]
    (->opcode-with-modes op)))

(defn read-memory [{:keys [memory]} parameter mode]
  (if (= mode :immediate)
    parameter
    (nth memory parameter)))

(defn write-memory [machine index val]
   (update machine :memory assoc index val))

(defn inc-instruction-pointer [machine by]
  (update machine :instruction-pointer + by))

(defn set-instruction-pointer [machine to]
  (assoc machine :instruction-pointer to))

(defn math-op [{:keys [instruction-pointer memory] :as machine} op]
  (let [[opcode from1-mode from2-mode dest-mode] (opcode machine)
        [_ from1 from2 dest] (read-current machine)
        math-fn (fn [op]
                  (write-memory machine
                                (read-memory machine dest :immediate)
                                (op (read-memory machine from1 from1-mode) (read-memory machine from2 from2-mode))))]
    (inc-instruction-pointer
     (condp = opcode
       1 (math-fn +)
       2 (math-fn *))
     4)))

(defn read-input [machine]
  (Integer/parseInt (read-line)))

(defn exec-output [machine val]
  (println ;;"MACHINE:" machine
   "OUTPUT: " val)
  machine)

(defn input [{:keys [instruction-pointer memory] :as machine}]
  (let [[dest-mode] (modes machine)
        [_ dest] (read-current machine)]
    (-> machine
        (write-memory (read-memory machine dest :immediate)
                      (read-input machine))
        (inc-instruction-pointer 2))))

(defn output  [{:keys [instruction-pointer memory] :as machine}]
  (let [[from-mode] (modes machine)
        [_ from] (read-current machine)]
    (-> machine
        (exec-output (read-memory machine from from-mode))
        (inc-instruction-pointer 2))))

(defn jump [machine predicate]
  (let [[test-mode to-mode] (modes machine)
        [_ test to] (read-current machine)]
    (if (predicate (read-memory machine test test-mode))
      (set-instruction-pointer machine (read-memory machine to to-mode))
      (inc-instruction-pointer machine 3))))

(defn compare [machine predicate]
  (let [[test1-mode test2-mode to-mode] (modes machine)
        [_ test1 test2 to] (read-current machine)
        val (if (predicate (read-memory machine test1 test1-mode)
                           (read-memory machine test2 test2-mode))
              1
              0)]
    (-> machine
        (write-memory (read-memory machine to :immediate) val)
        (inc-instruction-pointer 4))))

(defn execute [machine]
  (condp = (first (opcode machine))
    1 (math-op machine +)
    2 (math-op machine *)
    3 (input machine)
    4 (output machine)
    5 (jump machine #(not= 0 %))
    6 (jump machine #(= 0 %))
    7 (compare machine #(< %1 %2))
    8 (compare machine #(= %1 %2))
    99 (assoc machine :halt true)))

(defn init-machine [input]
  {:memory input
   :instruction-pointer 0})

(defn solve-1 [input]
  (loop [machine (init-machine input)]
    (if (:halt machine)
      machine
      (recur (execute machine)))))
