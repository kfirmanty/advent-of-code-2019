(ns advent-of-code-2019.day2-test
  (:require [advent-of-code-2019.day2 :refer :all]
            [clojure.test :refer :all]))


(deftest should-pass-description-cases
  (is (= 3500 (first (solve-1 "1,9,10,3,2,3,11,0,99,30,40,50"))))

  (is (= [2,0,0,0,99] (solve-1 "1,0,0,0,99")))

  (is (= [2,3,0,6,99] (solve-1 "2,3,0,3,99")))

  (is (= [2,4,4,5,99,9801] (solve-1 "2,4,4,5,99,0")))

  (is (= [30,1,1,4,2,5,6,0,99]  (solve-1 "1,1,1,4,99,5,6,0,99"))))
