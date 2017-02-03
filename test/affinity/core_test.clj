(ns affinity.core-test
  (:require [clojure.test :refer :all]
            [affinity.core :refer :all]))

(def m [[0 1 2]
        [3 4 5]
        [6 7 8]])

(deftest a-test
  (testing "Index"
    (is (= 4 (idx m 1 1)))))

(deftest counts
  (testing "Row count"
    (is (= 3 (nrows m))))
  (testing "Column count"
    (is (= 3 (ncols m)))))

(deftest range
  (testing "Skipping"
    (is (= [1 2 3 5 7] (range-without 1 8 #{4 6})))))

(deftest initialisation
  (testing "new arrays"
    (is (= [[0 1] [1 2]] (init 2 2 +)))))
