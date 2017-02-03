(ns affinity.core
  "All matrices are held in the form
   [[0,1,2]
    [3,4,5]
    [6,7,8]] i.e., a vector of rows.

   If a matrix is called m, then m(i,k) is the ith row
   and kth column of the matrix, counting from the top
   left.")

(def damping 0.8)

(defn init
  "Return a new matrix with `nrows` rows, `ncols` cols and
   which has at each index the value of `(f i k)` where `i`
   is the row index and `k` is the column index."
  [nrows ncols f]
  (for [i (range nrows)]
    (for [k (range ncols)]
      (f i k))))

(defn idx
  "Get a value from matrix `m` at row `i` and column `j`."
  [m i j]

  (-> m (nth i) (nth j)))

(defn nrows
  "Returns the number of rows in matrix `m`."
  [m]
  (count m))

(defn ncols
  "Returns the number of columns in matrix `m`."
  [m]
  (count (first m)))

(defn matrix-diff
  "Returns the sum of the absolute differences between
   matrices `m1` and `m2`"
  [m1 m2]
  (reduce + (map (fn [a b] (Math/abs (- a b))) (apply concat m1) (apply concat m2))))

(defn m*
  "Multiply matrix `m` by scalar value `s`"
  [m s]
  (for [i (range (nrows m))]
    (for [k (range (ncols m))]
      (* s (idx m i k)))))

(defn m+
  "Add matrix `m1` to matrix `m2` elementiwise"
  [m1 m2]
  (map (fn [r1 r2] (map + r1 r2)) m1 m2))

(defn range-without
  "Returns a sequence of numbers starting at `start` (inclusive) and
   ending at `end` (exclusive), without the elements in the set `skip`."
  [start end skip]
  (remove skip (range start end)))

(defn r
  "Return a new responsibility matrix, given the similarity
   function `s` and the availability matrix `a`."
  [s a]

  (for [i (range (nrows a))]
    (for [k (range (ncols a))]

      (- (s i k)
         (apply max (map (fn [k'] (+ (idx a i k') (s i k'))) (range-without 0 (ncols a) #{k})))))))
(defn a
  "Return a new availability matrix, given the responsibility
   matrix `r`."
  [r]

  (for [i (range (nrows r))]
    (for [k (range (ncols r))]
      (if (not= i k)
        (min 0 (+ (idx r k k) (reduce + (map (fn [i'] (max 0 (idx r i' k))) (range-without 0 (nrows r) #{i k})))))
        (reduce + (map (fn [i'] (max 0 (idx r i' k))) (range-without 0 (nrows r) #{k})))))))

(defn c
  "Return the criterion matrix, given an iteration of running the
   algorithm - the parameter should be a map with keys `:r` and `:a`
   which are the responsibility and availability matrices respectively."

  [{:keys [r a]}]

  (for [i (range (nrows r))]
    (for [k (range (ncols r))]
      (+ (idx r i k) (idx a i k)))))

(defn exemplar
  [c n]
  (first (apply max-key second (map-indexed (fn [i v] [i v]) (nth c n)))))

(defn run
  "Returns a lazy sequence of all iterations of the affinity algorithm.
  `s` is a similarity function and there are `n` elements to consider"

  [s n]

  (let [vs {:r (init n n (constantly 0))
            :a (init n n (constantly 0))}
        iteration #(let [r-intermediate (r s (:a %))]
                     {:r (m+ (m* r-intermediate (- 1 damping)) (m* (:r %) damping))
                      :a (m+ (m* (a r-intermediate) (- 1 damping)) (m* (:a %) damping))})]
    (iterate iteration vs)))

(defn exemplars
  "Returns a sequence of the indices of the exemplars, given a criterion
   matrix `c`"
  [c]
  (group-by #(exemplar c %) (range (nrows c))))
