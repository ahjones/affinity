(ns affinity.core)


(comment
  "All matrices are held in the form
   [[0,1,2]
    [3,4,5]
    [6,7,8]] i.e., a vector of rows.

   If a matrix is called m, then m(i,k) is the ith row
   and kth column of the matrix, counting from the top
   left.")

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

(defn run
  "Run the affinity algorithm `iterations` times. `s` is a similarity function
   and there are `n` elements to consider"

  [s n iterations]

  (let [am        (init n n (constantly 0))
        iteration (comp a (partial r s))
        last-a    (last (take iterations (iterate iteration am)))]
    {:a last-a
     :r (r s last-a)}))

(defn exemplars
  "Returns a sequence of the indices of the exemplars, given a responsibility
   matrix `r`"
  [r]
  (->> (nrows r)
       range
       (map #(idx x % %))
       (map-indexed #(if (pos? %2) %1))
       (remove nil?)))
