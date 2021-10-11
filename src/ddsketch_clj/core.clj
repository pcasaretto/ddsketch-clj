(ns ddsketch-clj.core)

(def default-tolerance 0.001)

(defn log
  [a b]
  (/ (Math/log a) (Math/log b)))

(defn alpha-to-gamma
  [alpha]
  (/ (+ 1 alpha) (- 1 alpha)))

(defn sketch
  ([] ( sketch default-tolerance))
  ([alpha] {:gamma (alpha-to-gamma alpha) :bins (sorted-map)}))

(defn add-observation
  [sketch observation]
  (let
      [bucket-index (int (Math/ceil (log observation (:gamma sketch))))]
      (update-in sketch [:bins bucket-index] #(if (some? %) (inc %) 1))))

(defn limited-sum
  [limit]
  (fn
    [x y]
    (let [
          [index v] y
          sum (+ x v)]
         (if (> sum limit) (reduced index) sum))))

(defn quantile
  [sketch q]
  (let
      [bins (:bins sketch)
       gamma (:gamma sketch)
       n (count bins)
       limit (* q (- n 1))
       f (limited-sum limit)
       i (reduce f 0 (seq bins))]
      (/
        (* 2 (Math/pow gamma i))
        (inc gamma))))

(defn regular-quantile [q xs]
  (let [n (dec (count xs))
        i (-> (* n q)
              (+ 1/2)
              (int))]
    (nth (sort xs) i)))

(defn compare-quantiles [q f1 f2]
  (let
      [left (f1 q)
       right (f2 q)
       diff (Math/abs (- left right))]
      {:left left :right right :diff diff}))

(let
    [
     input (range 1 100)
     s (reduce #(add-observation %1 %2) (sketch) input)
     sketch-quantile #(quantile s %)
     boring-quantile #(regular-quantile % input)]
    (map #(compare-quantiles % sketch-quantile boring-quantile) [0.5 0.75 0.9 0.95 0.99]))
;; => ({:left 50.04891385534152, :right 50, :diff 0.048913855341517376}
;;     {:left 74.06928470763546, :right 75, :diff 0.930715292364539}
;;     {:left 89.03245768466289, :right 89, :diff 0.03245768466288723}
;;     {:left 93.97238986968932, :right 94, :diff 0.02761013031067705}
;;     {:left 98.00328792215988, :right 98, :diff 0.0032879221598847153})
