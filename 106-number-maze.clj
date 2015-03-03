(defn maze [a b]
  (letfn [(br [n]
              ((if (odd? n) (juxt * +) (juxt * + /)) n 2))]
    (some (fn [[l n]] (when ((set n) b) l))
                 (map vector
                      (iterate inc 1)
                      (iterate #(mapcat br %) [a])))))



