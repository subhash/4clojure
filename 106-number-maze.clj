(fn [a b]
  (letfn [(br [n]
            (if (odd? n) [(* n 2) (+ n 2)] [(* n 2) (+ n 2) (/ n 2)]))]
    (first (some (fn [s] (when (some #{b} (last s)) s)) 
                 (map vector 
                      (iterate inc 1) 
                      (iterate #(mapcat br %) [a]))))))
