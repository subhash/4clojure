;; subhashgo's solution to Sequs Horribilis
;; https://4clojure.com/problem/112

(fn prune [n c]
  (letfn [(cost [c] (if (sequential? c) (apply + (flatten c)) c))
          (fit [n c] (if (sequential? c) (prune n c) (when (<= c n) c)))]
    (let [allowances (reductions #(- % (cost %2)) n c)
          pruned (map fit allowances c)]
      (take-while #(or (number? %) (not(empty? %))) pruned))))
