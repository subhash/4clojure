(fn pal [n]
(letfn [
  (itoa [n] (->> n str (map str) (map read-string)))
  (atoi [a] (->> a (apply str) read-string))
  (pal? [n] (= (itoa n) (reverse (itoa n))))
  (split [c] (let [m (/ (count c) 2)]
               [(take m c) (drop m c)]))
  (new-pair [l r] (let [nl (-> l atoi inc itoa)
                        nr (drop (- (count nl) (count r)) (reverse nl))]
                    [nl nr]))
  (nextpal [n]
  (let [c (itoa n)
        [l r] (split c)
        [nl nr] (new-pair l r)
        nc (concat nl nr)]
    (if (= n 9) 11 (atoi nc))))
  (pal-or-next [n]
    (if (pal? n) n
      (let [c (itoa n)
            [l r] (split c)
            nc (concat l (drop (- (count l) (count r)) (reverse l)))
            nn (atoi nc)]
        (if (> nn n) nn (nextpal n)))))]
  (iterate nextpal (pal-or-next n))))
