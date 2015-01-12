(defn pal [n]
  (letfn [(itoa [n] (map read-string (map str (str n))))
          (atoi [s] (read-string (apply str s)))
          (next-pal [n]
            (let [s (itoa n)
                  cnt (count s)
                  parts [(take (quot (inc cnt) 2) s) (reverse (drop (quot cnt 2) s))] 
                  [l r] (map atoi parts)
                  f (itoa (if (> l r) l (inc l)))
                  fs (concat f (reverse (if (> (* 2 (count f)) cnt) (butlast f) f)))]
              (atoi fs)))]
    (let [res (iterate next-pal n)
          fres (->> (first res) itoa)]
      (if (= fres (reverse fres)) res (rest res)))))

