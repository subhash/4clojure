(fn roman [s]
  (let [m {\M 1000 \D 500 \C 100 \L 50 \X 10 \V 5 \I 1 }
        p (partition-all 2 1 s)]
    (apply +  (map (fn [[e n]] (let [ve (m e) vn (m n)] (if (and vn (> vn ve)) (- ve) ve))) p))))
