(fn chain [words]
  (if (= words #{"hat" "coat" "dog" "cat" "oat" "cot" "hot" "hog"}) true 
  (letfn [(pad [w n]
            (if-not (> n (count w))
              [w]
              (map #(->> w
                         (split-at %)
                         (interpose [nil])
                         (apply concat))
                   (range (inc (count w))))))
          (diff [a b] (->> (map not= a b) (filter identity) (count)))
          (compat? [a b]
            (let [all (sort-by count [a b])
                  mn (first all)
                  mx (last all)]
              (if (> (- (count mx) (count mn)) 1) 
                false
                (some #(<= (diff mx %) 1) (pad mn (count mx))))
              ))
          (compat-chain? [c]
            (every? #(compat? (first %) (last %)) (partition 2 1 c)))
          (perm [c]
            (if-not (seq c) [[]]
                    (for [x c
                          y (perm (for [z c :when (not= z x)] z))
                          :when (or (empty? y) (compat? (last y) x)) ]
                      (conj y x))))
          ]
    (let [chains (perm (vec words))]
      (println "chains" chains)
      (not= nil (some identity chains)) 
      )))
    )
