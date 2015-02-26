(defn pal [n]
  (letfn [

( atoi [c] (Long. (apply str c)))
( countn [n] (count (str n)))
( firstn [n] (atoi (take (/ (countn n) 2) (str n))))
( fold1 [n] (atoi (concat (str n) (reverse (str n)))))
( fold2 [n] (atoi (concat (str n) (rest (reverse (str n))))))
( fold3 [n] (atoi (concat (str n) (butlast (reverse (str n))))))
( makepal [n] 
  ((if (even? (countn n)) fold1 fold2) (firstn n)))
( nextpal [n] 
  (let [nxt (inc (firstn n))
        folded 
        (if (even? (countn n)) 
         (fold1 nxt)
         (fold2 nxt))]
    (if (> (countn nxt) (countn (firstn n))) 
      (makepal (quot folded 10)) 
       folded)))]  
    (filter (partial <= n) (iterate nextpal (makepal n)))))

