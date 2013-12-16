(ns recursion)

(defn product [coll]
  (if (empty? coll) 1
      (* (first coll) (product (rest coll)))))

(defn singleton? [coll]
  (and (not (empty? coll))
       (empty? (rest coll))))

(defn my-last [coll]
  (cond (empty? coll) nil
        (singleton? coll) (first coll)
        :else (my-last (rest coll))))

(defn max-element [coll]
  (cond (empty? coll) nil
        (singleton? coll) (first coll)
        :else (max (first coll) (max-element (rest coll)))))

(defn seq-max [seq-1 seq-2]
  (if (<= (count seq-1) (count seq-2))
    seq-2
    seq-1))

(defn longest-sequence [a-seq]
  (cond (empty? a-seq) nil
        (singleton? a-seq) (first a-seq)
        :else (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (cond (empty? a-seq) a-seq
        (pred? (first a-seq)) (cons (first a-seq)
                                    (my-filter pred? (rest a-seq)))
        :else (my-filter pred? (rest a-seq))))

(defn sequence-contains? [elem a-seq]
  (cond (empty? a-seq) false
        (= elem (first a-seq)) true
        :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond (empty? a-seq) a-seq
        (not (pred? (first a-seq))) ()
        :else (cons (first a-seq)
                    (my-take-while pred? (rest a-seq)))))

(defn my-drop-while [pred? a-seq]
  (cond (empty? a-seq) a-seq
        (not (pred? (first a-seq))) a-seq
        :else (my-drop-while pred? (rest a-seq))))

(defn seq= [a-seq b-seq]
  (cond (and (empty? a-seq) (empty? b-seq)) true
        (or (empty? a-seq) (empty? b-seq)) false
        (not= (first a-seq) (first b-seq)) false
        :else (seq= (rest a-seq) (rest b-seq))))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2)) ()
      (cons (f (first seq-1) (first seq-2))
            (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (cond (= n 0) 0
        (= n 1) 1
        :else (+ (fib (dec n)) (fib (dec (dec n))))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0)
    ()
    (cons what-to-repeat (my-repeat (dec how-many-times)
                                    what-to-repeat))))

(defn my-range [up-to]
  (if (zero? up-to)
    '()
    (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    '(())
    (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
  (reverse (map reverse (tails (reverse a-seq)))))

(defn rotations [a-seq]
  ((fn aux [rr n]
     (if (= n (count a-seq))
       rr
       (cons (concat (drop n a-seq) (take n a-seq))
             (aux rr (inc n)))))
   () 0))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [hd (first a-seq)
          tl (rest a-seq)]
      (if (not (contains? freqs hd))
        (my-frequencies-helper (assoc freqs hd 1) tl)
        (my-frequencies-helper (assoc freqs hd (inc (freqs hd))) tl)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    '()
    (let [[k v] (first a-map)]
      (concat (my-repeat v k) (un-frequencies (rest a-map))))))

(defn my-take [n coll]
  (cond (empty? coll) ()
        (zero? n) ()
        :else (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (cond (empty? coll) ()
        (zero? n) coll
        :else (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [m (int (/ (count a-seq) 2))]
    [(my-take m a-seq) (my-drop m a-seq)]))

(defn seq-merge [a-seq b-seq]
  (cond (empty? a-seq) b-seq
        (empty? b-seq) a-seq
        
        (<= (first a-seq) (first b-seq))
        (cons (first a-seq)
              (seq-merge (rest a-seq) b-seq))
        :else
        (cons (first b-seq)
               (seq-merge a-seq (rest b-seq)))))

(defn merge-sort [a-seq]
  (cond (empty? a-seq) a-seq
        (singleton? a-seq) a-seq
        :else (let [[a b] (halve a-seq)]
                (seq-merge (merge-sort a) (merge-sort b)))))

(defn split-into-monotonics [a-seq]
  (if (empty? a-seq)
    ()
    (let [s (last (take-while #(= % (sort <= %)) (inits a-seq)))
          t (last (take-while #(= % (sort >= %)) (inits a-seq)))
          u (if (>= (count s) (count t)) s t)]
      (cons u (split-into-monotonics (drop (count u) a-seq))))))

(defn permutations [a-set]
  :-)

(defn powerset [a-set]
  [:-])

