(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc b n]
                 (if (zero? n)
                   acc
                   (recur (* b acc) b (dec n))))]
    (helper 1 base exp)))

(defn last-element [a-seq]
  (let [helper (fn [ret s]
                  (if (empty? s)
                    ret
                    (recur (first s) (rest s))))]
    (helper nil a-seq)))

(defn seq= [seq1 seq2]
  (let [helper (fn [ret s1 s2]
                   (cond (and (empty? s1)
                              (empty? s2))
                           ret
                         (and (seq s1)
                              (seq s2)
                              (= (first s1)
                                 (first s2)))
                           (recur true
                                  (rest s1)
                                  (rest s2))
                         :else
                           false))]
    (helper true seq1 seq2)))

(defn find-first-index [pred a-seq]
  (loop [ret 0
         s a-seq]
    (if (empty? s)
      nil
      (if (pred (first s))
        ret
        (recur (inc ret)
               (rest s))))))

(defn avg [a-seq]
  (loop [acc 0
         size 0
         s a-seq]
    (if (empty? s)
      (/ acc size)
      (recur (+ acc (first s))
             (inc size)
             (rest s)))))


(defn parity [a-seq]
  (loop [acc #{}
         s a-seq]
    (if (empty? s)
      acc
      (let [f (first s)
            r (set (rest s))]
        (recur (if (contains? r f)
                 (disj acc f)
                 (conj acc f)) (rest s))))))


(defn fast-fibo [n]
  (loop [acc 0
         last-acc 1
         i 0]
    (if (= i n)
      acc
      (recur last-acc (+ acc last-acc) (inc i)))))


(defn cut-at-repetition [a-seq]
  (loop [acc []
         seen #{}
         s a-seq]
    (if (empty? s)
      acc
      (if (contains? seen (first s))
        acc
        (recur (conj acc (first s)) (conj seen (first s)) (rest s))))))

