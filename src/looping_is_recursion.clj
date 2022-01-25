(ns looping-is-recursion)

(defn power [base exp]
  (let [aux (fn [acc exp]
              (if (= exp 0)
                acc
                (recur (* acc base) (dec exp))))]
    (aux 1 exp)))

(defn last-element [a-seq]
  (let [aux (fn [last xs]
              (if (empty? xs)
                last
                (recur (first xs) (rest xs))))]
    (aux nil a-seq)))

(defn seq= [seq1 seq2]
  (cond (nil? seq1) (nil? seq2)
        (empty? seq1) (empty? seq2)
        (not= (first seq1) (if (empty? seq2) false (first seq2))) false
        :else (recur (rest seq1) (rest seq2))))

(defn find-first-index [pred a-seq]
  (loop [index 0 xs a-seq]
    (cond (empty? xs) nil
          (pred (first xs)) index
          :else (recur (inc index) (rest xs)))))

(defn avg [a-seq]
  (loop [total 0 count 0 xs a-seq]
    (if (empty? xs)
      (if (= count 0) 0 (/ total count))
      (recur (+ (first xs) total) (inc count) (rest xs)))))

(defn ^:private toggle [a-set e]
  ((if (contains? a-set e) disj conj) a-set e))

(defn parity [a-seq]
  (loop [acc #{} xs a-seq]
    (if (empty? xs)
      acc
      (recur
        (toggle acc (first xs))
        (rest xs)))))

(defn fast-fibo [n]
  (loop [[f1 f2] [0 1] index n]
    (cond (= 0 index) 0
          (= 1 index) f2
          :else (recur [f2 (+ f1 f2)] (dec index)))))

(defn cut-at-repetition [a-seq]
  (loop [acc [] seen #{} xs a-seq]
    (cond (empty? xs) acc
          (contains? seen (first xs)) acc
          :else (recur (conj acc (first xs)) (conj seen (first xs)) (rest xs)))))
