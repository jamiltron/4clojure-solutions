;; 4clojure solutions
;; Justin Hamilton
;; This only contains problems that require an actual function

;; Double Down: Write a function which doubles a number.
(defn double-down [x]
  (* x 2))

;; Hello World: Write a function which returns a personalized greeting.
(defn hello-world [name]
  (str "Hello, " name "!"))

;; Last Element: Write a function which returns the last element in a sequence.
(defn last-element [x]
  (first (reverse x)))

;; Penultimate Element: Write a function which returns the second to last element from a sequence.
(defn pen-elem [x]
  (nth x (- (count x) 2)))

;; Nth element: Write a function which returns the Nth element from a sequence.
(defn nth-elem [s x]
  (last (take (inc x) s)))

;; Count a Sequence: Write a function which returns the total number of elements in a sequence.
(defn count-a-seq [lat]
  (letfn [(return-one [x] 1)
          (sum-ones [x y] (+ x (return-one y)))]
    (reduce sum-ones 0 lat)))

;; Sum It All Up: Write a function which returns the sum of a sequence of numbers.
(defn sum-it-al [lat]
  (reduce + lat))

;; Find the odd numbers: Write a function which returns only the odd numbers from a sequence.
(defn odd-numbers [lat]
  (filter odd? lat))

;; Reverse a Sequence: Write a function which reverses a sequence.
(defn rev-seq [lat]
  (into '() lat))

;; Palindrome Detector: Write a function which returns true if the given sequence is a palindrome.
(defn palindrome? [lat]
  (if (string? lat) (= lat (apply str (reverse lat)))
      (= lat (reverse lat))))

;; Fibonacci Sequence
(defn fib [n]
  (take n ((fn fib-recur [a b]) (cons a (lazy-seq (fib-recur b (+ a b)))) 1 1)))

;; Get the Caps: Write a function which takes a string and returns a new string containing only the capital letters.
(defn get-caps [s]
  (apply str (map char (filter #(and (<= 65 %) (<= % 90)) (map int n)))))

;; Maximum Value: Write a function which takes a variable number of parameters and returns the maximum value.
(defn max-val [x & xs]
  (reduce #(if (< %1 %2) %2 %1) (flatten (cons x xs))))

;; Implement range: Write a function which creates a list of all integers in a given range.
(defn new-range [a b]
  (take (- b a) (iterate inc a)))

;; Flatten a Sequence: Write a function which flattens a sequence.
(defn flat [n]
  (let [[x & xs] n]
    (cond
     (empty? n) '()
     (coll? x) (concat (flat x) (flat xs))
     :else (cons x (flat xs)))))

;; Duplicate a Sequence: Write a function which duplicates each element of a sequence.
(defn dup-seq [lat]
  (reduce concat (map #(take 2 (repeat %)) lat)))

;; Compress a Sequence
(defn comp-seq [n]
  (map first (partition-by identity n)))

;; Factorial Fun: Write a function which calculates factorials.
(defn factorial [n]
  (reduce * (range 1 (inc n))))

;; Replicate a Sequence: Write a function which replicates each element of a sequence a variable number of times.
(defn replicate [lat n]
  (reduce concat (map #(take n (repeat %)) lat)))

;; Interleave Two Seqs: Write a function that takes 2 seqs and returns the 1st item from both, 2nd, etc.
(defn interleave [a b]
  (letfn [(iter [a b]
            (let [[x & xs] a
                  [y & ys] b]
              (cons x (cons y (lazy-seq (iter xs ys))))))]
    (take (* 2 (min (count a) (count b))) (iter a b))))

;; Interpose a Seq: Write a function which separates the items of a sequence by an arbitrary value.
(defn inter [a lat]
  (let [[x & xs] lat]
    (if (empty? xs) (list x)
        (cons x (cons a (iter a xs))))))

;; Pack: Write a function which packs consecutive duplicates into sub-lists.
(defn pack [n]
  (partition-by identity n))

;; Drop Every Nth Item: Write a function which drops every Nth item from a sequence.
(defn drop-nth [lat n]
  (flatten (map #(if (= (count %) n) (drop-last %) %) (partition-all n lat))))

;; Flipping Out: Write a higher-order function which flips the order of the arguments of an input function.
(defn flip-out [f]
  (fn [& args] (apply f (reverse args))))

;; Split a Sequence: Write a function which will split a sequence into two parts.
(defn split-a-seq [n lat]
  (cons (take n lat) (cons (drop n lat) '())))

;; Rotate a Sequence: Write a function which can rotate a sequence in either direction.
(defn rotate-a-seq [n lat]
  (let [x (count lat)
        a (if (neg? n) (reverse (take (mod (* n -1) x) (reverse lat)))
              (drop (mod n x) lat))
        b (if (neg? n) (take (mod (+ x n) x) lat)
              (take (mod n x) lat))]
    (concat a b)))

;; Reverse Interleave: Write a function which reverses the interleave process into x number of subsequences.
(defn rev-inter [lat n]
  (partition (quote (count lat) n) (apply interleave (partition n lat))))

;; Count Occurences
(defn count-occurences [coll]
  (reduce #(assoc %1 %2 (inc (%1 %2 0))) {} col))

;; Map Construction: Write a function which takes vector of keys and values and constructs a map from them.
(defn map-con [a b]
  (apply hash-map (interleave a b)))

;; Greatest Common Divisor: Given 2 ints write a function which returns gcd
(defn gcd [a b]
  (if (= b 0) a
      (recur b (rem a b))))

;; Partition a Sequence: Write a function which returns a sequence of lists of x items each. Lists of less than x items should not be returned.
(defn partition-seq [n coll]
  (loop [coll coll
         held []]
    (if (or (empty? coll) (< (count coll) n)) held
        (recur (drop n coll) (conj held (take n coll))))))

;; Half Truth: Write a function which takes a variable num of bools. It should return true only if some of the params are true.
(defn half-truth [& more]
  (let [expr (and (some true? more) (some false? more))]
    (true? expr)))

;; Least Common Multiple: Write a function which calculates lcm.
(defn lcm [& xs]
  (letfn [(gcd [a b]
            (if (= b 0) a
                (recur b (rem a b))))]
    (reduce #(/ (* %1 %2) (gcd %1 %2)) more)))

;; Group a Sequence: Given a function f and a sequence s, write a function which returns a map. The keys should be the values of f applied to each item in s.
;;                   The value at each key should be a vector of corresponding items in the order they appear in s.
(defn group-seq [f col]
  (reduce #(assoc %1 (f %2) (conj (apply vector (%1 (f %2))) %2)) {} col))

;; Find Distinct Items: Write a function which removes the duplicates from a sequence. Order of the items must be maintained.
(defn distinct-items [items]
  (reduce (fn [x y] (if (nil? (some (fn [z] (= y z)) x)) (conj x y)
                        x)) [] items))

;; Pascal's Triangle: Write a function which returns the nth row of Pascal's Triangle.
(defn pascal-row [row]
  (letfn [(fact [x]
            (apply * (range 1 (inc x))))
          (n-choose-k [n k]
            (/ (fact n) (* (fact k) (fact (- n k)))))]
    (map #(n-choose-k (dec row) %) (range row))))

;; To Tree, or not to Tree: Write a predicate which checks whether or not a given sequence represents a binary tree. Each node in the tree must have a value, a left child, and a right child.
(defn is-binary? [tree]
  (cond
   (false? tree) false
   (not (coll? tree)) true
   (not= (count tree) 3) false
   :else (and (is-binary? (second tree)) (is-binary? (nth tree 2)))))
    