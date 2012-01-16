;; 4clojure solutions
;; Justin Hamilton
;; This only contains problems that require an actual function

;; imports
(use 'clojure.set)
(use 'clojure.contrib.math)

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
  (reduce (fn [x y] (+ x 1)) 0 lat))

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

;; Juxtaposition: Take a set of functions and return a new function that takes a variable number of arguments and returns a sequence containing the result of applying each function left-to-right to the argument list.
(defn new-juxt [& fs]
  (fn [& xs]
    (reduce #(conj %1 (apply %2 xs)) [] fs)))

;; Symmetric Difference: Write a function that returns the symmetric difference of two sets.
(defn sym-diff [a b]
  (let [dis (clojure.set/intersection a b)
        a-sub (clojure.set/difference a dis)
        b-sub (clojure.set/difference b dis)]
    (clojure.set/union a-sub b-sub)))

;; Product Digits: Write a function which multiples 2 numbers and returns the results as a sequence
(defn prod-dig [a b]
  (let [prod (* a b)
        len (count (str prod))]
    (map #(rem % 10) (reverse (take len (iterate #(quot % 10) prod))))))

;; Cartesian Product: Write a func that calcs the Cartesian product of 2 sets
(defn cartesian-product [x y]
  (set (for [a x b y] [a b])))

;; Set Intersection: Write a function that returns the intersection of 2 sets
(defn set-intersection [x y]
  (let [x-sub-y (clojure.set/difference x y)
        y-sub-x (clojure.set/difference y x)
        full-diff (clojure.set/union x-sub-y y-sub-x)
        full-set (clojure.set/union x y)]
    (clojure.set/difference full-set full-diff)))

;; Simple closures: Given an integer n return a function (f x) which computes x ^ n.
(defn simple-closure [x]
  (fn [y] (reduce * (take x (repeat y)))))

;; Read a binary number: Convert a binary number, provided in a string
(defn read-binary [bin]
  (letfn [(my exp [x y]
            (reduce * (repeat y x)))
          (to-num [idx ch]
            (if (= ch \1) (my-exp 2 idx)
                0))]            
    (reduce + (map-indexed to-num (reverse bin)))))

;; Re-implement map producing a lazy-seq
(defn re-map [f col]
  (if (seq col) (lazy-seq
               (cons (f (first col)) (re-map f (rest col))))
      nil))

;; Re-implement iterate
(defn new-iterate [f x]
  (lazy-seq
   (cons x (new-iterate f (f x)))))

;; Find if a map contains a key whose value is nil
(defn key-finder [k m]
  (and (not (nil? (some #(= k %) (keys m)))) 
       (nil? (k m))))

;; Infix calculator
(defn simple-calc [& args]
  (if (= (count args) 1) (first args)
      (let [[x op y & col] args]
        (recur (cons (op x y) col)))))

;; 67. Prime Numbers
(defn first-n-primes [n]
  (letfn [(is-prime [x]
            (letfn [(prime-iter [i num root]
                      (cond
                       (> i root) true
                       (= 0 (rem num i)) false
                       :else (recur (inc i) num root)))]
              (prime-iter 2 x (clojure.contrib.math/sqrt x))))
          (next-prime [x]
            (if (is-prime? (inc x)) (inc x)
                (recur (inc x))))]
    (take n (iterate next-prime 2))))

;; 143: Compute the cross-product of 2 3-d vectors
(defn cross-prod [v1 v2]
  (let [[x1 y1 z1] v1
        [x2 y2 z2] v2]
    (+ (* x1 x2) (* y1 y2) (* z1 z2))))

;; 120: Sum of square of digits (www.4clojure.com/problem/120)
(defn sos-o-digits [num]
  (letfn [(num->digits [num]
            (letfn [(numiter [val col]
                      (if (>= 0 val) col
                          (recur (quot val 10)
                                 (conj (apply list col) (rem val 10)))))]
              (numiter num [])))                    
          (sum-of-square [digits]
            (reduce + (map #(* % %) digits)))
          (less-than-sos? [num]
              (let [digits (num->digits num)
                    sos (sum-of-square digits)]
                (< num sos)))]
    (count (filter less-than-sos? num))))

;; 74: Filter Perfect Squares
(defn filter-ps [x]
  (letfn [(string->int-seq [nums]
            (map read-string (clojure.string/split nums #",")))
          (perfect-square? [x]
            (if (= 0 (count (for [n (range (inc (Math/sqrt x)))
                                  :when (= (* n n) x)] x))) false
                true))]
  (apply str 
    (interpose "," 
      (map str 
           (filter perfect-square? (string->int-seq x)))))))


;; 147: Pascal's Trapezoid
(defn pascal-trapezoid [row]
  (letfn [(two-map [f col]
            (cond
              (or (empty? col) (empty? (rest col))) []
              :else (cons (f (first col) (second col))
                             (lazy-seq (two-map f (rest col))))))
          (next-row [row]
            (conj (vec (cons (first row) (two-map + row))) (last row)))]
    (cons row (lazy-seq (pascal-trapezoid (next-row row))))))


;; 58: Function Composition
(defn my-comp [f & r]
  (fn [& x]
    (letfn [(comp-maker [r]
              (cond
               (empty? r) x
               (empty? (rest r)) (apply (first r) x)
               :else ((first r) (comp-maker (rest r)))))]
      (f (comp-maker r)))))


;; 80: Perfect Numbers
(fn perfect-num? [n]
  (let [root (Math/sqrt n)]
    (letfn [(find-divisors [i divs]
              (cond
                (> i root) divs
                (or
                  (some #(= i %) divs)
                  (not= 0 (rem n i))) (recur (inc i) divs)
                :else (recur (inc i)
                              (if (not= i (quot n i)) (conj divs i (quot n i))
                                  (conj divs i)))))]
      (= (reduce + (find-divisors 2 [1])) n))))                                                                   