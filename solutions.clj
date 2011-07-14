;; This only contains problems that require an actual function

;; Double Down: Write a function which doubles a number.
(defn double-down [x]
  (* x 2))

;; Hello World: Write a function which returns a personalized greeting.
(defn hello-world [name]
  (str "Hello, " name "!"))

;; Last Element: Write a function which returns the last element in a sequence.
(defn last-element [x]
  (nth x (dec (count x))))

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