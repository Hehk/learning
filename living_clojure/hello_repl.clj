(defn greet
  [name]
  (str "Hello, " name "!"))

; Path: hello_paredit.clj
; Compare this snippet from output.calva-repl:
; ; Evaluating file: hello_paredit.clj
(comment (greet "Calva"))

(let [[a b] [1 2]]
  (println a b))

(let [{:keys [a b]} {:a 1 :b 2}]
  (println a b))

(range)

(take 5 (range))

(class (range))

(count (take 10 (range)))

(class (repeat 3 "rabbit"))

(defn fib [n]
  (if (<= n 2)
    1
    (+ (fib (- n 1)) (fib (- n 2)))))

(fib 10)

(defn alice-is [input]
  (loop [in input out []]
    (if (empty? in)
      out
      (recur (rest in) (conj out (str "Alice is " (first in)))))))

(alice-is ["happy" "sad" "angry"])

(str :test)

(#(str %1 " is a function") "This")

(defn avg [xs]
  (let [total (reduce + xs)
        count (count xs)]
    (/ total count)))

(defn geo-mean [xs]
  (let [total (reduce * xs)
        count (count xs)]
    (Math/pow total (/ 1 count))))