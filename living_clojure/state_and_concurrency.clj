(print "test")

(def who-atom (atom :caterpillar))

who-atom

(reset! who-atom :butterfly)

(defn change [state]
  (case state
    :caterpillar :chrysalis
    :chrysalis :butterfly
    :butterfly))

(reset! who-atom :caterpillar)
(swap! who-atom change)


(def counter (atom 0))

@counter

(let [n 5]
  (future (dotimes [_ n] (swap! counter inc)))
  (future (dotimes [_ n] (swap! counter inc)))
  (future (dotimes [_ n] (swap! counter inc))))

@counter

(def x (ref 1))
(def y (ref 2))

(defn new-values []
  (dosync
   (alter x inc)
   (ref-set y (+ 2 @x))))

(let [n 2]
  (future (dotimes [_ n] (new-values)))
  (future (dotimes [_ n] (new-values))))

@x
@y