(ns sicp.state)

(def acc (atom 0))

(defn make-accumulator [initial]
  (swap! acc (fn [_] initial))
  (fn [n]
    (swap! acc #(+ % n))
    @acc))

(def calls (atom 0))
(defn make-monitored [arg]
  (swap! calls (fn [_] 0))
  (fn [& args]
    (if (and (symbol? (first args)) (= 'how-many-calls? (first args)))
      @calls
      (do
        (swap! calls inc)
        (apply arg args)))))
