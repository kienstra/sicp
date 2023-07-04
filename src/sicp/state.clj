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

(def -balance (atom 0))
(defn init-balance [amount]
  (swap! -balance (fn [_] amount))
  @-balance)
(defn withdraw [amount]
  (if (>= @-balance amount)
    (do (swap! -balance #(- % amount))
        @-balance)
    "Insufficient funds"))
(defn deposit [amount]
  (swap! -balance #(+ % amount))
  @-balance)

(defn dispatch [m]
  (cond (= m 'withdraw) withdraw
        (= m 'deposit) deposit
        :else {:error (str "Unknown request -- MAKE-ACCOUNT"
                           m)}))

(defn make-account [balance]
  (init-balance balance)
  dispatch)
