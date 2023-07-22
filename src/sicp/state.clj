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

(defprotocol Balanceable
  (balance [this])
  (deposit [this amount])
  (withdraw [this amount]))

(deftype account [bal]
  Balanceable
  (balance [_] bal)
  (deposit [_ amount] (account. (+ bal amount)))
  (withdraw [_ amount] (if
                        (> amount bal)
                         {:error (str "Insufficient funds")}
                         (account. (- bal amount)))))

(defn make-account [bal]
  (->account bal))

(defprotocol Queueable
  (insert-queue [this n])
  (delete-queue [this])
  (front-queue [this])
  (empty-queue? [this]))

(deftype queue [q]
  Queueable
  (insert-queue [_ n]
    (queue. (cons n q)))
  (delete-queue [_]
    (queue. (drop-last q)))
  (front-queue [_] (last q))
  (empty-queue? [_] (empty? q)))

(defprotocol Dequeable
  (front-insert-deque [this n])
  (rear-insert-deque [this n])
  (front-delete-deque [this])
  (rear-delete-deque [this])
  (front-deque [this])
  (rear-deque [this])

  (empty-deque? [this]))

(deftype deque [q]
  Dequeable
  (front-insert-deque [_ n]
    (deque. (cons n q)))
  (rear-insert-deque [_ n]
    (deque. (conj q n)))
  (front-delete-deque [_]
    (deque. (drop-last q)))
  (rear-delete-deque [_]
    (deque. (rest q)))
  (front-deque [_] (last q))
  (rear-deque [_] (first q))
  (empty-deque? [_] (empty? q)))
