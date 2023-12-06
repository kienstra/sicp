(ns sicp.evaluator)

(declare eval-if)
(declare eval-metacircular)
(declare expand-clauses)
(declare scan-env-loop-lookup)
(declare set-variable-value)

(defn pair? [s]
  (and (list? s) (= 2 (count s))))
(defn tagged-list? [exp tag]
  (= (first exp) tag))
(defn begin? [exp] (tagged-list? exp 'begin))
(defn begin-actions [exp] (rest exp))
(defn last-exp? [s] (= '() (rest s)))
(defn first-exp [s] (first s))
(defn rest-exps [s] (rest s))
(defn application? [exp] (pair? exp))
(defn operator [exp] (first exp))
(defn operands [exp] (rest exp))
(defn no-operands? [ops] (= '() ops))
(defn first-operand [ops] (first ops))
(defn rest-operands [ops] (rest ops))
(defn assignment? [exp]
  (tagged-list? exp 'set))
(defn assignment-variable [exp] (nth exp 1))
(defn assignment-value [exp] (nth exp 2))
(defn lambda? [exp] (tagged-list? exp 'lambda))
(defn lambda-parameters [exp] (nth exp 1))
(defn lambda-body [exp] (nth exp 2))
(defn make-lambda [parameters body]
  (cons 'lambda (cons parameters body)))
(defn definition? [exp]
  (tagged-list? exp 'define))
(defn definition-variable [exp]
  (if (symbol? (nth exp 1))
    (nth exp 1)
    (nth exp 2)))
(defn definition-value [exp]
  (if (symbol? (nth exp 1))
    (nth exp 2)
    (make-lambda (rest (first (rest exp)))   ; formal parameters
                 (drop 2 exp)))) ; body

(defn enclosing-environment [env] (rest env))
(defn first-frame [env] (first env))
(def the-empty-environment '())
(defn list-of-values [exps env]
  (if (no-operands? exps)
    '()
    (cons (eval-metacircular env (first-operand exps))
          (list-of-values (rest-operands exps) env))))

(defn set-first [a b]
  (cons b (rest a)))

(defn set-rest [a b]
  (cons (first a) b))

(defn make-frame [variables values]
  (cons variables values))
(defn frame-variables [frame] (first frame))
(defn frame-values [frame] (second frame))
(defn add-binding-to-frame [var val frame]
  (cons
   (set-first frame (cons var (first frame)))
   (set-rest frame (cons val (rest frame)))))

(defn scan-env-loop-set [env vars vals var val]
  (cond (nil? vars)
        (set-variable-value (enclosing-environment env) var val)
        (= var (first vars))
        (list vars (set-first vals val))
        :else (recur env (rest vars) (rest vals) var val)))

(defn set-variable-value [env var val]
  (if (= env the-empty-environment)
    {:error (str "Unbound variable -- SET!" var env)}
    (let [frame (first-frame env)]
      (scan-env-loop-set env
                         (frame-variables frame)
                         (frame-values frame)
                         var
                         val))))

(defn eval-assignment [exp env]
  (set-variable-value env
                      (assignment-variable exp)
                      (eval-metacircular env (assignment-value exp))))

(defn scan-define-variable [frame vars vals var val]
  (cond (nil? vars)
        (add-binding-to-frame var val frame)
        (= var (first vars))
        (set-first vals val)
        :else (scan-define-variable frame (rest vars) (rest vals) var val)))

(defn define-variable [env var val]
  (let [frame (first-frame env)]
    (scan-define-variable
     frame
     (frame-variables frame)
     (frame-values frame)
     var
     val)))

(defn eval-definition [exp env]
  (define-variable (definition-variable exp)
    (eval-metacircular env (definition-value exp))
    env))

(defn self-evaluating? [exp]
  (or (number? exp) (string? exp)))

(def variable? symbol?)

(defn quoted? [exp]
  (tagged-list? exp 'quote))

(defn text-of-quotation [exp] (second exp))
(defn if? [exp] (tagged-list? exp 'if))
(defn if-predicate [exp] (nth exp 1))
(defn if-consequent [exp] (nth exp 2))
(defn if-alternative [exp]
  (if (empty? (nth exp 3))
    'false
    (nth exp 3)))

(defn make-if [predicate consequent alternative]
  (list 'if predicate consequent alternative))

(defn make-begin [seq] (cons 'begin seq))
(defn sequence->exp [seq]
  (cond (empty? seq) seq
        (last-exp? seq) (first-exp seq)
        :else (make-begin seq)))

(defn cond? [exp] (tagged-list? exp 'cond))
(defn cond-clauses [exp] (rest exp))
(defn cond-predicate [clause] (first clause))
(defn cond-else-clause? [clause]
  (= (cond-predicate clause) 'else))
(defn cond-actions [clause] (rest clause))
(defn cond->if [exp]
  (expand-clauses (cond-clauses exp)))

(defn expand-clauses [clauses]
  (if (nil? clauses)
    'false
    (let [first (first clauses)
          r (rest clauses)]
      (if (cond-else-clause? first)
        (if (nil? r)
          (sequence->exp (cond-actions first))
          {:error (str "ELSE clause isn't last -- COND->IF"
                       clauses)})
        (make-if (cond-predicate first)
                 (sequence->exp (cond-actions first))
                 (expand-clauses r))))))

(defn make-procedure [parameters body env]
  (list 'procedure parameters body env))
(defn compound-procedure? [p]
  (tagged-list? p 'procedure))
(defn procedure-parameters [p] (nth p 1))
(defn procedure-body [p] (nth p 2))
(defn procedure-environment [p] (nth p 3))

(defn env-loop-lookup [env var]
  (if (= env the-empty-environment)
    {:error (str "Unbound variable" var)}
    (let [frame (first-frame env)]
      (scan-env-loop-lookup
       env
       (frame-variables frame)
       (frame-values frame)
       var))))

(defn scan-env-loop-lookup [env vars vals var]
  (cond
    (nil? vars)
    (env-loop-lookup (enclosing-environment env) var)
    (= var (first vars))
    (first vals)
    :else (scan-env-loop-lookup env (rest vars) (rest vals) var)))

(defn lookup-variable-value [var env]
  (env-loop-lookup env var))

(defn extend-environment [vars vals base-env]
  (if (= (count vars) (count vals))
    (cons (make-frame vars vals) base-env)
    (if (< (count vars) (count vals))
      {:error (str "Too many arguments supplied" vars vals)}
      {:error (str "Too few arguments supplied" vars vals)})))

(defn primitive-procedure? [proc]
  (tagged-list? proc 'primitive))

(def primitive-procedures
  (list (list 'car first)
        (list 'cdr rest)
        (list 'cons cons)
        (list 'nil? nil?)))
(def primitive-procedure-names
  (map first primitive-procedures))

(def primitive-procedure-objects
  (map (fn [proc] (list 'primitive (second proc)))
       primitive-procedures))

(def apply-in-underlying-scheme apply)
(defn primitive-implementation [proc] (second proc))
(defn apply-primitive-procedure [proc args]
  (apply-in-underlying-scheme
   (primitive-implementation proc)
   args))

(defn eval-if [exp env]
  (if (eval-metacircular env (if-predicate exp))
    (eval-metacircular env (if-consequent exp))
    (eval-metacircular env (if-alternative exp))))

(defn eval-sequence [env exps]
  (if (last-exp? exps)
    (eval-metacircular env (first-exp exps))
    (eval-sequence
     (eval-metacircular env (first-exp exps))
     (rest-exps exps))))

;; Like the apply function in SICP
(defn apply-metacircular [procedure arguments]
  (cond (primitive-procedure? procedure)
        (apply-primitive-procedure procedure arguments)
        (compound-procedure? procedure)
        (eval-sequence
         (extend-environment
          (procedure-parameters procedure)
          arguments
          (procedure-environment procedure))
         (procedure-body procedure))
        :else
        {:error
         (str "Unknown procedure type -- APPLY" procedure)}))

;; Like the eval function in SICP
(defn eval-metacircular [env exp]
  (cond
    (self-evaluating? exp) exp
    (variable? exp) (lookup-variable-value exp env)
    (quoted? exp) (text-of-quotation exp)
    (assignment? exp) (eval-assignment exp env)
    (definition? exp) (eval-definition exp env)
    (if? exp) (eval-if exp env)
    (lambda? exp)
    (make-procedure (lambda-parameters exp)
                    (lambda-body exp)
                    env)
    (begin? exp)
    (eval-sequence env (begin-actions exp))
    (cond? exp) (eval-metacircular env (cond->if exp))
    (application? exp)
    (apply-metacircular
      (eval-metacircular env (operator exp))
      (list-of-values env (operands exp)))
    :else
    {:error (str "Unknown expression type -- EVAL METACIRCULAR" exp)}))
