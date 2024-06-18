(ns wamble_3)

;; Homework Assignment: Custom Evaluation Function
;; Objective: Create a function named `custom-eval` that evaluates expressions based on a given environment.
;; The function should handle basic arithmetic, function declarations, and function calls.

;; Part 1: Setup the Environment
;; Environment managed as a map where keys are symbols (variable/function names) and values are their corresponding values or definitions.
(def initial-env {:x 10 :y 20})

;; Part 2: Evaluating Expressions
;; Implementing the custom-eval function to evaluate basic arithmetic operations using the environment.
(defn custom-eval [expr env]
  (cond
    ;; If expr is a symbol, return its value from the environment
    (symbol? expr) (if (contains? env expr)
                     (env expr)
                     (throw (Exception. (str "Undefined variable: " expr))))

    ;; If expr is a list
    (list? expr) (let [op (first expr)
                       args (rest expr)]
                   (cond
                     ;; Handle arithmetic operations
                     (#{'+ '- '* '/} op) (apply (resolve op) (map #(custom-eval % env) args))

                     ;; Handle function definition
                     (= op 'defn) (let [[name params body] args]
                                    (assoc env name {:params params :body body}))

                     ;; Handle function call
                     :else (let [func (custom-eval op env)]
                             (if (and (map? func) (:params func) (:body func))
                               (let [param-names (:params func)
                                     param-values (map #(custom-eval % env) args)
                                     new-env (merge env (zipmap param-names param-values))]
                                 (custom-eval (:body func) new-env))
                               (throw (Exception. (str "Undefined function: " op))))))

    ;; If expr is a constant, return it directly
                   :else expr)))

;; Sample Test Cases for Evaluating Expressions
  (println "Test Arithmetic: " (= (custom-eval '(* x y) {:x 10, :y 2}) 20))
  (println "Test Nested Expr: " (= (custom-eval '(+ x (* y 2)) {:x 5, :y 3}) 11))

;; Part 3: Declaring and Using Functions
;; Expanded custom-eval function can now handle function definitions and calls within the given environment.
  (def updated-env
    (custom-eval '(defn sum [a b] (+ a b)) initial-env))

  (println "Test Function Call: " (= (custom-eval '(sum x y) updated-env) 30))
  (println "Test Function Call with Literals: " (= (custom-eval '(sum 15 25) updated-env) 40))

;; Part 4: Handling Errors
;; Error handling for undefined variables or functions and type mismatches.
  (println "Error Handling: "
           (try
             (custom-eval '(sum x z) {:x 1 :y 2})
             (catch Exception e "Undefined variable or function")))
