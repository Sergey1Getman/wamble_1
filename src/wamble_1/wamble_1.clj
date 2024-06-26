(ns wamble-1.wamble-1)


(def result (atom {:correct 0 :incorrect 0}))
(defmacro =check [left right]
  `(let [left# ~left
         right# ~right]
     (if (= right# left#)
       (swap! result update :correct inc)
       (do
         (println "missmatch:" ~(str (first left)) left# right#)
         (swap! result update :incorrect inc)))
     left#))


;; reduce examples
(defn my-reduce [f init coll]
  (if (empty? coll)
    init
    (recur f (f init (first coll)) (rest coll))))
(reduce + 0 [1 2 3 4 ])
(my-reduce + 0 [1 2 3 4])
(=check (my-reduce + 0 [1 2 3 4]) 10)
(=check (my-reduce str "" ["a" "b" "c"]) "abc")
(=check (my-reduce + 0 (range 10000)) 49995000)

(defn another-filter [fun coll]
  (reduce (fn [result item]
            (if (fun item)
              (conj result item)
              result))[] coll))
;; filter examples
(defn reduce-filter [f coll] 
  (reduce (fn [result item]
            (if (f item)
              (conj result item)
              result)) [] coll))
(reduce-filter even? [1 2 3 4 5 6])
(=check (another-filter even? [1 2 3 4 5 6]) [2 4 6])

(=check (reduce-filter #(> (count %) 3) ["hi" "hello" "hey" "greetings"]) ["hello" "greetings"])
(=check (reduce-filter #(and (even? %) (> % 10)) [12 2 13 14 3]) [12 14])

;; concat examples
(defn reduce-concat [coll1 coll2]
  (reduce conj coll1 coll2)) 
(reduce-concat [1 2] [3 4])
(=check (reduce-concat[1 2] [3 4]) [1 2 3 4])
(=check (count (reduce-concat (range 5000) (range 5000 10000))) 10000)

;; nth examples
(defn my-nth [coll n]
  (if (= n 0)
    (first coll)
    (recur (rest coll) (dec n))))
(defn reduce-nth [coll n]);???????
(=check (my-nth [10 20 30 40] 2) 30)
(=check (my-nth [1 2 3 4] 10) nil) ; Assuming nil for out of bounds
(=check (my-nth [1 2 3 4] 3) 4)
;; max/min examples
  (defn reduce_max [coll]
    (if (empty? coll)
      nil
      (reduce (fn [x y]
                (if (> x y) x y)) coll)))
 (defn reduce_min [coll]
  (if (empty? coll)
    nil
    (reduce (fn [x y]
              (if (< x y) x y)) coll)))

(=check (reduce_max [5 3 9 1]) 9)
(=check (reduce_min[5 3 9 1]) 1)
(=check (reduce_min [-5 -3 -9 -1]) -9)
(=check (reduce_min []) nil)
(=check (reduce_max []) nil)



;; take examples

(defn reduce-take [n coll]
  (if (<= n 0)
    []
    (reduce (fn [acc item]
              (if (< (count acc) n)
                (conj acc item)
                acc))
            []
            coll)))
(=check (reduce-take 3 [5 4 3 2 1]) [5 4 3])

(defn let-merge[map1 map2]
  (if (empty? map2)
    map1
    (let [[k v] (first map2)
          {:keys (first-key)} (first map2)]
      (let-merge (assoc map1 first-key v) (rest map2)))))

;; merge examples

(defn my-merge [map1 map2]
  (if (empty? map1)
    map2
    (reduce (fn [result-map [key value]]
              (assoc result-map key value)) map1 map2)))
(=check (let-merge {:a 1 :b 2} {:b 3 :c 4}) {:a 1 :b 3 :c 4})
(=check (let-merge {:foo "bar"} {:foo "baz", :hello "world"}) {:foo "baz", :hello "world"})
(=check (let-merge {} {:a 1}) {:a 1})

;; group-by examples
(defn my-group-by [f coll]
  (reduce (fn [acc elem]
            (let [k (f elem)] 
              (assoc acc k (conj (get acc k []) elem))))
          {}
          coll))
(=check (my-group-by :type [{:type :a :value 1} {:type :b :value 2} {:type :a :value 3}])
        {:a [{:type :a :value 1} {:type :a :value 3}], :b [{:type :b :value 2}]})
(=check (my-group-by even? [1 2 3 4 5 6]) {true [2 4 6], false [1 3 5]})
(=check (my-group-by count ["one" "two" "three" "four"]) {3 ["one" "two"], 5 ["three"], 4 ["four"]})

;; keys examples

(defn my-keys [map]
  (reduce (fn [result-map [keys]]
            (conj result-map keys)) [] map))
(my-keys {:a 1 :b 2 :c 3})
(=check (my-keys {:a 1 :b 2 :c 3}) [:a :b :c])
(=check (my-keys {:foo "bar" :baz "qux"}) [:foo :baz])
(=check (my-keys {}) [])
;; vals examples
(defn my-vals [map]
(reduce (fn [result-map [values]]
          (conj result-map values))  [] map))
(=check (my-vals {:a 1 :b 2 :c 3}) [1 2 3])
(=check (my-vals {:foo "bar" :baz "qux"}) ["bar" "qux"])
(=check (my-vals {}) [])

(defn my-map [fun coll]
  (if (empty? coll)
    coll
    (cons (fun (first coll)) (my-map fun (rest coll)))))
(=check (my-map inc [10 20 30])[11 21 31])

[+ max min] 
(apply max [1 2 3 5])

select-keys
(defn my-select-keys [map keys])
(=check (my-select-keys {:a 1 :b 2 :c 3} [:a :c]) {:a 1 :c 3})
(=check (my-select-keys {:name "Alice" :age 30 :gender "Female"} [:name :age]) {:name "Alice", :age 30})
(=check (my-select-keys {:foo "bar" :hello "world"} [:foo]) {:foo "bar"})
next
flatten
(defn my-flatten [args]
  (if (sequential? args)
    (mapcat my-flatten args)
    [args]))
(my-flatten [1 2 3 4])
(defn isUpperCase [string]
  (if (empty? string)
    nil
    (str string
         (if (Character/isUpperCase string)
           (print "ok")))))
(isUpperCase ["HeLLo WoRLd"]) 
(println "Test results:" @result)
