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

;; filter examples
(defn my-filter [pred coll]
  (if (empty? coll)
    coll
    (if (pred (first coll))
      (cons (first coll) (my-filter pred (rest coll)))
      (my-filter pred (rest coll)))))

(=check (my-filter even? [1 2 3 4 5 6]) [2 4 6])
(=check (my-filter #(> (count %) 3) ["hi" "hello" "hey" "greetings"]) ["hello" "greetings"])
(=check (my-filter #(and (even? %) (> % 10)) [12 2 13 14 3]) [12 14])

;; concat examples
(defn my-concat [coll1 coll2]
  (if (empty? coll2)
    coll1
    (recur (conj coll1 (first coll2)) (rest coll2))))
(my-concat [1 2] [3 4])
(=check (my-concat [1 2] [3 4]) [1 2 3 4])
(=check (count (my-concat (range 5000) (range 5000 10000))) 10000)

;; nth examples
(defn my-nth [coll index]
  (if (= index 0)
    (first coll)
    (my-nth (rest coll) (- index 1))))
(=check (my-nth [10 20 30 40] 2) 30)
(=check (my-nth [1 2 3 4] 10) nil) ; Assuming nil for out of bounds
(=check (my-nth [1 2 3 4] 3) 4)

;; max/min examples
(defn my-max [coll]
  (if (empty? coll)
    nil
    (let [max-rest (my-max (rest coll))]
      (if (or (nil? max-rest) (> (first coll) max-rest))
        (first coll)
        max-rest))))
  
(defn my-min [coll]
  (if (empty? coll)
    nil
    (let [min-rest (my-min (rest coll))]
      (if (or (nil? min-rest) (< (first coll) min-rest))
        (first coll)
        min-rest))))
(=check (my-max [5 3 9 1]) 9)
(=check (my-min [5 3 9 1]) 1)
(=check (my-max [-5 -3 -9 -1]) -1)
(=check (my-min [-5 -3 -9 -1]) -9)
(=check (my-max []) nil)
(=check (my-min []) nil)

;; count examples
(defn my-count [coll]
  (if (empty? coll)
    0
    (let [index (my-count (rest coll))]
    (+ index 1)  )))
(=check (my-count [1 2 3 4 5]) 5)
(=check (my-count [[1 2] [3 4] [5]]) 3)
(=check (my-count []) 0)

;; take examples
(defn my-take [n coll]
  (if (empty? coll)
    coll
    (if (pos? n)
      (cons (first coll) (my-take (dec n) (rest coll))))))

(=check (my-take 3 [5 4 3 2 1]) [5 4 3])

;; merge examples
(defn my-merge [map1 map2]
  (if (empty? map1)
    map2
    (let [[key value] (first map2)
          merged-map (assoc map1 key value)]
      (recur merged-map (rest map2)))))
(=check (my-merge {:a 1 :b 2} {:b 3 :c 4}) {:a 1 :b 3 :c 4})
(=check (my-merge {:foo "bar"} {:foo "baz", :hello "world"}) {:foo "baz", :hello "world"})
(=check (my-merge {} {:a 1}) {:a 1})

;; group-by examples
(defn my-group-by [f coll]
  (letfn [(group-helper [coll result]
            (if (empty? coll)
              result
              (let [current (first coll)
                    key (f current)
                    grouped (get result key [])]
                (recur (rest coll) (assoc result key (conj grouped current))))))]
    (group-helper coll {})))
(=check (my-group-by :type [{:type :a :value 1} {:type :b :value 2} {:type :a :value 3}])
        {:a [{:type :a :value 1} {:type :a :value 3}], :b [{:type :b :value 2}]})
(=check (my-group-by even? [1 2 3 4 5 6]) {true [2 4 6], false [1 3 5]})
(=check (my-group-by count ["one" "two" "three" "four"]) {3 ["one" "two"], 5 ["three"], 4 ["four"]})

;; keys examples
(defn my-keys [map]
  (letfn [(keys-helper [remaining-keys result]
            (if (empty? remaining-keys)
              result
              (let [current-key (first remaining-keys)]
                (recur (rest remaining-keys) (conj result current-key)))))]
    (keys-helper (keys map) [])))
(=check (my-keys {:a 1 :b 2 :c 3}) [:a :b :c])
(=check (my-keys {:foo "bar" :baz "qux"}) [:foo :baz])
(=check (my-keys {}) [])

;; vals examples
(defn my-vals [map]
  (letfn [(vals-helper [remaining-keys result]
            (if (empty? remaining-keys)
              result
              (let [current-key (first remaining-keys)]
                (recur (rest remaining-keys) (conj result (get map current-key))))))]
    (vals-helper (keys map) [])))
(=check (my-vals {:a 1 :b 2 :c 3}) [1 2 3])
(=check (my-vals {:foo "bar" :baz "qux"}) ["bar" "qux"])
(=check (my-vals {}) [])

;; select-keys examples
(defn my-select-keys [map keys]
  (loop [remaining-keys keys
         result {}]
    (if (empty? remaining-keys)
      result
      (let [current-key (first remaining-keys)
            current-value (get map current-key)]
        (recur (rest remaining-keys)
               (if current-value
                 (assoc result current-key current-value)
                 result))))))
(=check (my-select-keys {:a 1 :b 2 :c 3} [:a :c]) {:a 1 :c 3})
(=check (my-select-keys {:name "Alice" :age 30 :gender "Female"} [:name :age]) {:name "Alice", :age 30})
(=check (my-select-keys {:foo "bar" :hello "world"} [:foo]) {:foo "bar"})

(println "Test results:" @result)
