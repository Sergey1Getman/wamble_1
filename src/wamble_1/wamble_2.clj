(ns wamble_2)
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

(defn my-juxt [& funs]
  (fn [& args]
    (loop [fs funs
           results []]
      (if (empty? fs)
        results
        (recur (rest fs) (conj results (apply (first fs) args)))))))

(=check ((my-juxt + max min) 2 3 5 1 6 4)[])
(=check ((my-juxt #(.toUpperCase %) count) "hello") ["HELLO" 5])
(=check ((my-juxt :a :b :c) {:a 2, :b 4, :c 6, :d 8 :e 10})[2 4 ])


(defn my-comp [& funs]
  (fn [& args]
    (loop [fs (rest (reverse funs))
           result (apply (last funs) args)]
      (if (seq fs)
        (recur (rest fs) ((first fs) result))
        result))))
(=check ((my-comp rest reverse) [1 2 3 4]) [3 2 1])
(=check ((my-comp (partial + 3) second) [1 2 3 4]) 5)
(=check ((my-comp  zero? #(mod % 8) +) 3 5 7 9)[true])
(=check ((my-comp #(.toUpperCase %) #(apply str %) take) 5 "hello world")["HELLO"])
(defn my-merge-with
  [f & maps]
  (reduce (fn [acc m]
            (reduce (fn [result [k v]]
                      (update result k f v))
                    acc
                    m))
          (first maps)
          (rest maps)))
(=check (my-merge-with * {:a 2, :b 3, :c 4} {:a 2} {:b 2} {:c 5})
        {:a 4, :b 6, :c 20})
(=check (my-merge-with concat {:a [3], :b [6]} {:a [4 5], :c [8 9]} {:b [7]}) 
        {:a [3 4 5], :b [6 7], :c [8 9]})
(defn decurry [fns]
  (fn [& args]
    (loop [f fns a args]
      (if (not (fn? (f (first a)))) (f (first a))
          (recur (f (first a)) (rest a))))))
(=check ((decurry (fn [a]
                   (fn [b]
                     (fn [c]
                       (fn [d]
                         (* a b c d)))))) 1 2 3 4)[24])
(=check ((decurry (fn [a]
                    (fn [b]
                      (fn [c]
                        (fn [d]
                          (+ a b c d)))))) 1 2 3 4) [10])
(=check ((decurry (fn [a]
                    (fn [b]
                      (* a b))))5 5)[25])


(def all-patients
  '({:firstname "Adam"
     :lastname  "Smith"
     :diagnosis "COVID-19"
     :treated   true}
    {:firstname "Joseph"
     :lastname  "Goodman"
     :diagnosis "COVID-19"
     :treated   true}
    {:firstname "Werner"
     :lastname  "Ziegler"
     :diagnosis "COVID-19"
     :treated   false}
    {:firstname "Boris"
     :lastname  "Henry"
     :diagnosis "Healthy"
     :treated   false}
    {:firstname "Johnny"
     :lastname  "Grayhand"
     :diagnosis "COVID-76"
     :treated   false})
  )
(for [patient all-patients]
  (:diagnosis patient))
(for [patient all-patients]
  (:lastname patient))
(for [patient all-patients]
  (:treated patient))
(for [patient all-patients]
  (:diagnosis patient))
(defmacro my-factor-group [all-patients]
  (let [treated? :treated
        disease-name :diagnosis]
    (if (empty? all-patients)
      1
      (for [patient all-patients]
         (println " начало обработки группы пациентов с диагнозом " disease-name
                 (if treated? ", подвергавшихся лечению"
                     ", НЕ подвергавшихся лечению"))
        ))))

(defmacro factor-group [all-patients]
  `(let [grouped-patients# (group-by #(vector (:diagnosis %) (:treated %)) ~all-patients)]
     (if (empty? ~all-patients)
       (println "No patients to process.")
       (doseq [[key# patients-group#] grouped-patients#]
         (let [disease-name# (first key#)
               treated?# (second key#)
               surnames# (map :lastname patients-group#)]
           (println "Начало обработки группы пациентов с диагнозом" disease-name#
                    (if treated?# ", подвергавшихся лечению"
                        ", НЕ подвергавшихся лечению"))
           (println "Количество пациентов в группе - " (count surnames#))
           (println "Фамилии пациентов - " (clojure.string/join ", " surnames#)
                     ))))))

(factor-group all-patients)
