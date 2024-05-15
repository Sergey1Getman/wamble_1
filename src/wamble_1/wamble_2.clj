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
