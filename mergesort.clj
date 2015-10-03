(ns ward.mergesort
  (:refer-clojure :exclude [merge]))

(defn merge
  "Merges two sorted lists into a single sorted list."
  [right left]
  (flatten (loop [r right, l left, results []]
             (let [rhead (first r)
                   lhead (first l)]
               (cond (nil? rhead) (conj results l)
                     (nil? lhead) (conj results r)
                     (< (compare lhead rhead) 1) (recur (rest l) r (conj results lhead))
                     :else (recur l (rest r) (conj results rhead)))))))

(defn mergesort
  "Performs a mergesort on any number of unordered lists, producing a single ordered list."
  ([items]   
   (if (<= (count items) 1)
     items
     (let [midpoint (/ (count items) 2)
           left (mergesort (take midpoint items))
           right (mergesort (drop midpoint items))]
       (merge left right))))
  
  ([items & more]   
   (let [merged-items (map mergesort (conj more items))]
           (reduce merge merged-items))))

(defn random 
  "Creates a list n-long of random integers of a value up to max"
  [n max]
  (take n (repeatedly #(rand-int max))))

(defn random-randomness [n]
  "Creates an n-long list of random lists. Random meta."
  (take n (repeatedly #(random (rand-int 100) 10000))))

(let [lists (random-randomness 200)]
  (prn "mergesorting " (count lists) "unordered lists")
  (let [results (time (apply mergesort lists))]
    (prn results)
    (prn (str "sorted " (count results) " total results"))))