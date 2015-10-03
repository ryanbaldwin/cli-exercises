(ns ward.index-of)

(defn index-of
  "Returns the first index where needle is found in haystack.
  Returns -1 if the needle is  not found in haystack."
  [needle haystack]  
  (loop [n needle h haystack index 0]
    (let [n-length (count n)
          h-length (count h)]
      (cond 
        (> n-length h-length) -1
        (= n (subs h 0 n-length)) index
        :else (recur n (subs h 1) (inc index))))))

;use java's random generator
(def random (java.util.Random.))
;define characters list to use to generate string
(def chars 
   (map char (concat (range 48 58) (range 66 92) (range 97 123))))
;generates 1 random character
(defn random-char [] 
  (nth chars (.nextInt random (count chars))))
; generates random string of length characters
(defn random-string [length]
  (apply str (take length (repeatedly random-char))))