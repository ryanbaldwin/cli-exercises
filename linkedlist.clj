(ns ward.linkedlist)

(defprotocol Node
  (value [this]
         "Gets the value of this node.")
  (nxt [this] 
       "Gets the next node in the linked list.")
  
  (has-next? [this]
            "Returns true if this node has a next, otherwise false.")
  
  (push [this val]
        "Pushes a new node to the head of the list.")
  
  (append [this val]
          "Appends a new node to the end of the list." )
  
  (traverse [this]
            "Prints the value of this node and then traverses to the next, until the end."))

(defrecord SingleLinkedList [val n]
  Node
  (value [this] (:val this))
  
  (nxt [this]
       (:n this))
  
  (has-next? [this]
            (not (nil? (nxt this))))
  
  (append [this val]    
          (if (has-next? this)
            (assoc this :n (append (nxt this) val))
            (assoc this :n (SingleLinkedList. val nil))))
  
  (push [this val]
        (SingleLinkedList. val this))
  
  (traverse [this]            
            #_(loop [n this result []]
              (if-not (has-next? n)
                (conj result [(value n)])
                (recur (nxt n) (conj [result] (value n)))))
            (if (has-next? this)
              [(value this) (traverse (nxt this))]
              [(value this)])))

(defn seed [n]    
  (if (> n 0)
    (loop [i (dec n) result (SingleLinkedList. n nil)]      
      (if (= i 0)
        result
        (recur (dec i) (push result i))))))