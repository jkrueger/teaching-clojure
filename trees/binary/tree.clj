(ns tree
  (:use [clojure.test :only [testing is]]))

(defrecord Tree [value left right])

(defn make-leaf [v]
  (Tree. v nil nil))

(defn insert [tree value]
  (if (nil? tree)
    (make-leaf value)
    (if (< value (:value tree))
      (update tree :left (fn [x] (insert x value)))
      (update tree :right (fn [x] (insert x value))))))

(defn lookup [tree v]
  (if (not (nil? tree))
    (if (= v (:value tree))
      (:value tree)
      (if (< v (:value tree))
        (lookup (:left tree) v)
        (lookup (:right tree) v)))))

(defn binary-tree [s] 
  (reduce insert nil s))

(testing "Binary Trees"
  (testing "Inserting into binary tree"
    (is (= (insert nil 5) (Tree. 5 nil nil))) 
    (is (= (insert (make-leaf 5) 8) (Tree. 5 nil (Tree. 8 nil nil))))
    (is (= (insert (insert (make-leaf 5) 8) 8)
           (Tree. 5 nil (Tree. 8 nil (Tree. 8 nil nil))))))

  (testing "Finding values in a bariny tree"
    (is (= (lookup nil 5) nil))
    (is (= (lookup (Tree. 5 nil nil)) 5))
    (is (= (lookup (insert (make-leaf 5) 1) 1)))
    (is (= (lookup (insert (make-leaf 5) 10) 10)))
    (is (= (lookup (binary-tree [1 10 4 8 2 3 7]) 3) 3))
    (is (= (lookup (binary-tree [1 10 4 8 2 3 7]) 6) nil))))
