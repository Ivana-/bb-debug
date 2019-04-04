(ns bb-debug.example-requires
  (:require [bb-debug.core :refer [dbg dbg-all watch inspect]]))

(defn fact [n]
  (^{:cond (= 3 n) :name "factorial"} dbg)
  (if (<= n 0)
    1
    (* n (fact (- n 1)))))

(comment
  (fact 5)
  )
