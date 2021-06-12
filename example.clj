(ns bb-debug.example
  (:require [bb-debug.core :refer [dbg dbg-all dbg-all* watch last-context]]
            [bb-debug.inspect :refer [inspect]]))

;; (clojure-version) needs 1.9 or later cause of bounded count

;; in terminal:
;; lein repl
;; (require 'bb-debug.example)
;; (in-ns 'bb-debug.example)
;; or
;; (use '[bb-debug.core :refer [dbg dbg-all dbg-all* watch last-context]])


(comment
  "fprog-spb stream"

  ;; -------------------------------------------------------------------------------------------------------
  ;; single break-point, local context REPL, local bindings frame
  ;; -------------------------------------------------------------------------------------------------------


  (dbg)
  (dbg 1)

  (let [x 33] (dbg x))

  (let [x (range)] (dbg take 5 x))

  ((fn [x] ((fn [y] (dbg + x y)) 2)) 1)


  ;; -------------------------------------------------------------------------------------------------------
  ;; evaluations in local context REPL
  ;; -------------------------------------------------------------------------------------------------------


  (let [p {:a {:b 1 :c 2}}
        e (reduce (fn [acc i] (assoc acc i (str "val " i))) {} (range 10))]
    (dbg [p]))

  ;; eval in repl in local context of debugging form above
  (doall (map prn [(-> p :a) (-> p :a :c)]))
  {:p0 p :p1 p :pa (-> p :a) :pac (-> p :a :c)}


  (let [x (atom 33)] (dbg x))
  (swap! x inc)


  ;; -------------------------------------------------------------------------------------------------------
  ;; conditional / named breakpoints, break
  ;; -------------------------------------------------------------------------------------------------------


  (defn fact [n]
    (dbg ;; ^{:cond (= 3 n) :name "body"}
     if (<= n 0)
     1
     (* n (fact (- n 1)))))

  (defn fact [n]
    (dbg
     if (dbg <= n 0)
     1
     (* n (fact (dbg - n 1)))))

  (defn fact [n]
    (dbg ^{:name "body"}
     if (dbg ^{:name "if cond"} <= n 0)
         1
         (* n (fact (dbg ^{:name "arg dec"} - n 1)))))

  (fact 5)
  ;; break


  ;; -------------------------------------------------------------------------------------------------------
  ;; watcher / inspector
  ;; -------------------------------------------------------------------------------------------------------

  (defn fact [n]
    (^{:cond (= 3 n) :name "factorial"} dbg)
    (if (<= n 0)
      1
      (* n (fact (- n 1)))))

  (defn fact [n] (dbg if (<= n 0) 1 (* n (fact (- n 1)))))

  (fact 5)

  (watch [n (* 2 n)])

  (inspect [n (range)])

  (inspect {:a '(1 2 3)
            :b 33
            :c [1 [2 3] 4 5]
            :d [(range 8) (range 15) 3]
            :e {:r (range)}
            :f (range)
            :h "abc"})

  (+ n (let [n 33] n))


  ;; -------------------------------------------------------------------------------------------------------
  ;; Level 2
  ;; -------------------------------------------------------------------------------------------------------


  (fact 5)
  (fact 5)
  (let [x (atom 33)] (dbg x))


  ;; -------------------------------------------------------------------------------------------------------
  ;; tmp variables in local context
  ;; -------------------------------------------------------------------------------------------------------


  (let [{:keys [a b]} {:a 1 :b 2}
        [x y z] [3 4 5]]
    (dbg [a b x y z]))


  (map #(dbg * 3 %) (range 5))

  (for [i (range 5)] (dbg * 3 i))

  (watch i)


  ;; -------------------------------------------------------------------------------------------------------
  ;; multiple break-point - step-by-step debugging
  ;; -------------------------------------------------------------------------------------------------------


  (dbg-all*                   + 1 (let [x (+ 1 2)] (+ 3 x)))
  (dbg-all*                  (+ 1 (let [x (+ 1 2)] (+ 3 x))))
  (dbg-all* ^{:cond (= 3 b)}  + 1 (let [x (+ 1 2)] (+ 3 x)))
  (dbg-all* ^{:cond (= 3 b)} (+ 1 (let [x (+ 1 2)] (+ 3 x))))

  ;; why macroexpand-all before?

  (-> [x 1 y 2]
      (let (+ x y)))

  ((-> [x y]
       (fn (+ x y))) 3 4)

  (dbg-all* cond
            (> 10 20) (+ 10 20)
            (> 20 10) (- 20 10)
            :else 200)

  (dbg-all* (let [x 1] (inc x)))

  (dbg-all* -> [10 11]
            (conj 12)
            (as-> xs (map - xs [3 2 1]))
            (reverse))

  (dbg-all* let [req {:host "//mysite.com" :path "/a/123" :x "15.1" :y "84.2"}]
            (as-> req {:keys [host path x y] :as m}
              (assoc m :url (str host path))
              (assoc m :coord [(Double/valueOf x) (Double/valueOf y)])))

  (dbg-all* as-> {:a 1 :b 2} m
            (update m :a + 10)
            (reduce (fn [s [_ v]] (+ s v)) 0 m))


  ;; -------------------------------------------------------------------------------------------------------
  ;; horror and darkness inside a dog !!!
  ;; -------------------------------------------------------------------------------------------------------


  ;; question - how much breakpoints will it have?
  (dbg-all doseq [i [1 2 3]] (prn i))

  (^{:cond (= 2 i)} dbg-all doseq [i [1 2 3]] (prn i))

  (doseq [i [1 2 3]] (dbg prn i))

  (doseq [i [1 2 3]] (^{:cond (= 2 i)} dbg prn i))


  (def v [1 2])

  ;; question - how much breakpoints will it have?
  (dbg-all for [i v, j v, k v] [i j k])

  (watch {:i i :j j :k k})

  (dbg-all ^{:cond (= 2 i j k)} for [i v, j v, k v] [i j k])

  (for [i v, j v, k v] (dbg [i j k]))


  ;; -------------------------------------------------------------------------------------------------------
  ;; last-context - breakpoint on exception
  ;; -------------------------------------------------------------------------------------------------------


  (defn f [x]
    (dbg-all ^{:cond false}
     cond
             (> 1 2) 3
             (= "123" (subs x 0 3)) (+ 4 5 6)
             :else 22))

  (f 2)
  (f "12")
  (f "12345")
  (last-context)

  ;;
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(comment

  (me dbg + 1 2)
  (me dbg (+ 1 2))

  (me ^{:cond (= 1 a)} dbg ^{:name "b"} (+ 1 2))
  (me ^{:cond (= 1 a) :name "b"} dbg    (+ 1 2))
  (me dbg ^{:cond (= 1 a) :name "b"}    (+ 1 2))
  (me ^{:cond (= 1 a)} dbg ^{:name "b"}  + 1 2)
  (me ^{:cond (= 1 a) :name "b"} dbg     + 1 2)
  (me dbg ^{:cond (= 1 a) :name "b"}     + 1 2)

  ;;
  )
