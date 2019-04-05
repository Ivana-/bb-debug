(ns bb-debug.example
  (:require [bb-debug.core :refer [dbg dbg-all dbg-all* watch inspect]]
            [bb-debug.example-requires :as reqs]
            [bb-debug.test :refer :all]
            [clojure.string :as str]))

(comment

  (clojure-version)
  
  ;; (use '[bb-debug.core :refer [dbg dbg-all dbg-all* watch inspect]])
  
  (alter-var-root #'clojure.pprint/*print-suppress-namespaces* (constantly true))

  ;; -------------------------------------------------------------------------------------------------------
  ;; single break-point
  ;; -------------------------------------------------------------------------------------------------------
  
  (me dbg + 1 2)
  (me dbg (+ 1 2))

  (me ^{:cond (= 1 a)} dbg ^{:name "b"} (+ 1 2))
  (me ^{:cond (= 1 a) :name "b"} dbg    (+ 1 2))
  (me dbg ^{:cond (= 1 a) :name "b"}    (+ 1 2))
  (me ^{:cond (= 1 a)} dbg ^{:name "b"}  + 1 2)
  (me ^{:cond (= 1 a) :name "b"} dbg     + 1 2)
  (me dbg ^{:cond (= 1 a) :name "b"}     + 1 2)


  (dbg 1)
  (dbg)

  ((fn [x] ((fn [y] (dbg + x y)) 2)) 1)

  (let [x 33] (dbg x))

  (reqs/fact 5)

  ((fn [z w q]
     (dbg let [p {:a {:b 1 :c 2}}
               x (reqs/fact (+ 1 z))]
          (dbg [x p])))
   2 4 6)

  (let [p {:a {:b 1 :c 2}}
        e (reduce (fn [acc i] (assoc acc i (str "val " i))) {} (range 1000))]
    (dbg [p]))

  ;; eval in repl in local context of debugging form above
  (doall (map prn [(-> p :a) (-> p :a :c)]))
  {:p0 p :p1 p :pa (-> p :a) :pac (-> p :a :c)}


  (let [x (atom 33)] (dbg x))
  (swap! x inc)


  ;; -------------------------------------------------------------------------------------------------------
  ;; watcher / inspector
  ;; -------------------------------------------------------------------------------------------------------
  
  (defn fact [n]
    (dbg if (<= n 0)
         1
         (* n (fact (- n 1)))))

  (fact 10)

  (watch [n (* 2 n)])

  (+ n (let [n 33] n))


  (for [i (range 10)] (dbg * 3 i))

  (watch i)

  (inspect {:a '(1 2 3)
            :b 33
            :c [1 [2 3] 4 5]
            :d [(range 8) (range 15) 3]
            :e {:r (range)}
            :f (range)})


  ;; -------------------------------------------------------------------------------------------------------
  ;; multiple break-point
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


  (dbg-all* -> [10 11]
            (conj 12)
            (as-> xs (map - xs [3 2 1]))
            (reverse))
  (dbg-all (-> [10 11]
               (conj 12)
               (as-> xs (map - xs [3 2 1]))
               (reverse)))

  (dbg-all let [req {:host "//mysite.com" :path "/a/123" :x "15.1" :y "84.2"}]
           (as-> req {:keys [host path x y] :as m}
             (assoc m :url (str host path))
             (assoc m :coord [(Double/valueOf x) (Double/valueOf y)])))

  (dbg-all as-> {:a 1 :b 2} m
           (update m :a + 10)
           (reduce (fn [s [_ v]] (+ s v)) 0 m))


  (dbg-all doseq [i [1 2 3]] (prn i))
  (^{:cond (= 2 i)} dbg-all doseq [i [1 2 3]] (prn i))
  (doseq [i [1 2 3]] (dbg prn i))
  (doseq [i [1 2 3]] (^{:cond (= 2 i)} dbg prn i))

  (dbg-all ^{:cond (= 2 i j k n)} for [i [1 2] j [1 2] k [1 2] n [1 2]] [i j k n])
  (dbg-all ^{:name "_"} for [i [1 2] j [1 2] k [1 2] n [1 2]] [i j k n])
  (for [i [1 2] j [1 2] k [1 2] n [1 2]] (dbg ^{:name "final"} [i j k n]))
  (watch {:i i :j j :k k :n n})
  (dbg-all* for [i [(atom 1) 2] j [1 2] k [1 2]] [i j k])
;;
  )  
