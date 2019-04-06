(ns bb-debug.test
  (:require [clojure.test :refer :all]
            [bb-debug.core :refer :all]
            [clojure.walk :as walk]))

;; ----------------------------- test utils

;; removing different forms
(defn remove-all-forms-with-heads [v head-forms-to-remove]
  (let [go
        (fn go [v]
          (cond
            (seq? v) (if (and
                          (contains? head-forms-to-remove (first v))
                          (seq? (second v)))
                       nil
                       (map go v))
            (map? v)  (reduce (fn [acc [k v]] (assoc acc (go k) (go v))) {} v)
            (coll? v) (into (empty v) (map go v))
            :else v))]
    (go v)))

(defmacro me [& x] `(-> '~x macroexpand          pprint-expanded-form-with-all-break-points))
(defmacro ma [& x] `(-> '~x walk/macroexpand-all pprint-expanded-form-with-all-break-points))

(defmacro me- [& x] `(-> '~x macroexpand (remove-all-forms-with-heads #{'quote})))
(defmacro ma- [& x] `(-> '~x walk/macroexpand-all (remove-all-forms-with-heads #{'quote})))

;; ----------------------------- tests

(deftest common-test

  (testing "macroexpand single break-point without parameters"
    (is (=
         (me- dbg + 1 2)
         (me- dbg (+ 1 2)))))

  (testing "macroexpand single break-point with parameters"
    (is (=
         (me- ^{:cond (= 1 a)} dbg ^{:name "b"} (+ 1 2))
         (me- ^{:cond (= 1 a) :name "b"} dbg    (+ 1 2))
         (me- dbg ^{:cond (= 1 a) :name "b"}    (+ 1 2))
         (me- ^{:cond (= 1 a)} dbg ^{:name "b"}  + 1 2)
         (me- ^{:cond (= 1 a) :name "b"} dbg     + 1 2)
         (me- dbg ^{:cond (= 1 a) :name "b"}     + 1 2))))

  (testing "macroexpand multiple break-point without parameters"
    (is (=
         (ma- dbg-all  + 1 (let [x (+ 1 2)] (+ 3 x)))
         (ma- dbg-all (+ 1 (let [x (+ 1 2)] (+ 3 x)))))))

  (testing "macroexpand multiple break-point with parameters"
    (is (=
         (ma- ^{:cond (= 1 a)} dbg-all ^{:name "b"} (+ 1 (let [x (+ 1 2)] (+ 3 x))))
         (ma- ^{:cond (= 1 a) :name "b"} dbg-all    (+ 1 (let [x (+ 1 2)] (+ 3 x))))
         (ma- dbg-all ^{:cond (= 1 a) :name "b"}    (+ 1 (let [x (+ 1 2)] (+ 3 x))))
         (ma- ^{:cond (= 1 a)} dbg-all ^{:name "b"}  + 1 (let [x (+ 1 2)] (+ 3 x)))
         (ma- ^{:cond (= 1 a) :name "b"} dbg-all     + 1 (let [x (+ 1 2)] (+ 3 x)))
         (ma- dbg-all ^{:cond (= 1 a) :name "b"}     + 1 (let [x (+ 1 2)] (+ 3 x))))))
;;
  )

(comment
  (run-tests)
  )
