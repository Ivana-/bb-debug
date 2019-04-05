(ns bb-debug.core
  (:import [java.awt.event WindowAdapter])
  (:require [bb-debug.gui :as gui]
            [clojure.main :as main]
            [clojure.walk :as walk]
            [clojure.pprint :as pprint]
            [clojure.string :as str]))

;; ----------------------------- atoms / dynamics
  
(defonce ^:dynamic *debug-context* {:repl-level 0})

(defonce watch-frames (atom nil))

;; ----------------------------- prepare / eval-in-local-context
  
(defn pre-eval [v]
  (list 'let (reduce (fn [acc [k v]] (conj acc
                                           (symbol k) ;; v 
                                           (list 'get-in 'bb-debug.core/*debug-context* [:locals k])))
                     [] (:locals *debug-context*)) v))

(defn eval-in-local-context! [v] (->> v pre-eval eval))
(defn eval-in-local-context [v] (try (eval-in-local-context! v) (catch Exception e e)))

;; ----------------------------- utils
  
(defmacro locals-map [] (into {} (for [[sym val] &env] [(name sym) sym])))
;; (defmacro locals-vec [] (reduce (fn [acc [sym val]] (conj acc (list 'quote sym) sym)) [] &env))

(defn substr-left [s* len] (let [s (str s*)] (if (> (count s) len) (str (subs s 0 (- len 3)) "...") s)))

(defn list-or-single-item [x] (if (and (seq? x) (empty? (rest x))) (first x) x))

(defn esc [n] (str (char 27) "[" n "m"))

;; ----------------------------- gui frames

(defn refresh-frames []
  (gui/refresh-locals-jframe
   {:title (-> *debug-context* :break-point-name str (str/replace #"\s+" " ") (substr-left 100))
    :data (reduce (fn [acc [k v]] (assoc acc (symbol k) v)) {} (:locals *debug-context*))})
  ;;  (gui/open-locals-jframe)
  (doall (map (fn [{:keys [raw-form value]}] (reset! value (eval-in-local-context raw-form))) (vals @watch-frames))))

(defn clear-frames []
  (gui/refresh-locals-jframe nil)
  ;; (gui/close-locals-jframe)
  (doall (map (fn [{value :value}] (reset! value nil)) (vals @watch-frames))))

;; ----------------------------- custom repl functions
  
(defn repl-caught [e]
  (if (instance? java.lang.InterruptedException e)
    (do
      (clear-frames)
      (throw e))
    (main/repl-caught e)))

(defn repl-read [request-prompt request-exit]
  (or ({:line-start request-exit ;; request-prompt 
        :stream-end request-exit} (main/skip-whitespace *in*))
      (let [input (try (read {:read-cond :allow} *in*)
                       (catch Exception e (do
                                            (repl-caught e)
                                            request-prompt)))]
        (main/skip-if-eol *in*)
        (case input
          ;; :repl/quit request-exit
          break (throw (InterruptedException. "Debug process was stoped by user"))
          input))))

;; (defn repl-prompt [] (printf "my\n%s=>" (ns-name *ns*)))
(defn repl-prompt []
  (print (str (let [x (:repl-level *debug-context*)] (when (> x 1) (str " LEVEL " x))) "   => ")))

(defn repl-eval [v] (let [r (eval-in-local-context! v)] (refresh-frames) r))

;; ----------------------------- break-point info / eval condition

(defn remove-all-break-points [v]
  (cond
    (seq? v) (let [r (map remove-all-break-points v)]
               (case (first r)
                 bb-debug.core/break-point (drop 2 r)
                 ;; break-point (drop 2 r)
                 r))
    (map? v)  (reduce (fn [acc [k v]] (assoc acc (remove-all-break-points k) (remove-all-break-points v))) {} v)
    (coll? v) (into (empty v) (map remove-all-break-points v))
    :else v))

; (defn all-syms-binded? [v]
;   (cond
;     (symbol? v) (or (boolean (resolve v)) ((-> *debug-context* :locals keys set) (name v)))
;     (map? v)  (and (all-syms-binded? (keys v)) (all-syms-binded? (vals v)))
;     (coll? v) (every? all-syms-binded? v)
;     :else true))

(defn get-condition [v] (or (:cond v) (:when v)))

(defn eval-condition [v] (try (eval-in-local-context! v) (catch Exception e false)))

;; (format (str "%-" 80 "s") prompt)
(defn make-break-point-name-condition [[params & x] {:keys [line column]}]
  (let [pp-form #(-> %
                     pprint/pprint
                     with-out-str
                     ;; (str/replace #"\s+" " ")
                     str/trim)]
    {:break-point-condition (when-let [x (get-condition params)] (pp-form x))
     :break-point-name (or (:name params) (-> x list-or-single-item remove-all-break-points pp-form))}))

(defn print-break-point-info []
  (when-let [x (:break-point-condition *debug-context*)]  (println (str (esc 33) x (esc 0))))
  (let [x (or (:break-point-name *debug-context*) "???")] (println (str (esc 32) x (esc 0)))))

;; ----------------------------- break-point !!!

(defmacro break-point [params & body]
  `(do
     (binding [*debug-context* (merge {:locals (locals-map)
                                       :repl-level (inc (:repl-level *debug-context*))}
                                      (make-break-point-name-condition '~(rest &form) ~(meta &form)))]
       (when ~(if-let [c (get-condition params)] `(eval-condition '~c) true)
         (refresh-frames)
         (print-break-point-info)
         (main/repl :eval repl-eval
                    :prompt repl-prompt
                    :read repl-read
                    :caught repl-caught)
         (clear-frames)))
     ~(list-or-single-item body)))

;; ----------------------------- group break-points

(defn add-all-break-points [ps v]
  (let [go
        (fn go [v]
          (cond
            (seq? v) (let [[e1 & e1-] v
                           [e2 & es] e1-]
                       (case e1
                         (def set! quote) (conj (go es) e2 e1)
                         (let* loop*) (conj (drop 2 (go es))
                                            (->> e2 (partition 2) (mapcat (fn [[a b]] [a (go b)])) vec)
                                            e1)
                         (fn*) (cons e1 (cond
                                          (symbol? e2) (cons e2 (drop 1 (go (cons e1 es)))) ;; named fn* !!!
                                          (vector? e2) (cons e2 (drop 2 (go es))) ;; one case fn*
                                          :else (map (fn [[h & t]] (cons h (drop 2 (go t)))) e1-))) ;; multicase fn*
                         (conj (map go v) ps 'bb-debug.core/break-point)))
            (map? v)  (list 'bb-debug.core/break-point ps (reduce (fn [acc [k v]] (assoc acc (go k) (go v))) {} v))
            (coll? v) (list 'bb-debug.core/break-point ps (into (empty v) (map go v)))
            :else v))]
    (go v)))

; (defmacro debug-all-core [ps & x]
;   `(binding [pprint/*print-suppress-namespaces* true]
;      ~(->> x list-or-single-item
;            walk/macroexpand-all
;            (add-all-break-points ps))))

(defmacro debug-all-core [ps & x] (->> x list-or-single-item walk/macroexpand-all (add-all-break-points ps)))

;; ----------------------------- pretty-print all form break-points

(defn color-all-break-points [s word color]
  (str/replace s
               (re-pattern (str "\\((\\s*)" word "(\\s)"))
               (str "($1" (esc color) word (esc 0) "$2")))

(defmacro pprint-expanded-form-with-all-break-points [x]
  `(binding [pprint/*print-right-margin* 100
             pprint/*print-suppress-namespaces* true]
     (-> ~x
         pprint/pprint
         with-out-str
         (color-all-break-points "break-point" 45) ;; 43 103 (+60)
         (color-all-break-points "bb-debug.core/break-point" 45)
         ;; str/trim
         print ;; ln
         )))

(defmacro debug-all-show [ps & x]
  `(->> '~x list-or-single-item
        walk/macroexpand-all
        (add-all-break-points '~ps)
        pprint-expanded-form-with-all-break-points))

;; ----------------------------- watch
  
(defn watch-core [title raw-form]
  (let [jframe (doto (gui/jframe-maker (substr-left title 100))
                 (.addWindowListener (proxy [WindowAdapter] []
                                       (windowClosing [evt]
                                         (swap! watch-frames dissoc (.. evt getWindow hashCode)))))
                   ;; (.setVisible true)
                 )
        value (atom nil)]
    (add-watch value :show-on-frame (fn [k a old new] (gui/show-on-jframe jframe new)))
    (reset! value (eval-in-local-context raw-form))
    (swap! watch-frames assoc (.hashCode jframe) {:jframe jframe :raw-form raw-form :value value})
    nil))

(defmacro watch
  ([s x] `(watch-core  ~s '~x))
  ([x]   `(watch-core '~x '~x)))

;; ----------------------------- inspect
  
(defn inspect-core [title data]
  (-> (gui/jframe-maker (substr-left title 100))
      (gui/show-on-jframe data)))

(defmacro inspect
  ([s x] `(inspect-core  ~s ~x))
  ([x]   `(inspect-core '~x ~x)))

;; ----------------------------- main export user debug macroses

(defn merge-meta-params [form body]
  (select-keys (merge (meta (first form)) (meta (first body))) [:cond :when :name]))

(defmacro dbg      [& body] `(break-point     ~(merge-meta-params &form body) ~@body))
(defmacro dbg-all  [& body] `(debug-all-core  ~(merge-meta-params &form body) ~@body))
(defmacro dbg-all* [& body] `(debug-all-show  ~(merge-meta-params &form body) ~@body))

; (defmacro dbg-common [head-form body] `(~head-form ~(merge-meta-params &form ~body) ~@body))

; (defmacro dbg      [& body] `(dbg-common break-point ~body))
; (defmacro dbg-all  [& body] `(dbg-common debug-all-core ~body))
; (defmacro dbg-all* [& body] `(dbg-common debug-all-show ~body))


;; ----------------------------- comments

(comment

  )
