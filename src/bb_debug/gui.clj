(ns bb-debug.gui
  (:import [javax.swing JFrame JTree JScrollPane SwingUtilities]
           [java.awt Font]
           [javax.swing.tree DefaultMutableTreeNode TreeSelectionModel]))


(defn jframe-maker [title]
  (doto (JFrame. (str title))
    (.setDefaultCloseOperation JFrame/DISPOSE_ON_CLOSE)
    (.setSize 400 250)
    ;; (.setLocationByPlatform true)
    (.setAlwaysOnTop true)))

(defonce locals-jframe (jframe-maker "Local bindings"))

(defn open-locals-jframe  [] (when-not (.isVisible locals-jframe) (.setVisible locals-jframe true)))
(defn close-locals-jframe [] (when     (.isVisible locals-jframe) (.setVisible locals-jframe false)))

(defonce max-seq-size 10)

(defn- show [x]
  (cond
    (some #(instance? % x) [clojure.lang.ChunkedCons]) (str "#object[" (.. x getClass getName) "]")
    (map-entry? x) (str (show (key x)) " " (show (val x)))
    (vector? x)    (str "["  (count x) "]")
    (map? x)       (str "{"  (count x) "}")
    (set? x)       (str "#{" (count x) "}")
    (list? x)      (str "("  (count x) ")")
    ;; (counted? x)   (str "("  (count x) ")")
    (coll? x)      (str "(" (let [bc (bounded-count (inc max-seq-size) x)]
                              (if (> bc max-seq-size)
                                (str ">" max-seq-size)
                                bc)) ")")
    (fn? x) "#function"
    (instance? java.lang.Exception x) #_"#error" (str (.. x getClass getSimpleName) " " (.getMessage x))
    :else (pr-str x)))

(defn unexpandedMutableTreeNode [data]
  ;; proxy to split unexpanded node type and allow .toString of lazySeqs
  (proxy [DefaultMutableTreeNode] [data]
    (toString [] "#unexpanded")))

(comment
  (def a (DefaultMutableTreeNode. "1"))
  (def b (unexpandedMutableTreeNode "2"))

  (isa? DefaultMutableTreeNode (class a))
  (isa? DefaultMutableTreeNode (class b))
  
  (instance? clojure.lang.IProxy a)
  (instance? clojure.lang.IProxy b)

  (instance? DefaultMutableTreeNode a)
  (instance? DefaultMutableTreeNode b))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; https://gist.github.com/mecdemort/345693/e293c74ec36c2672eb4d3d001e3fc6d130364e7a

(defn unshowable-coll? [coll] (some #(instance? % coll) [clojure.lang.ChunkedCons]))

(defn create-nodes [root coll]
  (when-not (unshowable-coll? coll)
    (let [add-node-and-children (fn [node v]
                                  (.add root node)
                                  (when (and (coll? v) (not (unshowable-coll? v)) (not (empty? v)))
                                    (.add node (unexpandedMutableTreeNode v))))]
      (cond
        (map? coll)
        (doseq [[k v] coll :let [node (DefaultMutableTreeNode. (str (show k) " " (show v)))]]
          (add-node-and-children node v))
      ;; IllegalArgumentException Don't know how to create ISeq from: clojure.lang.ArrayChunk  clojure.lang.RT.seqFrom (RT.java:550)
      ;; (counted? coll)
        (some #(% coll) [list? vector? set?])
        (doseq [v coll :let [node (DefaultMutableTreeNode. (show v))]]
          (add-node-and-children node v))

        (coll? coll)
        (do
          (doseq [v (take max-seq-size coll) :let [node (DefaultMutableTreeNode. (show v))]]
            (add-node-and-children node v))
          (when (> (bounded-count (inc max-seq-size) coll) max-seq-size)
            (add-node-and-children (DefaultMutableTreeNode. "...") (drop max-seq-size coll))))))))

(defn add-listeners [tree]
  (doto tree
    (.addTreeWillExpandListener
     (proxy [javax.swing.event.TreeWillExpandListener] []
       (treeWillExpand [event]
         (SwingUtilities/invokeLater
          #(let [node (.. event getPath getLastPathComponent)
                 first-child (.. node (getChildAt 0))]
             ;; check and expand unexpandedMutableTreeNode
             (when-not (isa? DefaultMutableTreeNode (class first-child))
             ;; (when (instance? clojure.lang.IProxy first-child)
               (.removeAllChildren node)
               (create-nodes node (.getUserObject first-child))
               (.. tree getModel (nodeStructureChanged node))
              ;  (println (str "first time expanded "
              ;                (->> event .getPath .getPath (map (fn [x] (.getUserObject x))) pr-str)
              ;                " - " (.getChildCount node) " children"))
               ))))
       (treeWillCollapse [event])))))

(defn obj->jtree [obj]
  (when-not (nil? obj)
    (let [root (DefaultMutableTreeNode. (show obj))]
      (create-nodes root obj)
      (doto (JTree. root)
        (.setFont (Font. "Monospaced" Font/PLAIN 18))
        (add-listeners)))))

(defn refresh-locals-jframe [{:keys [data title]}]
  (SwingUtilities/invokeLater
   #(doto locals-jframe
      (.setContentPane (JScrollPane. (obj->jtree data)))
      (.setTitle title)
      (.setVisible true))))

(defn show-on-jframe [jframe data]
  (SwingUtilities/invokeLater
   #(doto jframe
      (.setContentPane (JScrollPane. (obj->jtree data)))
      (.setVisible true))))


(comment

  (let [x {:a '(1 2 3)
           :b 33
           :c [1 [2 3] 4 5]
           :d [(range 8) (range 15) 3]
           :e {:r (range)}
           :f (range)}]
    (refresh-locals-jframe {:data x :title "zazazazaza"}))
  
  (refresh-locals-jframe {:data nil})
  
  (refresh-locals-jframe {:data (range)})
  ;;
  )
