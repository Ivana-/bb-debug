(ns bb-debug.inspect
  (:require [clojure.pprint :as pprint])
  (:import [javax.swing JFrame JScrollPane JTree JPanel JLabel SwingUtilities BorderFactory]
           [java.awt Font Color]
           [javax.swing.tree DefaultMutableTreeNode DefaultTreeCellRenderer]))

(def max-seq-size 10)

(def gui-settings
  {:colors {:lite-brown (Color. 156 93 39)
            :green (Color. 68 140 39)
            :lite-grey (Color. 200 200 200)
            :opaque-yellow (Color. 255 255 0 30)}
   :fonts {:tree (Font. "Monospaced" Font/PLAIN 18)}
   :borders {:empty-for-label (BorderFactory/createEmptyBorder 2 3 2 4)
             :underline-red   (BorderFactory/createMatteBorder 0 0 2 0 Color/red)
             :underline-empty (BorderFactory/createEmptyBorder 0 0 2 0)}})

(defn- pprint-safe [value]
  (-> value
      (pprint/write
       ;; :lines* - not yet supported, check later!
       :pretty true
       :stream true ;; (indicates *out*)
       ;;  :level 5
       :length max-seq-size
       ;;  :right-margin 100 ;; :miser-width - let be default values
       :dispatch pprint/code-dispatch
       :suppress-namespaces true)
      with-out-str))

(defrecord TreeNodeUserObjectType [component path value]
  java.lang.Object
  (toString [this] (str (when (seq path) (pprint-safe path))
                        "\n"
                        (pprint-safe value))))

(defn- ^JLabel label [^String text & [{:keys [color]}]]
  (let [^JLabel label (JLabel. text)]
    (when-let [color (get-in gui-settings [:colors color])]
      (.setForeground label color))
    (doto label
      (.setBorder (get-in gui-settings [:borders :empty-for-label]))
      (.setFont   (get-in gui-settings [:fonts :tree])))))

(defn- wrap-in-panel [& elements]
  (let [^JPanel panel (javax.swing.JPanel.)
        ^java.awt.FlowLayout panel-layout (.getLayout panel)]
    (doto panel-layout
      (.setVgap 0)
      (.setHgap 0))
    (doseq [^java.awt.Component e elements]
      (.add panel e))
    panel))

(defn- show [x]
  (cond
    (some #(instance? % x)
          [clojure.lang.ChunkedCons]) (label (str "#object[" (.. ^java.lang.Object x getClass getName) "]"))
    (map-entry? x)   (wrap-in-panel (show (key x)) (show (val x)))
    (vector? x)      (label (str "["  (count x) "]") {:color :lite-grey})
    (set? x)         (label (str "#{" (count x) "}") {:color :lite-grey})
    (list? x)        (label (str "("  (count x) ")") {:color :lite-grey})
    (associative? x) (label (str "{"  (count x) "}") {:color :lite-grey}) ;; map? or datomic entity
    ;; (counted? x)   (str "("  (count x) ")")
    (coll? x) (label (str "(" (let [bc (bounded-count (inc max-seq-size) x)]
                                (if (> bc max-seq-size)
                                  (str ">" max-seq-size)
                                  bc)) ")")
                     {:color :lite-grey})
    (fn? x) (label "#function")
    (instance? java.lang.Exception x) (label (str (.. ^java.lang.Object x getClass getSimpleName)
                                                  " " (.getMessage ^java.lang.Exception x)))
    (some #(% x) [nil? boolean? number? keyword?]) (label (pr-str x) {:color :lite-brown})
    (string? x) (label (pr-str x) {:color :green})
    :else (label (pr-str x))))

(defn- ->panel [x]
  (let [v (show x)]
    (if (instance? JPanel v)
      v
      (wrap-in-panel v))))

(defn- ^DefaultMutableTreeNode ->node [component path value]
  (DefaultMutableTreeNode. (TreeNodeUserObjectType. component path value)))

(defn- unexpandedMutableTreeNode [data]
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

(defn- unshowable-coll? [coll] (some #(instance? % coll) [clojure.lang.ChunkedCons]))

(defn- has-children? [v] (and (coll? v) (not (unshowable-coll? v)) (seq v)))

(defn- create-nodes [^DefaultMutableTreeNode root coll]
  (when-not (unshowable-coll? coll)
    (let [path (:path (.getUserObject root))
          add-node-and-children (fn [^DefaultMutableTreeNode node v]
                                  (.add root node)
                                  (when (and (coll? v) (not (unshowable-coll? v)) (seq v))
                                    (.add node (unexpandedMutableTreeNode v))))]
      (cond
        ;; IllegalArgumentException Don't know how to create ISeq from: clojure.lang.ArrayChunk  clojure.lang.RT.seqFrom (RT.java:550)
        ;; (counted? coll)
        ((some-fn list? vector? set?) coll)
        (doseq [[i v] (map-indexed vector coll)
                :let [node (->node (->panel v) (conj path i) v)]]
          (add-node-and-children node v))

        (associative? coll) ;; map? or datomic entity
        (doseq [[k v :as me] coll
                :let [node (->node (->panel me) (conj path k) v)]]
          ;; (add-node-and-children node v)
          (.add root node)
          (let [k-children? (has-children? k)
                v-children? (has-children? v)]
            (cond
              (and
               k-children?
               v-children?) (let [k-node (->node (wrap-in-panel (label "key" {:color :lite-grey}))
                                                 (conj path 'key) k)
                                  v-node (->node (wrap-in-panel (label "val" {:color :lite-grey}))
                                                 (conj path k) v)]
                              (.add node k-node)
                              (.add k-node (unexpandedMutableTreeNode k))
                              (.add node v-node)
                              (.add v-node (unexpandedMutableTreeNode v)))
              k-children? (.add node (unexpandedMutableTreeNode k))
              v-children? (.add node (unexpandedMutableTreeNode v)))))

        (coll? coll)
        (do
          (doseq [[i v] (map-indexed vector (take max-seq-size coll))
                  :let [node (->node (->panel v) (conj path i) v)]]
            (add-node-and-children node v))
          (when (> (bounded-count (inc max-seq-size) coll) max-seq-size)
            (let [v (drop max-seq-size coll)]
              (add-node-and-children (->node (wrap-in-panel (label "...")) path v) v))))))))

(defn- add-listeners [^JTree tree]
  (doto tree
    (.addTreeWillExpandListener
     (proxy [javax.swing.event.TreeWillExpandListener] []
       (treeWillExpand [^javax.swing.event.TreeExpansionEvent event]
         (SwingUtilities/invokeLater
          #(let [^DefaultMutableTreeNode node (.. event getPath getLastPathComponent)
                 first-child (.getChildAt node 0)]
             ;; check and expand unexpandedMutableTreeNode
             (when-not (isa? DefaultMutableTreeNode (class first-child))
               ;; dirty hack! Here first-child is a subclass of DefaultMutableTreeNode
               ;; i.e. NOT a DefaultMutableTreeNode (we checked it above)
               ;; but here we casting it to its superclass DefaultMutableTreeNode
               ;; for preventing reflection warning on .getUserObject on it
               (let [^DefaultMutableTreeNode first-child first-child]
                 (.removeAllChildren node)
                 (create-nodes node (.getUserObject first-child))
                 ;;  (.. tree getModel (nodeStructureChanged node))
                 (let [^javax.swing.tree.DefaultTreeModel tree-model (.getModel tree)]
                   (.nodeStructureChanged tree-model node))
                 ;  (println (str "first time expanded "
                 ;                (->> event .getPath .getPath (map (fn [x] (.getUserObject x))) pr-str)
                 ;                " - " (.getChildCount node) " children"))
                 )))))
       (treeWillCollapse [^javax.swing.event.TreeExpansionEvent event])))))

(defn- ^DefaultTreeCellRenderer myTreeCellRenderer []
  (proxy [DefaultTreeCellRenderer] []
    (getTreeCellRendererComponent [^JTree tree, ^DefaultMutableTreeNode value, sel, expanded, leaf, row, hasFocus]
      (let [value (.getUserObject value)
            ;; ^javax.swing.JComponent r (or (:component value) (JLabel. "???")) ;; check value type or isa?
            ^javax.swing.JComponent r (if (instance? TreeNodeUserObjectType value)
                                        (.component ^TreeNodeUserObjectType value)
                                        (JLabel. "???"))] ;; FIXME change to settings value
        (if (or leaf expanded)
          (.setOpaque r false)
          (do
            (.setOpaque r true)
            (.setBackground r (get-in gui-settings [:colors :opaque-yellow]))))
        (doto r
          (.setBorder (get-in gui-settings [:borders (if sel :underline-red :underline-empty)])))))))

(def my-tree-cell-renderer (myTreeCellRenderer))

(defn- obj->jtree [obj]
  (let [^DefaultMutableTreeNode root (->node (->panel obj) [] obj)]
    (create-nodes root obj)
    (doto (JTree. root)
      (.setCellRenderer my-tree-cell-renderer)
      (.setToggleClickCount 1)
      (.setShowsRootHandles true)
      (add-listeners))))

(defn- jframe-update-content [^JFrame jframe ^JTree jtree & [title]]
  (SwingUtilities/invokeLater
   (fn []
     (when title (.setTitle jframe title))
     (doto jframe
       (.setContentPane (JScrollPane. jtree))
       ;;  3 ways for update frame:
       ;;  1-st: frame catches focus and repl loss cursor
       ;;  (.setVisible true)
       ;;  2-nd: frame updates, but blinks gray/white
       ;;  (SwingUtilities/updateComponentTreeUI)
       ;;  3-rd: works fine!
       (.invalidate)
       (.validate)
       (.repaint)))))

;; ----------------------------- public api

(defn jframe-create [title]
  (doto (JFrame. (str title))
    (.setDefaultCloseOperation JFrame/DISPOSE_ON_CLOSE)
    (.setSize 400 300)
    (.setLocationByPlatform true)
    (.setAlwaysOnTop true)
    ;; (.setVisible true)
    ))

(defn jframe-show [^JFrame jframe data & [title]] (jframe-update-content jframe (obj->jtree data) title))

(defn jframe-clear [^JFrame jframe] (jframe-update-content jframe nil ""))

(defn jframe-switch-visibility [^JFrame jframe] (.setVisible jframe (not (.isVisible jframe))))

(defn jframe-open  [^JFrame jframe] (when-not (.isVisible jframe) (.setVisible jframe true)))

(defn jframe-close [^JFrame jframe] (when     (.isVisible jframe) (.setVisible jframe false)))

(defn inspect [data]
  (let [^JFrame jframe (jframe-create "")]
    (-> (doto jframe
          (.setVisible true))
        (jframe-show data))))
