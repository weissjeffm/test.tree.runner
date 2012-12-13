(ns test.tree.runner
  (:require clojure.pprint
            [test.tree :refer [state]]
            [test.tree.debug :as debug]
            [fn.trace :as trace]
            [clojure.pprint :refer [pprint *print-right-margin*]])
  (:use seesaw.core
        seesaw.chooser)
  (:import [javax.swing.tree DefaultMutableTreeNode]
           [javax.swing JTree]
           [javax.swing.tree DefaultTreeModel]
           [javax.swing.table DefaultTableModel]
           [javax.swing.tree TreePath]
           [javax.swing.tree DefaultTreeCellRenderer]
           [java.text SimpleDateFormat]
           [java.lang.System]
           [javax.swing ImageIcon]
           [javax.swing.filechooser FileSystemView]))

(def main-win-width 900)
(def main-win-height 600)
(def win-title "Interactive Test Runner")
(def config-rsrc "test/tree/icons/config-small.png") 
(def test-rsrc "test/tree/icons/test-small.png") 

(def prog-bar (progress-bar :value 0))
(def ^:dynamic test-map nil)
(def test-info-model (DefaultTableModel. (to-array ["Property" "Value"]) 6))
(def test-tree-root (DefaultMutableTreeNode. "Test Tree")) 
(def test-tree-model (DefaultTreeModel. test-tree-root))
(def test-tree (JTree. test-tree-model)) 
(def output-tree-root (DefaultMutableTreeNode. "Test Results")) 
(def output-tree-model (DefaultTreeModel. output-tree-root))
(def output-tree (JTree. output-tree-model))
(def output-tree-scroll (scrollable output-tree :hscroll :as-needed :vscroll :always)) 
(def output-tree-lock (atom nil))

(def running-test nil)
(def test-results-ref (atom nil))
(def in-repl? true)
(def need-save? false)
(def trace-trees (atom {}))
(def is-running? (atom false))


(defn getPathFromNode [node]
  (if-let [parent-node (.getParent node)]
    (.pathByAddingChild (getPathFromNode parent-node) node)
    (TreePath. node)))


(defn get-test-entry-from-path [test-map path path-idx]
  (let [next-path-idx (inc path-idx)]
    (if (<= (.getPathCount path) next-path-idx)
      test-map
      (let [cur-node (.getPathComponent path path-idx) 
            child-node (.getPathComponent path next-path-idx)
            index (.getIndex cur-node child-node)
            child-test-map (nth (:more test-map) (.getIndex cur-node child-node))]
        (if (<= (dec (.getPathCount path)) next-path-idx)
          child-test-map
          (get-test-entry-from-path child-test-map path next-path-idx))))))


(def test-tree-renderer 
  (proxy [DefaultTreeCellRenderer] []
    (getTreeCellRendererComponent [tree value selected? expanded? leaf? row hasFocus?]
      (let [this        (proxy-super getTreeCellRendererComponent tree value selected? expanded? leaf? row hasFocus?)
            test-map    (get-test-entry-from-path test-map (getPathFromNode value) 1)]
        (if (= value test-tree-root)
          (.setIcon this nil)
          
          (.setIcon this (-> (ClassLoader/getSystemClassLoader) 
                             (.getResource (if (:configuration test-map) config-rsrc test-rsrc)) 
                             ImageIcon.))) 
        this))))


(def output-tree-renderer 
  (proxy [DefaultTreeCellRenderer] []
    (getTreeCellRendererComponent [tree value selected? expanded? leaf? row hasFocus?]
      (let [this (proxy-super getTreeCellRendererComponent tree value selected? expanded? leaf? row hasFocus?)]
        (locking output-tree-lock
          (.setOpaque this false)
          (let [child-enum (.children value)]
            (while (.hasMoreElements child-enum) 
              (let [child (.nextElement child-enum)
                    child-str (str child)
                    status (->> child-str (re-find #"Status: (\w+)") second keyword)
                    result (->> child-str (re-find #"Result: (\w+)") second keyword)]
                (when (keyword? result)
                  (.setOpaque this true)
                  (.setBackground this (cond (= result :pass) java.awt.Color/green
                                             (= result :skip) java.awt.Color/yellow
                                             (= result :fail) java.awt.Color/red)))
                (when (keyword? status)
                  (.setOpaque this true)
                  (cond (= status :queued)  (.setBackground this java.awt.Color/lightGray)
                        (= status :running) (.setBackground this java.awt.Color/magenta))))))
          this)))))

(def trace-tree-renderer
  (proxy [DefaultTreeCellRenderer] []
    (getTreeCellRendererComponent [tree value selected? expanded? leaf? row hasFocus?]
      (let [this (proxy-super getTreeCellRendererComponent tree value selected? expanded? leaf? row hasFocus?)
            rsrc (str "test/tree/icons/" (if leaf? "upload" "download") "-small.jpg")
            url (-> (ClassLoader/getSystemClassLoader) (.getResource rsrc))]
        (.setIcon this (ImageIcon. url))
        this))))


(defn add-test-groups [test-group tree-node]
  (let [group-name (first (:groups test-group))
        new-node (DefaultMutableTreeNode. (str (:name test-group)))]
    (.insertNodeInto output-tree-model
                     new-node
                     tree-node,
                     (.getChildCount tree-node))
    (when (contains? test-group :more)
      (doseq [child-group (:more test-group)]
        (add-test-groups child-group new-node)))))


(defn selected-item-changed [event-info]
  (let [path (.getPath event-info)
        sel-test (get-test-entry-from-path test-map path 1)
        panel-print (fn [& output]
                      (binding [*print-right-margin* 60]
                        (with-out-str 
                          (apply pprint output))))
        fields [:file :line :groups :parameters :blockers :steps]]
    (doseq [index (-> fields count range)]
      (.setValueAt test-info-model (panel-print ((nth fields index) sel-test)) index 1))))


(def dateformat (SimpleDateFormat. "MM/dd/yyyy hh:mm:ss"))


(defn get-date-string [d]
  (when-not (nil? d)
    (.format dateformat d)))


(defn add-report-node [parent-node key-str val-str]
  (def node (atom nil))
  (let [node-str (str key-str ": " (if (keyword val-str) (name val-str) val-str))]
    ; Search through children for existing node
    (when (.getChildCount parent-node)
      (let [child-enum (.children parent-node)]
        (while (and (not @node) (.hasMoreElements child-enum))
          (let [child-node (.nextElement child-enum)
                child-str  (str child-node)]
            (when (.startsWith child-str (str key-str ": ")) 
              (when (not= child-str node-str) 
                (.valueForPathChanged output-tree-model 
                                      (TreePath. child-node)
                                      node-str)
                (when (.startsWith child-str "Status: ") 
                  (.reload output-tree-model parent-node)))
              (reset! node child-node)))))) 
    (when-not @node
      (.insertNodeInto output-tree-model 
                       (DefaultMutableTreeNode. node-str) 
                       parent-node 0))))


(defn update-output-node [report-group report test-group output-node]
  (let [results (:report report)]
    (cond 
      (= test-group report-group)
      (do
        (add-report-node output-node "Stack Trace" (-> results :error :throwable))
        (add-report-node output-node "Promise" (:promise report)) 
        (add-report-node output-node "Return Value" (:returned results)) 
        (add-report-node output-node "End Time" (get-date-string (:end-time results))) 
        (add-report-node output-node "Start Time" (get-date-string (:start-time results))) 
        (add-report-node output-node "Result" (:result results))
        (add-report-node output-node "Status" (:status report)) 
        (add-report-node output-node "Parameters" (:parameters results))
        true)
      (contains? test-group :more)
      (let [child-enum (.children output-node)
            recur-res  (for [child-group (:more test-group)] 
                         (update-output-node report-group
                                             report
                                             child-group 
                                             (.nextElement child-enum)))] 
        (some true? recur-res)))))


(defn set-summary-info [summary-node 
                        num-passed 
                        num-failed 
                        num-skipped
                        num-total]
  (let [node-texts (map #(apply str %) [["Passed: "  num-passed]
                                        ["Failed: "  num-failed] 
                                        ["Skipped: " num-skipped]
                                        ["Total: "   num-total]])]
    (doseq [index (-> node-texts count range)
            :let [node-text (nth node-texts index)]]
      (if (< index (.getChildCount summary-node))
        (.valueForPathChanged output-tree-model
                              (-> summary-node (.getChildAt index) TreePath.) 
                              node-text)
        (.insertNodeInto output-tree-model 
                         (DefaultMutableTreeNode. node-text)
                         summary-node 0)))))


(defn update-trace-tree [trace-tree test-map]
  (let [test-report (-> @test-results-ref second deref (get test-map) (get :report))
        trace-tree-model (.getModel trace-tree)]
    (when (contains? test-report :error)
      (loop [trace-list     (-> test-report
                                (get :error) 
                                (get :trace)
                                .trace-list)
             parent-node    (.getRoot trace-tree-model)
             node-idx-stack [0]]
        (when-first [trace-item trace-list]
          (let [child-count (.getChildCount parent-node) 
                node-label  (-> trace-item first pprint with-out-str)
                is-ret-val? (last trace-item)
                match-nodes (for [node-idx (-> node-idx-stack last (range child-count))
                                  :let [node (.getChildAt parent-node node-idx)] 
                                  :when (= (.getUserObject node) node-label)] node-idx)
                found-match? (not (empty? match-nodes))
                node-idx     (if found-match? (first match-nodes) child-count)
                child-node   (if found-match? (.getChildAt parent-node node-idx) 
                                              (DefaultMutableTreeNode. node-label))
                rest-trace-list (rest trace-list)]
            (when-not found-match? (.insertNodeInto trace-tree-model
                                                    child-node
                                                    parent-node
                                                    child-count))
            (let [node-idx-stack (drop-last node-idx-stack)]
              (if is-ret-val?
                (recur rest-trace-list (.getParent parent-node) node-idx-stack)
                (recur rest-trace-list child-node (conj (vec node-idx-stack) (inc node-idx) 0))))))))
        (.expandRow trace-tree 0)))


(defn view-trace-click [sel-path]
  (let [test-map         (get-test-entry-from-path running-test sel-path 1)
        trace-tree-root  (DefaultMutableTreeNode. "Trace")
        trace-tree-model (DefaultTreeModel. trace-tree-root)
        trace-tree       (JTree. trace-tree-model)
        trace-tree-panel (scrollable trace-tree 
                                     :hscroll :as-needed 
                                     :vscroll :always)]
    (update-trace-tree trace-tree test-map)
    (swap! trace-trees conj {test-map trace-tree})
    (.setCellRenderer trace-tree trace-tree-renderer)
    ; Expand all nodes in trace tree
    (loop [row 0
           row-count (.getRowCount trace-tree)]
      (.expandRow trace-tree row)
      (when (< row row-count)
        (recur (inc row) (.getRowCount trace-tree))))
    ; Create trace window
    (with-widgets [(frame :id :trace-frame
                          :title (str "Test Trace: " (:name test-map))
                          :content trace-tree-panel
                          :size [600 :by 600])]
      (show! trace-frame))))


(defn load-test-output [test-output]
  (locking output-tree-lock
    (let [[test-total 
           test-done 
           test-pass 
           test-fail 
           test-skip 
           test-unk] (take 6 (repeatedly #(atom 0)))
          summary-node (.getFirstChild output-tree-root)]
      (doseq [report-key (keys test-output)
              :let [report-val (get test-output report-key)
                    result (-> report-val :report :result)]]
          (when (= (:status report-val) :done) (swap! test-done inc))
          (swap! test-total inc)
          (swap!  (cond (= result :pass) test-pass
                        (= result :fail) test-fail
                        (= result :skip) test-skip
                        :else            test-unk)
                  inc)
          (update-output-node report-key 
                              report-val
                              running-test 
                              (.getNextSibling summary-node)))
      (.setMaximum prog-bar @test-total)
      (.setValue prog-bar @test-done)
      (set-summary-info summary-node
                        @test-pass
                        @test-fail
                        @test-skip
                        @test-total))))


(defn refresh-test-output [watch-key watch-ref old-state new-state]
  (load-test-output new-state)
  (doseq [test-map (keys @trace-trees)]
    (update-trace-tree (get @trace-trees test-map) test-map))
  (def need-save? true))


(defn reset-output-tree [sel-test]
  (.removeAllChildren output-tree-root)
  (let [summary-node (DefaultMutableTreeNode. "Summary")]
    (set-summary-info summary-node 0 0 0 0)
    (.insertNodeInto output-tree-model
                     summary-node
                     output-tree-root 0))
  (add-test-groups sel-test output-tree-root)
  (.nodeStructureChanged output-tree-model output-tree-root))


(defn run-test-click [test-tree]
  ;; TODO:) Check need-save?
  (if @is-running?
    (alert "Wait for current run to complete before running more tests.")
    
    (let [sel-path (.getSelectionPath test-tree)
          sel-test (get-test-entry-from-path test-map sel-path 1)]
  
      (def running-test sel-test)
      (reset-output-tree sel-test)
  
      (.setStringPainted prog-bar true)
      (doseq [node-index (-> 3 range reverse)] 
        (.expandRow output-tree node-index))
  
      ;; Start test run
      (reset! is-running? true)
      (future 
        (debug/debug 
         (with-meta sel-test
           (update-in (meta sel-test)
                      [:watchers]
                      assoc :test-runner-watch refresh-test-output))
         (katello.conf/trace-list) 
         test-results-ref)
        (alert "Test Run Complete")
        (.setValue prog-bar 0)
        (.setStringPainted prog-bar false)
        (reset! is-running? false)))))


(defn mouse-pressed [sender e]
  (when (= (.getButton e) 3)
    (let [x (.getX e)
          y (.getY e)
          menu-items (cond 
                       (= sender test-tree) 
                         (list
                           (menu-item :text "Run Tests" 
                                      :id :run-menu-item 
                                      :listen [:action (fn [sender]
                                                 (run-test-click test-tree))]) 
                           (menu-item :text "Terminate Run" 
                                      :id :terminate-menu-item
                                      :listen [:action (fn [sender]
                                                         (when @is-running?
                                                           (test.tree/terminate-all-tests (first @test-results-ref))
                                                           (reset! is-running? false)))]))
                       (= sender output-tree)
                         (list
                           (menu-item :text "View Trace"
                                      :id :view-trace-item
                                      :listen [:action (fn [_]
                                                         (view-trace-click (.getSelectionPath sender)))]))) ]
      (with-widgets [(popup :items menu-items
                            :id :popup-menu)]
        (->> (.getPathForLocation sender x y) (.setSelectionPath sender))
        (.show popup-menu sender x y)))))


(defn open-results [sender]
  (if @is-running?
    (alert "Wait for tests to complete before loading results.")
    (let [filename (choose-file (to-root sender)
                                :type :open
                                :filters [["Clojure Files" ["clj"]]])]
      (when filename
        (let [results (-> filename slurp read-string)
              test-map (first results)
              test-report (second results)]
          (reset! test-results-ref [(first results) (ref (second results))])
          (def running-test test-map)
          (reset-output-tree test-map)
          (load-test-output test-report))))))


(defn save-results [sender]
  (cond @is-running? (alert "Wait for tests to complete before saving results.")
        (nil? @test-results-ref) (alert "No results available.")
        :else (let [filename (choose-file (to-root sender)
                                          :type :save
                                          :filters [["Text Files" ["txt"]]])]
                (when filename
                  (binding [*out* (java.io.FileWriter. filename)] 
                           (prn (second @test-results-ref)))
                  (def need-save? false)))))


(defn load-test-tree [tree-map-symbol]
  (when-not (nil? tree-map-symbol)
    (.valueForPathChanged test-tree-model 
                          (TreePath. test-tree-root) 
                          tree-map-symbol)
    (.removeAllChildren test-tree-root)
    (try
      (def ^:dynamic test-map (-> tree-map-symbol read-string eval))
      (add-test-groups test-map test-tree-root)
      (.reload test-tree-model)
      (doseq [node-index (range 2)] (.expandRow test-tree node-index))
      (catch Exception e (alert (.getMessage e))))))


(defn load-test-tree-map [sender]
  (let [tree-map-symbol (input "Enter fully qualified test tree map symbol:"
                               :title "Load Test Tree Map" 
                               :type :question)]
    (load-test-tree tree-map-symbol)))


(defn reset-state []
  (reset! test-results-ref nil)
  (reset! trace-trees {})
  (.removeAllChildren test-tree-root)
  (.reload test-tree-model)
  (.removeAllChildren output-tree-root)
  (.reload output-tree-model))


(defn start-gui [& {:keys [test-tree-map]}]

  (reset-state)

  (.setCellRenderer test-tree test-tree-renderer)
  (.setCellRenderer output-tree output-tree-renderer) 

  ; Set up test info table
  (let [headers ["File:" "Line:" "Groups:" "Parameters:" "Blockers:" "Steps:"]]
    (doseq [index (-> headers count range)]
      (.setValueAt test-info-model (nth headers index) index 0)))

  (let [tree-scroll-pane (scrollable test-tree :hscroll :as-needed :vscroll :always)
        test-info-table (table :auto-resize :last-column :model test-info-model :show-grid? true :fills-viewport-height? true)
        info-scroll-pane (scrollable test-info-table :hscroll :as-needed :vscroll :as-needed)
        left-pane (top-bottom-split tree-scroll-pane info-scroll-pane :divider-location (/ (* main-win-height 5) 8))]

    (-> test-info-table (.getColumn "Property") (.setPreferredWidth 100))
    (-> test-info-table (.getColumn "Property") (.setMaxWidth 150))

    (with-widgets [(border-panel :id :right-pane
                                 :center output-tree-scroll
                                 :south  prog-bar) 
                   (left-right-split left-pane right-pane :id :main-panel :divider-location (/ main-win-width 3))
                   (menu-item :text "Open Results" 
                              :id :open-menu 
                              :listen [:action #(open-results %)])
                   (menu-item :text "Save Results" 
                              :id :save-menu 
                              :listen [:action #(save-results %)])
                   (menu-item :text "Load Test Tree"
                              :id :load-test-tree-menu
                              :listen [:action #(load-test-tree-map %)])
                   (menu-item :text "Reload Test Tree"
                              :id :reload-test-tree-menu
                              :listen [:action (fn [_] (load-test-tree (.getUserObject test-tree-root)))])
                   (menu-item :text "Exit" 
                              :id :exit-menu)
                   (menu      :text "File" 
                              :id :file-menu 
                              :items [open-menu 
                                      save-menu 
                                      (separator) 
                                      load-test-tree-menu 
                                      reload-test-tree-menu 
                                      (separator) 
                                      exit-menu])
                   (menubar   :id :main-menu-bar 
                              :items [file-menu])
                   (frame     :id :main-frame
                              :title win-title
                              :menubar main-menu-bar
                              :content main-panel
                              :size [main-win-width :by main-win-height]
                              :on-close (if in-repl? :dispose :exit))]

      (listen exit-menu :mouse-pressed 
        (fn [sender] (if in-repl? (.dispose main-frame) (. System exit 0))))
      (listen test-tree 
        :selection #(selected-item-changed %))
      (listen test-tree
        :mouse-pressed #(mouse-pressed test-tree %))
      (listen output-tree
        :mouse-pressed #(mouse-pressed output-tree %))

      (when-not (nil? test-tree-map) 
        (def ^:dynamic test-map (-> test-tree-map read-string eval))
        (load-test-tree))

      (show! main-frame))))
