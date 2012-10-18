(ns test.tree.runner
  (:require clojure.pprint
            [test.tree :refer [state]]
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
           [java.lang.System]))

(def win-width 900)
(def win-height 600)
(def win-title "Interactive Test Runner")

(def prog-bar (progress-bar :value 0))
(def ^:dynamic test-map nil)
(def test-info-model (DefaultTableModel. (to-array ["Property" "Value"]) 4))
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

(def output-tree-renderer 
  (proxy [DefaultTreeCellRenderer] []
    (getTreeCellRendererComponent [tree value selected? expanded? leaf? row hasFocus?]
      (let [this (proxy-super getTreeCellRendererComponent tree value selected? expanded? leaf? row hasFocus?)] 
        (locking output-tree-lock
          (.setOpaque this false) 
          (let [child-enum (.children value)]
            (def continue (atom true))
            (while (and (.hasMoreElements child-enum) @continue)
              (let [child (.nextElement child-enum)
                    child-str (str child)
                    status (->> child-str (re-find #"Status: (\w+)") second keyword)
                    result (->> child-str (re-find #"Result: (\w+)") second keyword)]
                (when (keyword? result)
                  (.setOpaque this true)
                  (.setBackground this (cond (= result :pass) java.awt.Color/green
                                             (= result :skip) java.awt.Color/yellow
                                             :else            java.awt.Color/red)))
                (when (keyword? status)
                  (.setOpaque this true)
                  (cond (= status :queued)  (.setBackground this java.awt.Color/lightGray)
                        (= status :running) (.setBackground this java.awt.Color/magenta))))))
          this)))))

(defn is-running? []
  (let [test-results @test-results-ref] 
    (if (nil? test-results)
        false
        (not= (state (first test-results) (second test-results)) :finished))))

(defn add-test-groups [test-group tree-node]
  (let [group-name (first (:groups test-group))
        new-node (DefaultMutableTreeNode. (str (:name test-group)))]

    (.add tree-node new-node)

    (when (contains? test-group :more)
      (doseq [child-group (:more test-group)]
        (add-test-groups child-group new-node)))))

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

(defn selected-item-changed [event-info]
  (let [path (.getPath event-info)
        sel-test (get-test-entry-from-path test-map path 1)
        panel-print (fn [& stuff]
                      (binding [*print-right-margin* 60]
                        (with-out-str 
                          (apply pprint stuff))))]
    (.setValueAt test-info-model (:groups sel-test) 0, 1)
    (.setValueAt test-info-model (panel-print (:parameters sel-test)) 1, 1)
    (.setValueAt test-info-model (panel-print (:blockers sel-test)) 2, 1)
    (.setValueAt test-info-model (panel-print (:steps sel-test)) 3, 1)))


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
                (.setUserObject child-node node-str)
                (.nodeChanged output-tree-model child-node)
                (when (.startsWith child-str "Status: ") 
                  (.nodeChanged output-tree-model parent-node)))
              (reset! node child-node))))))
    (when-not @node
      (.insertNodeInto output-tree-model (DefaultMutableTreeNode. node-str) parent-node 0))))


(defn update-output-node [report-group report test-group output-node]
  (when (contains? test-group :more)
    (let [child-enum (.children output-node)]
      (doseq [child-group (:more test-group)
              :while (not (update-output-node report-group
                                              report
                                              child-group 
                                              (.nextElement child-enum)))])))

  (let [results (:report report)]
    (if (= test-group report-group)
      (do
        (add-report-node output-node "Stack Trace" (-> results :error :throwable))
        (add-report-node output-node "Status" (:status report)) 
        (add-report-node output-node "Result" (:result results))
        (add-report-node output-node "Start Time" (get-date-string (:start-time results))) 
        (add-report-node output-node "End Time" (get-date-string (:end-time results))) 
        (add-report-node output-node "Parameters" (:parameters results)) 
        (add-report-node output-node "Return Value" (:returned results)) 
        (add-report-node output-node "Promise" (:promise report)) 
        true)
      false)))


(defn refresh-test-output [watch-key watch-ref old-state new-state]
  (locking output-tree-lock
    (let [test-total (atom 0)
          test-done  (atom 0)]
      (when (.getChildCount output-tree-root)
      (doseq [report-key (keys new-state)
              :let [report-val (get new-state report-key)]]
          (when (= (:status report-val) :done) (swap! test-done inc))
          (swap! test-total inc) 
          (update-output-node report-key 
                              report-val
                              running-test 
                              (.getFirstChild output-tree-root)))) 
      (.setMaximum prog-bar @test-total)
      (.setValue prog-bar @test-done)))
  (def need-save? true))


(defn run-test-click [test-tree]
  ; TODO:) Check need-save?
  (let [sel-path (.getSelectionPath test-tree)
        sel-test (get-test-entry-from-path test-map sel-path 1)]

    (def running-test sel-test)

    (.setStringPainted prog-bar true)
    (.removeAllChildren output-tree-root)
    (add-test-groups sel-test output-tree-root)
    (.nodeStructureChanged output-tree-model output-tree-root)
    (doseq [node-index [0 1]] (.expandRow output-tree node-index))

    ; Start test run
    (try 
      (reset! test-results-ref  
              (test.tree/run 
                (with-meta sel-test 
                           {:watchers {:test-runner-watch refresh-test-output}}))) 
      (catch Exception e (println (str "Exception: " (str e))))))

  ; Monitor for test completion
  (future 
    (while (is-running?) (Thread/sleep 500)) 
    (alert "Test Run Complete")
    (.setValue prog-bar 0)
    (.setStringPainted prog-bar false)))


(defn mouse-pressed [e]
  (when (= (.getButton e) 3)
    (let [x (.getX e)
          y (.getY e)]
      (with-widgets
          [(menu-item :text "Run Tests" 
                      :id :run-menu-item 
                      :listen [:action (fn [sender]
                                         (run-test-click test-tree))]) 
           (menu-item :text "Terminate Run" 
                      :id :terminate-menu-item
                      :listen [:action (fn [sender]
                                         (test.tree/terminate-all-tests (first @test-results-ref)))])
           (popup :items [run-menu-item terminate-menu-item] 
                  :id :popup-menu)]
        (let [running? (is-running?)]
          (config! run-menu-item :enabled? (not running?)) 
          (config! terminate-menu-item :enabled? running?)) 
        (->> (.getPathForLocation test-tree x y) (.setSelectionPath test-tree))
        (.show popup-menu test-tree x y)))))

(defn open-results [sender]
  ; TODO: Implementation
  )

(defn save-results [sender]
  (cond (is-running?)  (alert "Wait for tests to complete before saving results.")
        (nil? @test-results-ref) (alert "No results available.")
        :else (let [filename (choose-file (to-root sender)
                                          :type :save
                                          :filters [["Text Files" ["txt"]]])]
                (when (not (nil? filename))
                  (binding [*out* (java.io.FileWriter. filename)] 
                           (prn (second @test-results-ref))
                           )
                  (def need-save? false)))))


(defn load-test-tree []
  (.removeAllChildren test-tree-root)
  (add-test-groups test-map test-tree-root)
  (.reload test-tree-model)
  (doseq [node-index [0 1]] (.expandRow test-tree node-index)))


(defn load-test-tree-map [sender]
  (let [tree-map-symbol (input "Enter fully qualified test tree map symbol:"
                               :title "Load Test Tree Map" 
                               :type :question)]
    (when-not (nil? tree-map-symbol) 
      (try
        (def ^:dynamic test-map (-> tree-map-symbol read-string eval))
        (load-test-tree)  
        (catch Exception e (alert (.getMessage e)))))))


(defn reset-state []
  (reset! test-results-ref nil)
  (.removeAllChildren test-tree-root)
  (.reload test-tree-model)
  (.removeAllChildren output-tree-root)
  (.reload output-tree-model))


(defn start-gui [& {:keys [test-tree-map]}]

  (reset-state)

  (.setCellRenderer output-tree output-tree-renderer) 

  ; Set up test info table
  (.setValueAt test-info-model "Groups:" 0, 0)
  (.setValueAt test-info-model "Parameters:" 1, 0)
  (.setValueAt test-info-model "Blockers:" 2, 0)
  (.setValueAt test-info-model "Steps:" 3, 0)

  (let [tree-scroll-pane (scrollable test-tree :hscroll :as-needed :vscroll :always)
        test-info-table (table :auto-resize :last-column :model test-info-model :show-grid? true :fills-viewport-height? true)
        info-scroll-pane (scrollable test-info-table :hscroll :as-needed :vscroll :as-needed)
        left-pane (top-bottom-split tree-scroll-pane info-scroll-pane :divider-location (/ (* win-height 3) 4))]

    (-> test-info-table (.getColumn "Property") (.setPreferredWidth 100))
    (-> test-info-table (.getColumn "Property") (.setMaxWidth 150))

    (with-widgets [(border-panel :id :right-pane
                                 :center output-tree-scroll
                                 :south  prog-bar) 
                   (left-right-split left-pane right-pane :id :main-panel :divider-location (/ win-width 3))
                   (menu-item :text "Open Results" 
                              :id :open-menu 
                              :listen [:action #(open-results %)])
                   (menu-item :text "Save Results" 
                              :id :save-menu 
                              :listen [:action #(save-results %)])
                   (menu-item :text "Load Test Tree"
                              :id :load-test-tree-menu
                              :listen [:action #(load-test-tree-map %)])
                   (menu-item :text "Exit" 
                              :id :exit-menu)
                   (menu      :text "File" 
                              :id :file-menu 
                              :items [open-menu save-menu (separator) load-test-tree-menu (separator) exit-menu])
                   (menubar   :id :main-menu-bar 
                              :items [file-menu])
                   (frame     :id :main-frame
                              :title win-title
                              :menubar main-menu-bar
                              :content main-panel
                              :size [win-width :by win-height]
                              :on-close (if in-repl? :dispose :exit))]

      (listen exit-menu :mouse-pressed 
        (fn [sender] (if in-repl? (.dispose main-frame) (. System exit 0))))
      (listen test-tree 
        :selection #(selected-item-changed %))
      (listen test-tree
        :mouse-pressed #(mouse-pressed %))

      (when-not (nil? test-tree-map) 
        (def ^:dynamic test-map test-tree-map)
        (load-test-tree))

      (show! main-frame))))
