(ns clojush.pushgp.selection.selection
  (:use [clojush globals random]
        [clojush.pushgp.selection preselection tournament lexicase epsilon-lexicase
         elitegroup-lexicase random-threshold-lexicase novelty novelty-lexicase novelty-lexicase-dist]))

(defn select
  "Returns a selected parent."
  [pop {:keys [parent-selection print-selection-counts] :as argmap}]
  (let [pop-with-meta-errors (map #(update-in % [:errors] (comp vec concat) (:meta-errors %)) pop)
        preselected (preselect pop-with-meta-errors argmap)
        selected (case parent-selection
                   :tournament (tournament-selection preselected argmap)
                   :lexicase (lexicase-selection preselected argmap)
                   :epsilon-lexicase (epsilon-lexicase-selection preselected argmap)
                   :elitegroup-lexicase (elitegroup-lexicase-selection preselected argmap)
                   :random-threshold-lexicase (random-threshold-lexicase-selection 
                                                preselected argmap)
                   :leaky-lexicase (if (< (lrand) (:lexicase-leakage argmap))
                                     (lrand-nth preselected)
                                     (lexicase-selection preselected argmap))
                   :novelty-search (novelty-tournament-selection preselected argmap)
                   :uniform (lrand-nth preselected)
                   :novelty-lexicase (novelty-lex-selection preselected argmap)  ;;LJ added option
                   :novelty-dist (novelty-dist-selection preselected argmap)     ;;LJ added option
                   (throw (Exception. (str "Unrecognized argument for parent-selection: "
                                           parent-selection))))]
    (when print-selection-counts
      (swap! selection-counts 
             update-in 
             [(:uuid selected)] 
             (fn [sel-count] (if (nil? sel-count) 1 (inc sel-count)))))
    selected))

