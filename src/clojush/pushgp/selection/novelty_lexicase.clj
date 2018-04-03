(ns clojush.pushgp.selection.novelty-lexicase
  (:use [clojush random util globals])
  (:require [clojure.math.numeric-tower :as math]))

(defn novelty-difference
  "Counts how many individuals in the population have the SAME behavior"
  [behavior pop-behaviors-in-case]
  (count (filter #(= behavior %) pop-behaviors-in-case)))

(defn calculate-individual-novelty
  "Calculates the novelty of each "
  [ind novelty-archive pop]
  (let [behaviors (:behaviors ind)
        pop-behaviors (concat (map #(cond
                              (= (:behaviors %) nil) '(:nil) ;;had issue where when value was nil, case-behavior-vector was empty
                              :else (:behaviors %)) pop) (map :behaviors novelty-archive))
        case-behavior-vector (apply map list pop-behaviors)
        ]
    (assoc ind :lex-novelty (concat (map #(novelty-difference %1 %2) behaviors case-behavior-vector)(:errors ind)))))


(defn calculate-lex-novelty
  "Take a population of agents, derefs them, and calculates novelty of each
  individual (based on how many individuals have the same result for x test"
  [pop-agents novelty-archive {:keys [use-single-thread] :as argmap}]
  (dorun (map #((if use-single-thread swap! send) %
                calculate-individual-novelty
                novelty-archive (map deref pop-agents))
              pop-agents))
  (when-not use-single-thread (apply await pop-agents)))


(defn novelty-lex-selection
  "Returns an individual that does the best on the novelty cases when considered one at a
  time in random order."
  [pop argmap]
  (loop [survivors pop
         cases (lshuffle (range (count (:lex-novelty (first pop)))))]
    (if (or (empty? cases)
            (empty? (rest survivors)))
      (lrand-nth survivors)
      (let [min-err-for-case (apply min (map #(nth % (first cases))
                                             (map :lex-novelty survivors)))]
        (recur (filter #(= (nth (:lex-novelty %) (first cases)) min-err-for-case)
                       survivors)
               (rest cases))))))

;;(novelty-lex-selection '({:lex-novelty (5 1)}
  ;;                           {:lex-novelty (4 4)}
    ;;;                         {:lex-novelty (3 3)}
       ;;                      {:lex-novelty (2 2)}
         ;;                    {:lex-novelty (2 3)})
           ;;                {})
