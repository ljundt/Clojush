(ns clojush.pushgp.selection.novelty-lexicase
  (:use [clojush random util globals])
  (:require [clojure.math.numeric-tower :as math]))

(defn novelty-difference
  "Counts how many individuals in the population have the SAME behavior"
  [behavior pop-behaviors-in-case]
  (count (filter #(= behavior %) pop-behaviors-in-case)))

(defn calculate-individual-novelty
  "Calculates the novelty of each "
  [ind pop]
  (let [behaviors (:behaviors ind)
        pop-behaviors (map #(cond
                              (= (:behaviors %) nil) '(:nil) ;;had issue where when value was nil, case-behavior-vector was empty
                              :else (:behaviors %)) pop)
        case-behavior-vector (apply map list pop-behaviors)
        ]
    (assoc ind :lex-novelty (map (novelty-difference behaviors case-behavior-vector)))))


(defn calculate-lex-novelty
  "Take a population of agents, derefs them, and calculates novelty of each
  individual (based on how many individuals have the same result for x test"
  [pop-agents novelty-archive {:keys [use-single-thread] :as argmap}]
  (dorun (map #((if use-single-thread swap! send) %
                calculate-individual-novelty
                (map deref pop-agents))
              pop-agents))
  (when-not use-single-thread (apply await pop-agents)))


(defn novelty-lex-selection
  "Returns an individual that does the best on the novelty cases when considered one at a
  time in random order."
  [pop argmap]
  (loop [survivors pop
         cases (lshuffle (range (count (:novelty-lex (first pop)))))]
    (if (or (empty? cases)
            (empty? (rest survivors))
            (< (lrand) (:lexicase-slippage argmap)))
      (lrand-nth survivors)
      (let [min-err-for-case (apply min (map #(nth % (first cases))
                                             (map :lex-novelty survivors)))]
        (recur (filter #(= (nth (:lex-novelty %) (first cases)) min-err-for-case)
                       survivors)
               (rest cases))))))
