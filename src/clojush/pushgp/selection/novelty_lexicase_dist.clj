(ns clojush.pushgp.selection.novelty-lexicase-dist
  (:use [clojush random util globals])
  (:require [clojure.math.numeric-tower :as math]))

(defn make-test-res-map
  "Makes a map of the different behaviors
  and how many there are in the population"
  [behaviors]
  (loop [behaviors behaviors res {}]
    (cond
      (empty? behaviors) res
      :else
      (let [curbehavior (first behaviors)]
        (cond
          (contains? res curbehavior) (recur (rest behaviors)
                                             (assoc res curbehavior
                                                    (inc (get res curbehavior))))
          :else (recur (rest behaviors) (assoc res curbehavior 1)))))))

(defn get-behavior-distance
  "Gets the difference between two behaviors
   based on their type"
  [b1 b2]
  ;(println b1)
  ;(println b2)
  (cond
    ; Handles equal behaviors, including if both are :no-stack-item
    (= b1 b2) 0
    ; If one has :no-stack-item and the other does not, give max difference
    (or (= b1 :no-stack-item)
        (= b2 :no-stack-item)) max-number-magnitude
    (or (= b1 [])
        (= b2 [])) (count (concat b1 b2))
    :else (case (recognize-literal b1)
            :string (levenshtein-distance b1 b2)
            (:integer :float) (math/abs (-' b1 b2))
            (:boolean :char) (if (= b1 b2) 0 1)
            (:vector_integer :vector_float :vector_string :vector_boolean) (hamming-distance b1 b2)
            (throw (Exception. (str "Unrecognized behavior type in novelty distance for: " b1))))))

(defn calculate-behavior-dist-map
  "Calculates a map storing the distances between any two behaviors, of the form:
    {behavior1 {behavior1 dist11 behavior2 dist12 behavior3 dist13 ...}}"
  [behavior all-behaviors]
  (let [behavior-dist-map {}]
    (map (fn [other-behavior]
           (vector other-behavior
                   (if (contains? behavior-dist-map other-behavior)
                     (get-in behavior-dist-map
                             [other-behavior behavior])
                     (get-behavior-distance behavior other-behavior)))) all-behaviors)))
  ;(loop [behavior behavior
  ;       pop-behaviors distinct-pop-and-archive-behaviors
  ;       behavior-distance-map {}]
  ;  (if (empty? pop-behaviors)
  ;    behavior-distance-map
  ;    (let [distances-from-behavior
  ;          (into {}
   ;               (map (fn [other-behavior]
   ;                      (vector other-behavior
   ;                              (if (contains? behavior-distance-map other-behavior)
    ;;                               (get-in behavior-distance-map
     ;                                      [other-behavior behavior])
      ;                             (do (println behavior)
       ;                            (println other-behavior)
        ;                           (get-behavior-distance behavior other-behavior)))))
         ;              distinct-pop-and-archive-behaviors))]
      ;  (recur behavior
       ;        (rest pop-behaviors)
        ;       (assoc behavior-distance-map behavior distances-from-behavior))))))


(defn calculate-behavior-sparseness
  "Calculates the sparseness/novelty of an individual by averaging together the
   distances between it and its k nearest neighbors. First, it must look up those
   distances using the behavior-distance-map."
  [pop-and-archive-behaviors behavior-distance-map {:keys [novelty-number-of-neighbors-k]}]
  (let [behavior-distances-to-others
        (into {}
              (for [[behavior dist-map] behavior-distance-map]
                (vector behavior
                        (map dist-map pop-and-archive-behaviors))))]
    (into {}
          (for [[behavior distances] behavior-distances-to-others]
            (vector behavior
                    (/ (apply +'
                              (take novelty-number-of-neighbors-k
                                    (sort distances)))
                       novelty-number-of-neighbors-k))))))

(defn ind-novelty
  [behavior case-behaviors argmap]
  (let [behavior-map (calculate-behavior-dist-map behavior case-behaviors)]
    (calculate-behavior-sparseness case-behaviors behavior-map argmap)))

(defn calculate-individual-novelty
  "Calculates the novelty of each individual"
  [ind novelty-archive pop argmap]
  (println ind)
  (println novelty-archive)
  (println pop)
  
  (let [behaviors (:behaviors ind)
        pop-behaviors (concat (map #(cond
                              (= (:behaviors %) nil) '(:nil) ;;had issue where when value was nil, case-behavior-vector was empty
                              :else (:behaviors %)) pop) (map :behaviors novelty-archive))
        case-behavior-vector (apply map list pop-behaviors)
        ]
    (assoc ind :lex-novelty (map #(ind-novelty %1 %2 %3) behaviors case-behavior-vector argmap))))

(calculate-individual-novelty {:behaviors [0 1]} {} '({:behaviors [1 2]} {:behaviors [2 2]}) {:use-single-thread true
                                                                                                  :novelty-number-of-neighbors-k 1})




(defn calculate-lex-novelty
  "Take a population of agents, derefs them, and calculates novelty of each
  individual (based on how many individuals have the same result for x test"
  [pop-agents novelty-archive {:keys [use-single-thread] :as argmap}]
  (dorun (map #((if use-single-thread swap! send) %
                calculate-individual-novelty
                novelty-archive (map deref pop-agents) argmap)
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
