(ns clojush.pushgp.selection.novelty-lexicase
  (:use [clojush random util globals])
  (:require [clojure.math.numeric-tower :as math]))

(defn nl-select-individuals-for-novelty-archive
  "Returns a number of individuals to be added to the novelty archive. Number
   of indviduals are :individuals-for-novelty-archive-per-generation."
  [population argmap]
  (take (:individuals-for-novelty-archive-per-generation argmap)
        (lshuffle population)))

(defn nl-behavioral-distance
  "Takes two behavior vectors and finds the distance between them. Differences in
   vectors are based on the data type(s) they contain. Distance metric is based on
   the arg :novelty-distance-metric.
   Note that there is no limit on behavior differences, which will only be limited
   by their max bounds based on things like maximum integer size."
  [behavior1 behavior2 {:keys [novelty-distance-metric] :as argmap}]
  (print behavior1)
  (print behavior2)
  (if (= novelty-distance-metric :hamming) ; This is here, instead of below, for speed reasons
    (hamming-distance behavior1 behavior2)
    (let [behavior-differences (map (fn [b1 b2]
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
                                    behavior1
                                    behavior2)]
      (case novelty-distance-metric
        :manhattan (apply +' behavior-differences)
        :euclidean (math/sqrt (apply +' (map #(*' % %) behavior-differences)))))))

(defn nl-calculate-behavior-distance-map
  "Calculates a map storing the distances between any two behaviors, of the form:
    {behavior1 {behavior1 dist11 behavior2 dist12 behavior3 dist13 ...}
     behavior2 {behavior1 dist21 behavior22 dist2 behavior3 dist23 ...}
     ...}
   Note: Only has outer-level keys for population behaviors, not archive behaviors.
   But, has inner-level keys for both population and archive behaviors."
  [distinct-pop-behaviors distinct-pop-and-archive-behaviors argmap]
  (loop [behavior (first distinct-pop-behaviors)
         pop-behaviors distinct-pop-behaviors
         behavior-distance-map {}]
    (if (empty? pop-behaviors)
      behavior-distance-map
      (let [distances-from-behavior
            (into {}
                  (map (fn [other-behavior]
                         (vector other-behavior
                                 (if (contains? behavior-distance-map other-behavior)
                                   (get-in behavior-distance-map
                                           [other-behavior behavior])
                                   (nl-behavioral-distance behavior other-behavior argmap))))
                       distinct-pop-and-archive-behaviors))]
        (recur (first (rest pop-behaviors))
               (rest pop-behaviors)
               (assoc behavior-distance-map behavior distances-from-behavior))))))

(defn nl-calculate-behavior-sparseness
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

(defn nl-assign-novelty-to-individual
  "Calculates the novelty of the individual based on the behaviors in the population
   and in the novelty-archive. Returns the individual with the :novelty key set, and
   if :novelty is a meta-error-category, also sets that."
  [individual behavior-sparseness]
  (let [novelty (get behavior-sparseness (:behaviors individual))
        novelty-inverse (/ 1 (inc novelty))]
    (assoc individual
           :novelty-lex (list* (:novelty-lex individual) novelty)
           :meta-errors (replace {:novelty novelty-inverse} (:meta-errors individual)))))



(defn calc-single-behavior-novelty
  "Takes a population with a single item in behaviors, outputs population where each individual's
  :novelty-lex has had a number appended"
  [pop-agents novelty-archive {:keys [use-single-thread] :as argmap}]
  (let [pop-behaviors (map #(:behaviors (deref %)) pop-agents)
        pop-and-archive-behaviors (concat pop-behaviors
                                          (map :behaviors novelty-archive))
        behavior-distance-map (nl-calculate-behavior-distance-map (distinct pop-behaviors)
                                                               (distinct pop-and-archive-behaviors)
                                                               argmap)
        behavior-sparseness (nl-calculate-behavior-sparseness pop-and-archive-behaviors
                                                           behavior-distance-map
                                                           argmap)]
    (dorun (map #((if use-single-thread swap! send)
                  % nl-assign-novelty-to-individual behavior-sparseness)
                pop-agents)))
  (when-not use-single-thread (apply await pop-agents)) ;; SYNCHRONIZE
  )


(defn calculate-ind-novelty
  "Calculates novelty for each individual in the population with respect to the
   rest of the population and the novelty-archive. Sets novelty to meta-error
   if necessary."
  [pop-agents novelty-archive {:keys [use-single-thread] :as argmap}]
  (print "Calculating novelty...") (flush)
  (loop [num 0 pop pop-agents archive novelty-archive argmap argmap]
    (if (>= num (count (:behaviors (first pop))))
      (recur (inc num) (calc-single-behavior-novelty (map #(:behaviors (nth (:behaviors %) num) %) pop) archive argmap) archive argmap)))
   (println "Done calculating novelty."))


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
                                             (map :novelty-lex survivors)))]
        (recur (filter #(= (nth (:novelty-lex %) (first cases)) min-err-for-case)
                       survivors)
               (rest cases))))))

