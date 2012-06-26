(ns cadr
  (:use [clojure.math.combinatorics :only (selections)]))

;;;; Bounded Kleene closure

(defn kleene-closure
  "Generates the set of all strings of length `lower-bound' to
`upper-bound' (inclusive) over `alphabet'; for e.g. Σ = {0, 1}, Σ^4_2
= {00, 01, ..., 1111}."
  ([alphabet]
     (kleene-closure alphabet (count alphabet)))
  ([alphabet upper-bound]
     (kleene-closure alphabet upper-bound 0))
  ([alphabet upper-bound lower-bound]
     (loop [n upper-bound
            kleene-closure '()]
       (if (< n lower-bound)
         kleene-closure
         (let [kleene-n (selections alphabet n)]
           (recur (dec n)
                  (concat kleene-n kleene-closure)))))))

;;;; Primitives

(def car first)
(def cdr rest)

;;;; Map names to compositions.

(def infix->primitive
  ^{:doc "Maps the c[ad]+r-infix to a primitive."
    :private true}
  '{a car
    d cdr})

(defn- infixes->composition [string]
  "Maps a string of c[ad]+r-infixes to a primitive-composition."
  (cons 'comp (map #(% infix->primitive) string)))

(defn- infixes->name [string]
  "Maps a string of c[ad]+r-infixes to its function-name."
  (apply (comp symbol str)
         (list "c" (apply str string) "r")))

(def name->composition
  ^{:doc "Generates a map of names to compositions; e.g. cadr -> (comp
  car cdr)."
    :private true}
  (map #(cons (infixes->name %)
              (infixes->composition %))
       (kleene-closure '(a d) 4 2)))

(defmacro defn-compositions []
  ^{:doc "Generates definitions from the name -> composition map of
  the form, e.g., `(defn caar [cons] (car (car cons)))'."
    :private true}
  `(do ~@(map (fn [[name & composition]]
                `(defn ~name [cons#] (~composition cons#)))
              name->composition)))

;;;; Evaluate the composition-definitions.

(defn-compositions)
