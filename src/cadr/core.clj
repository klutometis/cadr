(ns cadr.core
  (:use [clojure.math.combinatorics :only (selections)]))

(defn kleene-closure
  ([alphabet] (kleene-closure alphabet (count alphabet)))
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

(def Σ kleene-closure)

;;; Primitives
(def car first)
(def cdr rest)

(def ^{:private true}
  infix->primitive
  '{a car
    d cdr})

(defn- infixes->composition [string]
  (cons 'comp (map #(% infix->primitive) string)))

(defn- infixes->name [string]
  (apply (comp symbol str)
         (list "c" (apply str string) "r")))

(def name->composition
  ^{:private true}
  (map #(cons (infixes->name %)
              (infixes->composition %))
       (kleene-closure '(a d) 4 2)))

(defmacro
  ^{:private true}
  defn-compositions []
  `(do ~@(map (fn [[name & composition]]
                `(defn ~name [cons#] (~composition cons#)))
              name->composition)))

(defn-compositions)
