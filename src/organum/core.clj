(ns organum.core
  (:require [clojure.java.io :as io]
            [clojure.walk :as w]
            [hiccup.core :as h]
            [instaparse.core :as insta]
            [cuerdas.core :as s]))

;; Parsers

(def parser-root "src/organum/")
(def grammar-ext ".ebnf")
(def parsers* [:doc-metadata
               :headlines
               :inline-markup
               :is-table
               :org-tables])

(defn parser-path [bnf-name]
  ;; TODO: use resources folder
  (str parser-root (name bnf-name) grammar-ext))

(defn wrap-parser [f] (map (partial reparse-string f)))
(defn get-parser*
  [bnf-name]
  (insta/parser (slurp (parser-path bnf-name))))

(defn get-parser [bnf-name] (wrap-parser (get-parser* bnf-name)))

(def parsers (into {} (map (juxt identity get-parser) parsers*)))

(defn parse-all [init] (reduce #(%2 %1) init (vals parsers)))

(defn tree-fixer
  [tree item]
  (cond (vector? item)
        (conj tree (vec (reduce tree-fixer [] item)))

        (and (coll? item) (not (vector? item)))
        (apply concat tree (map (partial tree-fixer []) item))

        :default (conj tree item)))

(defn fix-tree
  [tree]
  (reduce tree-fixer '() tree))

(defn clean-headline
  [stars & args]
  (let [level (keyword (str "h" (count (second stars))))
        first-lists (->> args
                        (take-while (complement string?))
                        (drop-while string?))
        title (->> args
                   (drop-while (complement string?))
                   (take-while string?)
                   (apply str)
                   inline-markup)
        last-lists (->> args
                        (drop-while (complement string?))
                        (drop-while string?))]
    (vec (concat [level] first-lists title last-lists))))

(defn rejoin-lines
  "Rejoin lines with appropriate line breaks."
  [coll]
  (reduce #(if (string? (first %2))
             (conj %1 (apply str %2))
             (apply conj %1 %2))
          [] (partition-by string? (replace {[:br] "\n"} coll))))

;; Filters

(defn reparse-string
  "If `i` is a string, pass it to `parser`; otherwise, return `i` unmodified."
  [parser i]
  (if (string? i)
    (parser i)
    i))

(defn break-reducer
  "Filter based on breaks"
  [out item]
  (cond (string? item) (conj out item)
        (= [:br] item) (cond (string? (last out)) (conj out item)
                             (= [:br] (last out)) (conj out item)
                             :else out)
        (coll? item) (cond (string? (last out)) (conj out item)
                           (= [:br] (last out)) (break-reducer (butlast out)
                                                               item)
                           :else (conj out item))))

(defn break-cleaner
  "Remove extraneous breaks"
  ([coll]
   (reduce break-reducer [] coll))
  ([coll lead]
   (reduce break-reducer [lead] coll)))

;; Overall Parser

(defn parse
  "Take org-mode data and parse it to hiccup notation"
  [data]
  (->> data
       doc-metadata
       ;; (map (partial reparse-string headlines))
       ;; fix-tree
       ;; (insta/transform {:h clean-headline})
       ;; (insta/transform {:section (fn [& stuff]
       ;;                              (break-cleaner stuff :section))})
       ;; rejoin-lines
       ;; (insta/transform {:section (fn [& stuff]
       ;;                              (rejoin-lines (concat [:section]
       ;;                                                    stuff)))})
       ))

(defn parse-file
  "Read the given file path and parse it"
  [path]
  (parse (slurp path)))
