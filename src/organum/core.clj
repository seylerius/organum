(ns organum.core
  (:require [clojure.java.io :as io]
            [clojure.walk :as w]
            [hiccup.core :as h]
            [instaparse.core :as insta]
            [cuerdas.core :as s]))

;; Parsers

(def doc-metadata
  (insta/parser
   "<document> = token (ows token)*
    <token> = metadata / content
    <metadata> = title | author | date
    title = <'#+title: '> #'.*'
    author = <'#+author: '> #'.*'
    <ows> = <#'[\\s\r\n]*'>
    date = <'#+date: '> #'.*'
    <content> = #'(?s).*'"))

(def headlines
  (insta/parser
   "<S> = token (brs token)*
    <token> = section / content
    section = br? h ws? (brs content)*
    h = stars ws headline
    <headline> = keyed / unkeyed
    <keyed> = keyword ws unkeyed
    <unkeyed> = prioritized / title
    <prioritized> = priority ws title
    <title> = (#'.'+ ws* tags) / #'.+'
    stars = #'\\*+'
    keyword = #'TODO|DONE'
    priority = <'[#'> #'[a-zA-Z]' <']'>
    tags = <':'> (tag <':'>)+ ws?
    <tag> = #'[a-zA-Z0-9_@]+'
    <ws> = <#'[^\\S\\r\\n]+'>
    <brs> = (br br br br+) / (br br br+) / (br br+) / br+
    br = <#'\\r?\\n'>
    <content> = #'^([^*].*)?'"))

(def inline-markup
  (insta/parser
   "<inline> = (b | i | u | strike | verbatim | code | super | sub | string)+
    b = <'*'> inline <'*'>
    i = <'/'> inline <'/'>
    u = <'_'> inline <'_'>
    strike = <'+'> inline <'+'>
    verbatim = <'='> '[^=]+' <'='>
    code = <'~'> #'[^~]+' <'~'>
    super = <'^'> (#'\\w' | <'{'> inline <'}'>)
    sub = <'_'> (#'\\w' | <'{'> inline <'}'>)
    <string> = '\\\\*' | '\\\\/' | '\\\\_' | '\\\\+' | '\\\\='  '\\\\~' | '\\\\^' | #'[^*/_+=~^_\\\\]*'"))

;; (def is-table
;;   (insta/parser
;;    "org-table = bar-table-line+
;;     bar-table-line = #'|[^\\n\\r]+
;;     br = #'[\\r\\n]'"))

(def org-tables
  (insta/parser
   "table = th? tr+
    th = tr-start td+ line-break horiz-line
    <line-break> = <#'[\\r\\n]'>
    <horiz-line> = <#'[-|+]+'>
    tr = tr-start td+
    <tr-start> = <'|'> <#'[\\s]+'>
    td = contents td-end
    <td-end> = <#'\\s+|'>
    <contents> = #'(\\s*[^|\\s]+)+'"))

;; Fixers

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

;; Overall Parser

(defn parse
  "Take org-mode data and parse it to hiccup notation"
  [data]
  (->> data
       doc-metadata
       (map (partial reparse-string headlines))
       fix-tree
       (insta/transform {:h clean-headline})
       rejoin-lines
       (insta/transform {:section (fn [& stuff]
                                    (rejoin-lines (concat [:section]
                                                          stuff)))})
       ))

(defn parse-file
  "Read the given file path and parse it"
  [path]
  (parse (slurp path)))
