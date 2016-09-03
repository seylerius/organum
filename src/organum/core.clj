(ns organum.core
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
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
   "<S> = token (ows token)*
    <token> = section / content
    section = h (ows content)*
    h = ows stars <#'\\s+'> headline
    <headline> = keyed / unkeyed
    <keyed> = keyword ws-line unkeyed
    <unkeyed> = prioritized / title
    <prioritized> = priority ws-line title
    <title> = (#'.'+ ws-line? tags) / #'.+'
    stars = #'^\\*+'
    keyword = #'TODO|DONE'
    priority = <'[#'> #'[a-zA-Z]' <']'>
    tags = <':'> (tag <':'>)+ ws
    <tag> = #'[a-zA-Z0-9_@]+'
    <ws> = <#'[\r\n\\s]+'>
    <ws-line> = <#'[^\r\n\\S]+'>
    <ows> = <#'\\s*'>
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
       ))
