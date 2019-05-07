(ns organum.agenda
  (:require [clojure.string :as string]))

(defn tasks [org]
  (->> org
       (filter :kw)
       (map (fn [m] {:desc (:name m) :status (keyword (string/lower-case (:kw m)))}))))
