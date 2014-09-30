(ns config-reader.core
  (:require [clojure.java.io :as io]
            [clojure.string :refer [upper-case]]
            [slingshot.slingshot :refer [try+]]))

(defn- deep-merge
  "Recursively merge map sequence."
  [& ms]
  (letfn [(f [a b]
            (if (and (map? a) (map? b))
              (deep-merge a b)
              b))]
    (apply merge-with f ms)))

(defn- translate-value
  "For each variable value read convert numbers to numbers, leave the rest as-is"
  [value]
  (try+
   (let [rv (read-string value)]
     (if (number? rv)
       rv
       value))
   (catch Object _
     value)))

(defn- read-environment
  "Tries to read properties from environment variables"
  [key-list]
  (reduce #(if-let [value (System/getenv (-> %2 first name upper-case))]
             (assoc %1 (first %2) (translate-value value))
             %1) {}
             key-list))


(defn- select-file
  "Selects the first existent file from a list"
  [file-list]
  (->> file-list
       (drop-while #(not (.exists (io/file %))))
       first))


(defn read-config
  [defaults file-list]
  (if-let [file (select-file file-list)]
    (deep-merge defaults (read-string (slurp file)))
    defaults))
