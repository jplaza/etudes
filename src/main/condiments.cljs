(ns condiments
  (:require ["node-xml-lite" :as xml]
            [clojure.string :as str]))

;; (def xml (js/require "node-xml-lite"))

(defn empty-string?
  [item]
  (and (string? item) (str/blank? item)))

(defn node->map-entry
  [node]
  [(.-name node) (-> node .-childs first)])

(defn condiment?
  [[key v]]
  (.test #"cond_._name" key))

(defn condiment-food
  [row-map]
  ;; (println "ROWMAP:" row-map)
  ;; (println "filter:" (zipmap (->> (filter condiment? row-map)
  ;;                                 (map last))
  ;;                            (repeat (get row-map "display_name"))))
  ;; (println "food:" (get row-map "display_name"))
  (zipmap (->> (filter condiment? row-map)
               (map last))
          (repeat (vector (get row-map "display_name")))))

(defn log [in]
  (prn "!! log:")
  (cljs.pprint/pprint in)
  in)

(defn row->map2
  [row]
  (->> (remove empty-string? (.-childs row))
       (mapv node->map-entry)
       (into {})
       condiment-food
       log))

(defn condiment-food-map
  [[condiments food] node]
  (let [content (-> node .-childs first)]
    (cond
      (= (.-name node) "display_name")     (vector condiments (-> node .-childs first))
      (.test #"cond_._name" (.-name node)) (vector (assoc condiments content [food]) food))))

(defn row->map
  [row]
  (->> (remove empty-string? (.-childs row))
      ;;  (map node->map-entry)
       (reduce condiment-food-map [{} nil])
       log
       first
       #_(into {})))

(defn process-xml
  [xml-path]
  (let [docmap (.parseFileSync xml xml-path)
        rows (remove empty-string? (.-childs docmap))]
    (->> (map row->map rows)
         (apply merge-with into))))

(defn process-xml2
  [xml-path]
  (let [docmap (.parseFileSync xml xml-path)
        rows (remove empty-string? (.-childs docmap))]
    (->> (mapv row->map2 rows)
         (apply merge-with into))))

(comment

  (-> (process-xml "/Users/juanantonio/Workspace/gigs/guillermo-ithier/etudes/Foods_Needing_Condiments_Table.xml")
      (cljs.pprint/pprint))

  (-> (process-xml2 "/Users/juanantonio/Workspace/gigs/guillermo-ithier/etudes/Foods_Needing_Condiments_Table.xml")
      (cljs.pprint/pprint)))