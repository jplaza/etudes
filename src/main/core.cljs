(ns core
  (:require ["node-xml-lite" :as xml]))

;; (def xml (js/require "node-xml-lite"))

(defn main2
  [& _args]
  (let [fname (nth (.-argv js/process) 2)
        parsed-data (.parseFileSync xml fname)]
    ;; (.log js/console "Hello node RELOAD!" _args fname)
    (prn (.-name parsed-data))))

(declare process-child)

(defn process-children
  "Process an array of child nodes, given a current food name
and an accumulated result"
  [[food result] children]
  (let [[final-food final-map] (reduce process-child [food result] children)]
    [final-food final-map]))

(defn add-condiment
  "Add food to the vector of foods that go with this condiment"
  [result food condiment]
  (let [food-list (get result condiment)
        new-list (if food-list (conj food-list food) [food])]
    (assoc result condiment new-list)))

(defn empty-string?
  [item]
  (and (string? item) (clojure.string/blank? item)))

(defn process-child
  "Given a current food and result map, and an item, return the new food name and result map"
  [[food result] item]
  (when (and false (.-childs item) (.-name (first (.-childs item))))
    (println "item: '" item "'")
    (println "name: '" (.-name item) "'")
    (println "firstchild: '" (first (.-childs item)) "'")
    (println "food: '" food "'")
    (println "result: '" result "'")
    (println "childs: '" (.-childs item) "'" "\n**************************"))
  ;; The first child of an element is text - either a food name ;; or a condiment name, depending on the element name.
  (let [children (remove empty-string? (.-childs item))
        ;; _ (prn "children:" children)
        firstchild (first children)]
    (cond
      (= (.-name item) "display_name") (vector firstchild result)
      (.test #"cond_._name" (.-name item))
      (vector food (add-condiment result food firstchild))
      (and children (.-name firstchild))
      (process-children [food result] children)
      :else [food result])))

(defn main
  [& _args]
  (let [docmap (.parseFileSync xml (nth (.-argv js/process) 2))]
    ;; (prn "-----------")
    ;; (prn docmap)
    ;; (prn "-----------")
    (cljs.pprint/pprint (last (process-children ["" {}]
                                                (remove empty-string? (.-childs docmap)))))))

(defn start
  []
  (prn "reloading...")
  (main))