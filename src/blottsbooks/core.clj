(ns blottsbooks.core
  (:require [blottsbooks.pricing :as pricing])
  (:gen-class))

;(defn default-main
;  "I don't do a whole lot ... yet."
;  [& args]
;  (println "Hello, World!"))

(defn say-welcome
  "Say welcome."
  [what]
  (println "Welcome to" what "!"))

(defn -main
  "Executable entry point"
  []
  (say-welcome "Blott's Books")
  (print (pricing/discount-price { :title "Emma", :price 9.99}) "\n"))

(defn print-greeting
  "Use a different greeting for preferred customers."
  [preferred-customer]
  (if preferred-customer
    (println "Welcome back to Blott's Books!")
    (println "Welcome to Blott's Books")))