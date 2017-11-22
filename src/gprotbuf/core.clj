(ns gprotbuf.core
  (:require [instaparse.core :as insta]))


(defn clean-comment [text]
  (.replaceAll 
    text "//.*|(\"(?:\\\\[^\"]|\\\\\"|.)*?\")|(?s)/\\*.*?\\*/", "$1 "))


(def parser (insta/parser (clojure.java.io/resource "googleprotocolbuffers.bnf")))


(defn parse [text] (-> text clean-comment (parser :start :proto)))
  