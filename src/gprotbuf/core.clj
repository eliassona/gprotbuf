(ns gprotbuf.core
  (:require [instaparse.core :as insta])
  (:import [com.example.tutorial AddressBookProtos$Person AddressBookProtos$Person$PhoneNumber]))

(defmacro dbg [body]
  `(let [x# ~body]
     (println "dbg:" '~body "=" x#)
     x#))

(defn clean-comment [text]
  (.replaceAll 
    text "//.*|(\"(?:\\\\[^\"]|\\\\\"|.)*?\")|(?s)/\\*.*?\\*/", "$1 "))


(def parser (insta/parser (clojure.java.io/resource "googleprotocolbuffers.bnf")))


(defn parse [text] (-> text clean-comment (parser :start :proto)))


(def wire-types 
  {
   #{"int32", "int64", "uint32", "uint64", "sint32", "sint64", "bool", "enum"} 0
   #{"fixed64", "sfixed64", "double"} 1
   #{"string", "bytes" :message "repeated"} 2
   #{"fixed32", "sfixed32", "float"} 5
   })

(def ast->clj-map 
  {
   :ident (partial apply str)
   :messageName identity
   })

(defn ast->clj [ast]
    (insta/transform
    ast->clj-map 
    ast))
  

(defn more-bytes? [b] (= (bit-and b 0x80) 0x80))

(defn varint-decode [bytes offset] 
  (loop [i offset
         res 0]
    (let [b (nth bytes i)
          v (+ (bit-shift-left (bit-and b 127) (* (- i offset) 7)) res)]
      (if (more-bytes? b)
        (recur (inc i) v)
        {:type :varint, :value v, :index (inc i)}))))



  
(defn sixty-four-bit-decode [bytes offset] :sixty-four-bit)
(defn thirty-two-bit-decode [bytes offset] :thirty-two)
(defn length-delim-decode [bytes offset] :length-delim)

(defn decode-key [bytes offset]
  (let [b (nth bytes offset)
        type (bit-and b 0x7)
        key-value (bit-shift-right (bit-and b 127) 3)]
    (varint-decode (concat [(bit-or key-value (if (more-bytes? b) 0x80 0))] (nthrest bytes (inc offset))) 0)
  ))


(defn decode [bytes offset]
  (loop [i offset
         res []]
    (let [k (decode-key bytes offset)
          {:keys [type index]} k]
    (condp = type
      :varint (varint-decode bytes index)
      1 (sixty-four-bit-decode bytes index)
      2 (length-delim-decode bytes index)
      5 (thirty-two-bit-decode bytes index)
    ))))







(def message "message Outer {
  option (my_option).a = true;
  message Inner {   
    int64 ival = 1;
  }
  map<int32, string> my_map = 2;
}")
#_(def message "message Test1 {
  required int32 a = 1;
}")

(def b (AddressBookProtos$Person/newBuilder))
(.setId b 12345)
(.setName b "anders")
(.setEmail b "anders@hej.com")
(def p (.build b))
(AddressBookProtos$Person/parseFrom (.toByteArray p))
