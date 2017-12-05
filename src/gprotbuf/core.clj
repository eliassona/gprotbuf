(ns gprotbuf.core
  (:use [clojure.pprint])
  (:require [instaparse.core :as insta])
  (:import #_[com.example.tutorial AddressBookProtos$Person AddressBookProtos$Person$PhoneNumber ASimpleTest$Udr]
           #_[com.google.protobuf ByteString]
           [gprotbuf.exception ParserException]))

(defmacro dbg [body]
  `(let [x# ~body]
     (println "dbg:" '~body "=" x#)
     x#))


;;-----------parser----------------------------------

(defn clean-comment [text]
  (.replaceAll 
    text "//.*|(\"(?:\\\\[^\"]|\\\\\"|.)*?\")|(?s)/\\*.*?\\*/", "$1 "))


(def parser (insta/parser (clojure.java.io/resource "googleprotocolbuffers.bnf")))

(defn parse [text] (-> text clean-comment (parser :start :proto)))

(defn parse-block [text]
  (let [ct (clean-comment text)]
    (insta/add-line-and-column-info-to-metadata ct (parser ct :start :block, :partial true))))

(defn syntax? [text] (not (insta/failure? (insta/parse parser (clean-comment text) :start :proto :partial true))))

;;---------------------------------------------------------------------------------------------------------

(def wire-types 
  {
   #{"int32", "int64", "uint32", "uint64", "sint32", "sint64", "bool", "enum"} 0
   #{"fixed64", "sfixed64", "double"} 1
   #{"string", "bytes" :message "repeated"} 2
   #{"fixed32", "sfixed32", "float"} 5
   })

(defn check-name-clash [& args]
  (dbg args)
  )

(defn- name-of [ds]
  (when (contains? #{:message :enum} (first ds)) (with-meta {:name (second ds)} (meta ds))))


(defn throw-exception! [m]
  (let [col-line (meta m)]
    (throw (ParserException. (format "%s is already defined" (:name m))  (:instaparse.gll/start-line col-line) (:instaparse.gll/start-column col-line)))))

(defn check-name-and-reserved-clash [& args]
  (if-let [res  
    (first
      (filter 
        #(> (val %) 1)
        (frequencies 
          (filter 
            identity 
            (map 
              name-of 
              (dbg args))))))]
      (throw-exception! (first res))
    args))
  



(def ast->clj-map 
  {
   :ident (fn [& args] (apply str args))
;   :messageName identity
   :hexDigit identity
   :hexLit (fn [x y z] (read-string (str x y z)))
   :decimalDigit read-string
   :decimalLit (fn [& args] (read-string (apply str args)))
   :enumName identity
   :enumType (fn [& args] (apply str args))
   :messageBody check-name-and-reserved-clash
   :intLit identity
   :charValue identity
   :strLit (fn [& args] (apply str args))
   :boolLit read-string
   :octalDigit identity
   :octalLit (fn [& args] (read-string (apply str args)))
   :constant identity
   :oneofName identity
   :oneofField (fn [& args] args)
   :messageName identity
   :message (fn [& args] (conj args :message))
   :type identity
   :fieldName identity
   :fieldNumber identity
   :optionName identity
   :proto (fn [& args] (apply check-name-and-reserved-clash (map second (filter #(= (first %) :topLevelDef) args))))
;   :field (fn [& args] (reduce (fn [m [k v]] (assoc m k v)) {} args))
   })

(defn ast->clj [ast]
    (insta/transform
    ast->clj-map 
    ast))

(defn parse [text]
  (-> text (parser :start :proto) ast->clj))




(comment
(def b (AddressBookProtos$Person/newBuilder))
(.setId b 12345)
(.setName b "anders")
(.setEmail b "anders@hej.com")
(def p (.build b))
(AddressBookProtos$Person/parseFrom (.toByteArray p))

(do
	(def b (ASimpleTest$Udr/newBuilder))
	(.setBaField b (ByteString/copyFrom (byte-array [])))
	(.setStrField b "")
	(.setIntField b 0)
  (.addAllIntList b [])
  (.setSigned32 b 0)
  (.addAllSigned64 b [])
  (.setBoolField b false)
  (.setFloatField b 0.0)
  (.setDoubleField b 0.0)
  (.setF32Field b 0)
  (.setF64Field b 0)
  (.setInt32 b 0)
  (.setInt64 b 0)
	(def p (.build b))
  (vec (.toByteArray p))
  
  ;V2
  [10 0 18 0 24 0 64 0 -96 1 0 -83 1 0 0 0 0 -79 1 0 0 0 0 0 0 0 0 -67 1 0 0 0 0 -63 1 0 0 0 0 0 0 0 0 -16 1 0 -8 1 0]
  )

)