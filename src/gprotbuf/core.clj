(ns gprotbuf.core
  (:use [clojure.pprint])
  (:require [instaparse.core :as insta])
  (:import [com.example.tutorial AddressBookProtos$Person AddressBookProtos$Person$PhoneNumber ASimpleTest$Udr]
           [com.google.protobuf ByteString]
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
              args)))))]
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
   :type identity
   :fieldName identity
   :fieldNumber identity
   :optionName identity
   :proto (fn [& args] (dbg args))
 ;  :topLevelDef (fn [& args] args)
;   :field (fn [& args] (reduce (fn [m [k v]] (assoc m k v)) {} args))
   })

(defn ast->clj [ast]
    (insta/transform
    ast->clj-map 
    ast))

(defn parse [text]
  (-> text (parser :start :proto) ast->clj))



(defn more-bytes? [b] (= (bit-and b 0x80) 0x80))

#_(defn varint-decode [bytes offset] 
   (loop [i offset
          res 0]
     (let [b (nth bytes i)
           v (+ (bit-shift-left (bit-and b 127) (* (- i offset) 7)) res)]
       (if (more-bytes? b)
         (recur (inc i) v)
         {:type 0, :value v, :index (inc i)}))))

  
#_(defn sixty-four-bit-decode [bytes offset] (throw (IllegalStateException. "not impl")))
#_(defn thirty-two-bit-decode [bytes offset] (throw (IllegalStateException. "not impl")))

#_(defn str-of [bytes start-ix end-ix]
   (String. (byte-array (subvec bytes start-ix end-ix))))

#_(defn length-delim-decode [bytes offset]
   (let [l (dbg (varint-decode bytes offset))
         start-ix (:index l)
         end-ix (+ start-ix (:value l))]
     (dbg {:type 2, :value (str-of bytes start-ix end-ix), :index (inc end-ix)})))

#_(defn decode-key [bytes offset]
   (let [b (nth bytes offset)
         type (bit-and b 0x7)
         key-value (bit-shift-right (bit-and b 127) 3)]
     (assoc 
       (update (varint-decode (concat [(bit-or key-value (if (more-bytes? b) 0x80 0))] (nthrest bytes (inc offset))) 0) 
               :index (partial + offset)) :type type)))


#_(defn decode [bytes offset]
   (let [n (dec (count bytes))]
   (loop [i offset
          res []]
     (if (< i n)
       (let [k (decode-key bytes i)
           {:keys [type index value]} k
           v (assoc 
          (condp = type
            0 (varint-decode bytes index)
            1 (sixty-four-bit-decode bytes index)
            2 (length-delim-decode bytes index)
            5 (thirty-two-bit-decode bytes index)
          ) :key value)]
           (recur (:index v) (conj res v)))
         res))))



#_(def message "message Outer {
  option (my_option).a = true;
  message Inner {   
    int64 ival = 1;
  }
  map<int32, string> my_map = 2;
}")
#_(def message "message Test1 {
  required int32 a = 1;
}")

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