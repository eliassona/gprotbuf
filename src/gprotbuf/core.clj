(ns gprotbuf.core
  (:require [instaparse.core :as insta])
  (:import [com.example.tutorial AddressBookProtos$Person AddressBookProtos$Person$PhoneNumber]))

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
   :messageBody (fn [& args] args)
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
)