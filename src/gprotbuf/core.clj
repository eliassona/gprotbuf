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

#_(defn clean-comment [text]
   (.replaceAll 
     text "//.*|(\"(?:\\\\[^\"]|\\\\\"|.)*?\")|(?s)/\\*.*?\\*/", "$1 "))

(defmulti comment-replace (fn [state ch] state))

(defmethod comment-replace :idle [state ch]
  (condp = ch
   \/ [:slash " "]
   \" [:string ch]
   [state ch])
  )

(defmethod comment-replace :slash [state ch]
  (condp = ch
   \/ [:single-comment " "]
   \* [:multi-comment " "]
   [:idle ch])
  )

(defmethod comment-replace :single-comment [state ch]
  (condp = ch
   \newline [:idle ch]
   [state " "])
  )

(defmethod comment-replace :multi-comment [state ch]
  (condp = ch
   \* [:star " "]
   \newline [:star ch]
   [state " "])
  )

(defmethod comment-replace :star [state ch]
  (condp = ch
   \/ [:idle " "]
   [state " "])
  )

(defmethod comment-replace :string [state ch]
  (condp = ch
   \\ [:backslash ch]
   \" [:idle ch]
   [state ch])
  )

(defmethod comment-replace :backslash [state ch]
  [:string ch])

(defn clean-comment [text]
  (loop [t text
         state :idle
         res []]
    (if (not (empty? t))
      (let [[state ch] (comment-replace state (first t))]
        (recur (rest t) state (conj res ch)) )
      (apply str res))))
    


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

(defn name-of-label [l ds]
  (condp = l
    :field (nth ds 2)
    :message (second ds)
    :enum (second ds)))

(defn- name-of [ds]
  (let [l (first ds)]
    (when (contains? #{:message :enum :field} l) (with-meta {:name (name-of-label l ds)} (meta ds)))))

(defn throw-exception! [m msg]
  (let [col-line (meta m)]
    (throw (ParserException. (format "%s. %s" (:name m) msg)  (:instaparse.gll/start-line col-line) (:instaparse.gll/start-column col-line)))))

(defn check-name-clash [& args]
  (when-let [res  
    (first
      (filter 
        #(> (val %) 1)
        (frequencies 
          (filter 
            identity 
            (map 
              name-of 
              args)))))]
      (throw-exception! (first res) "")))

(defn intersect? [[[from-1 to-1] [from-2 to-2]]]
  (or 
    (and (>= from-2 from-1) (<= from-2 to-1))
    (and (>= to-2 from-1) (<= to-2 to-1))))

(def tag-max 536870911)
(def tag-reserved-ranged [19000 19999])

(defn- range-of [m]
  (fn ([v] (with-meta [v v] m))
      ([from _ to] (with-meta [from (if (number? to) to tag-max)] m))))

(defn res->clj [m] {:range (range-of m)
                    :reserved identity
                    :ranges (fn [& args] args)})

(defn check-reserved-overlap! [all-comb]
  (doseq [c all-comb]
      (when (intersect? c)
        (throw-exception! (second (dbg c)) ""))))

(defn remove-repeated [f]
  (let [r (rest f)]
    (with-meta 
      (conj
        (if (= (first r) "repeated")
          (rest r)
          r) :field) (meta f))))

(defn check-field-tags! [fields reserved]
  (loop [reserved reserved
         fields (map remove-repeated fields)]
    (when-let [f (first fields)]
      (let [range-of (range-of (meta f))
            tag (range-of (nth f 3))]
        (check-reserved-overlap! 
          (for [x (conj reserved tag-reserved-ranged)
                y [tag]]
            [x y]))
        (recur (conj reserved tag) (rest fields)))))) 
  

(defn check-reserved-clash [& args]
  (let [nth-fn #(nth % 2)
        f-fn (fn [v] (filter #(= (first %) v) args))
        reserved (f-fn :reserved)
        fields (f-fn :field)
        ranges (map-indexed (fn [i v] (conj v i)) (mapcat #(insta/transform (res->clj (meta %)) %) reserved))
        all-comb (for [x ranges
                          y ranges
                          :when (not= (nth-fn x) (nth-fn y))]
                      [x y])]
    (check-reserved-overlap! all-comb)
    (check-field-tags! fields ranges)    
    ))
      

(defn check-name-and-reserved-clash [& args]
  (apply check-name-clash args)
  (apply check-reserved-clash args)
   args)


(defn check-duplicates! [enum-name values the-fn]
  (let [f (filter #(> (val %) 1) (frequencies (map the-fn values)))]
    (when (not (empty? f))
      (let [e (second (filter #(= (-> f first first) (the-fn %)) values))
            v (second e)]
        (throw-exception! (with-meta {:name v} (meta e)) (format "\"%s\" is already defined in \"%s\"." v enum-name ))))))

(defn check-duplicate-enum-names! [enum-name values] (check-duplicates! enum-name values second))
(defn check-duplicate-enum-values! [enum-name values] (check-duplicates! enum-name values #(nth % 2)))

(def supported-enum-opts #{"allow_alias"}) 

(defn supported-opts-of [opts]
  (let [opt-map (into {} (mapv (comp vec rest) opts))]
    #_(when-not (clojure.set/subset? (-> keys set) supported-enum-opts)
       (throw-exception! (with-meta {:name :todo} (meta opts))))
    opt-map))

(defn check-enum-values [& args]
  (let [enum-name (-> args first second)
        body (second args)
        values (filter #(= (first %) :enumField) body)
        opts (filter #(= (first %) :option) body)
        name (-> args first second)
        m (-> args first meta)
        opts (supported-opts-of opts)]
    
    (when (empty? values) (throw-exception! (with-meta {:name name} m) "Enums must contain at least one value."))
    (when (not= (-> values first (nth 2)) 0) (throw-exception! (with-meta {:name name} (-> values first meta)) "The first enum value must be zero in proto3."))
    (check-duplicate-enum-names! enum-name values)
    (when-not (opts "allow_alias") 
     (check-duplicate-enum-values! enum-name values))
    [name body]
  ))


(def ast->clj-map 
  {
   :ident (fn [& args] (apply str args))
;   :messageName identity
   :hexDigit identity
   :hexLit (fn [x y z] (read-string (str x y z)))
   :decimalDigit read-string
   :decimalLit (fn [& args] (read-string (apply str args)))
;   :enumName identity
   :enumType second
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
   :enum check-enum-values
   :enumBody (fn [& args] args)
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
	(.setIntField b 1)
  (.addAllIntList b [])
  (.setSigned32 b 0)
  (.addAllSigned64 b [])
  (.setBoolField b false)
  (.setFloatField b 0.0)
  (.setDoubleField b 0.0)
  (.setF32Field b 2)
  (.setF64Field b 0)
  (.setInt32 b 0)
  (.setInt64 b 0)
	(def p (.build b))
  (vec (.toByteArray p))
  
  ;V2
  [10 0 18 0 24 0 64 0 -96 1 0 -83 1 0 0 0 0 -79 1 0 0 0 0 0 0 0 0 -67 1 0 0 0 0 -63 1 0 0 0 0 0 0 0 0 -16 1 0 -8 1 0]
  )

)