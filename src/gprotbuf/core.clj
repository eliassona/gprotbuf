(ns gprotbuf.core
  (:use [clojure.pprint])
  (:require [instaparse.core :as insta]
            [commentclean.core :as comment])
  (:import #_[com.example.tutorial AddressBookProtos$Person AddressBookProtos$Person$PhoneNumber ASimpleTest$Udr]
           #_[com.google.protobuf ByteString]
           [gprotbuf.exception ParserException]))

(defmacro dbg [body]
  `(let [x# ~body]
     (println "dbg:" '~body "=" x#)
     x#))

;;-----------parser----------------------------------

(def parser (insta/parser (clojure.java.io/resource "googleprotocolbuffers.bnf")))

(defn parse [text] (-> text comment/clean (parser :start :proto)))

(defn parse-block [text]
  (let [ct (comment/clean text)]
    (insta/add-line-and-column-info-to-metadata ct (parser ct :start :block, :partial true))))

(defn syntax? [text] (not (insta/failure? (insta/parse parser (comment/clean text) :start :proto :partial true))))

;;---------------------------------------------------------------------------------------------------------

(defn name-of-label [l ds]
  (condp = l
    :field (nth ds (if (= (second ds) "repeated") 3 2))
    :message (second ds)
    :enum (second ds)))

(defn- name-of [ds]
  (let [l (first ds)]
    (when (contains? #{:message :enum :field} l) (with-meta {:name (name-of-label l ds)} (meta ds)))))

(defn throw-exception! [m msg]
  (let [col-line (meta m)]
    (assert col-line)
    (throw (ParserException. (format "%s. %s" (:name m) msg)  (:instaparse.gll/start-line col-line) (:instaparse.gll/start-column col-line)))))


(defn field-of [kw args]
  (filter #(= (first %) kw) args))

(defn check-name-clash [name args]
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
    (let [m (first res)]
      (throw-exception! (with-meta m (-> res first meta)) (format "\"%s\" is already defined in \"%s\"." (:name m) name)))))

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

(defn check-reserved-overlap! [name all-comb]
  (doseq [c all-comb]
      (when (intersect? c)
        (throw-exception! (with-meta {:name name} (meta (second c))) "Fields overlap."))))

(defn remove-repeated [f]
  (let [r (rest f)]
    (with-meta 
      (conj
        (if (= (first r) "repeated")
          (rest r)
          r) :field) (meta f))))

(defn name-and-value-of [f]
  (condp = (first f)
    :field 
    (let [ix (if (= (second f) "repeated")
               1 
               0)]
      [(nth f (+ 2 ix)) (nth f (+ 3 ix))])
    :enumField [(nth f 1) (nth f 2)]
    ))

(defn check-field-tags! [name fields reserved]
  (loop [reserved reserved
         fields fields
         ]
    (when-let [f (first fields)]
      (let [range-of (range-of (meta f))
            tag (-> f name-and-value-of second range-of)]
        (check-reserved-overlap! name
          (for [x (conj reserved tag-reserved-ranged)
                y [tag]]
            [x y]))
        (recur (conj reserved tag) (rest fields)))))) 

    

(defn check-reserved-clash [name args kw]
  (let [nth-fn #(nth % 2)
        reserved (field-of :reserved args)
        fields (field-of kw args)
        ranges (map-indexed (fn [i v] (conj v i)) (mapcat #(insta/transform (res->clj (meta %)) %) reserved))
        all-comb (for [x ranges
                          y ranges
                          :when (not= (nth-fn x) (nth-fn y))]
                      [x y])]
    (check-reserved-overlap! name all-comb)
    (check-field-tags! name fields ranges)))

(def proto-types #{"double" "float" "int32" "int64" "uint32" "uint64" 
                   "sint32" "sint64" "fixed32" "fixed64"
                   "sfixed32" "sfixed64" "bool" "string" "bytes"})


(defn valid-type? [t rel-types global-types]
  (or 
    (contains? proto-types t)
    (contains? rel-types t)
    (contains? global-types t)))

(defn check-types [name args global-types]
  (let [rel-types (into #{} (map second (filter #(contains? #{:enum :message} (first %)) args)))]
    (doseq [f (map remove-repeated (->> args (field-of :field)))]
      (let [t (second f)]
        (when-not (valid-type? t rel-types global-types)
          (throw-exception! (with-meta {:name name} (meta f)) (format "\"%s\" is not defined." t)))))))

(defn check-name-and-reserved-clash [name args]
  (check-name-clash name args)
  (check-reserved-clash name args :field)
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
    (check-reserved-clash name (second args) :enumField)
    [:enum name body]
  ))

(defn one-of-of-with-meta [one-of]
  (with-meta (-> one-of rest rest) (meta one-of)))

(defn one-of-of [fields]
  (let [one-ofs (filter #(= (first %) :oneof) fields)
        one-ofs (mapcat one-of-of-with-meta one-ofs)]
    (map #(with-meta (cons :field %) (meta %)) one-ofs)))

(defn check-message [message-name args global-types]
  (let [fields (second args)
        one-ofs (one-of-of fields)
        fields (concat one-ofs (second args))
        ]
    (check-types message-name fields global-types)
    (check-name-and-reserved-clash message-name fields) 
    (conj args :message)))

(defn type-str-of [& t]
  ;this is not well made
  (str (apply str (butlast t)) (-> t last second)))


(defn ast->clj-map [global-types] 
  {
   :ident (fn [& args] (apply str args))
   :hexDigit identity
   :hexLit (fn [x y z] (read-string (str x y z)))
   :decimalDigit read-string
   :decimalLit (fn [& args] (read-string (apply str args)))
   :enumType type-str-of
   :messageBody (fn [& args] args) 
   :message (fn [& args] (check-message (first args) args global-types))
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
   :enum check-enum-values
   :enumBody (fn [& args] args)
   :proto (fn [& args] (check-name-and-reserved-clash "" (map second (filter #(= (first %) :topLevelDef) args))))
   })


(def ast->names-map 
  {:decimalDigit identity
   :messageName identity
   :enumName identity
   :ident (fn [& args] (apply str args))})

(defn ast->names [ast]
    (insta/transform
    ast->names-map 
    ast))

(defn q-name-of [name context]
  (if (empty? context)
    name
    (reduce (fn [acc v] (if acc (format "%s.%s" acc v) v)) nil (conj context name)))
  )

(defn global-types-of
  ([ast]
    (let [state (atom #{})]
      (global-types-of (ast->names ast) [] state)
      @state))
  ([ast context state]
  (cond 
    (keyword? (first ast))
      (if (contains? #{:message :enum} (first ast))
        (let [name (second ast)] 
          (swap! state conj (q-name-of name context))
          (global-types-of (rest ast) (conj context name) state))
        (global-types-of (rest ast) context state)
        )
    (instance? java.util.List ast)  
    (doseq [x ast]
      (global-types-of x context state)
    ))))



(defn ast->clj [ast]
    (insta/transform
    (ast->clj-map (global-types-of ast))
    ast))



(defn parse [text]
  (-> text (parser :start :proto) ast->clj))



;;----------------------------------------------------------------------------------------------------------------------

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