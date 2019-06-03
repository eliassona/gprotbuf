(ns gprotbuf.core
  "Protocol buffers parser"
  (:use [clojure.pprint])
  (:require [instaparse.core :as insta]
            [commentclean.core :as comment]
            [gprotbuf.check :refer [check-enum-values
                                    check-name-and-reserved-clash
                                    check-message]])
  (:import [gprotbuf.exception ParserException]))

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


(defn type-str-of [& t]
  ;this is not well made
  (str (apply str (butlast t)) (-> t last second)))

(defn map-field-type-of [mfn]
  (apply conj mfn "_Message"))

(defn map-field->message [key-type value-type name tag]
  (let [name-type (map-field-type-of name)
        m (meta name)]
    (vary-meta 
      (with-meta 
        [:message
         [:messageName name-type]
         [:messageBody 
          (with-meta 
            [:field
             [:type [:enumType [:enumName key-type]]]
             [:fieldName [:ident "k" "e" "y"]]
               [:fieldNumber [:intLit [:decimalLit "1"]]]] m)
          (with-meta 
            [:field
             value-type
             [:fieldName [:ident "v" "a" "l" "u" "e"]]
               [:fieldNumber [:intLit [:decimalLit "2"]]]] m)]] m) 
      assoc :map-field-name name, :map-field-tag tag)))

(defn add-map-fields [args]
  (loop [a args
         res []]
    (if (empty? a)
      res
      (recur
        (rest a)
        (let [v (first a)
              m (meta v)]
          (if-let [mfn (:map-field-name m)]
            (conj res v (with-meta [:field "repeated" [:type [:enumType [:enumName (map-field-type-of mfn)]]] [:fieldName mfn] (:map-field-tag m)] m))
            (conj res v)))))))


(def ast-map-field->field-map
  { 
   :mapField map-field->message 
   :mapName identity
   :keyType identity
   :messageBody (fn [& args] (with-meta `[:messageBody ~@(add-map-fields args)] (-> args first meta)))
})


(defmacro if-check [prop-map expr]
  `(when-not (:disable-check ~prop-map) ~expr))

(defn ast->clj-map [global-types prop-map] 
  {
   :ident (fn [& args] (apply str args))
   :hexDigit identity
   :hexLit (fn [x y z] (read-string (str x y z)))
   :decimalDigit read-string
   :decimalLit (fn [& args] (read-string (apply str args)))
   :enumType type-str-of
   :messageBody (fn [& args] args) 
   :message (fn [& args] 
              (if-check prop-map (check-message (first args) args global-types)) 
              (conj args :message))
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
   :enum (fn [& args]        
           (let [body (second args)
                 name (-> args first second)]
             (if-check prop-map (check-enum-values args))
             [:enum name body]))
   :enumBody (fn [& args] args)
   :proto (fn [& args] 
            (if-check prop-map (check-name-and-reserved-clash "" (map second (filter #(= (first %) :topLevelDef) args)))) 
            args)
;   :keyType identity
;   :mapName identity
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



(defn ast->clj [ast & properties]
  (let [prop-map (apply hash-map properties)
        ast (insta/transform ast-map-field->field-map ast) ;make mapField into a regular sub message field and add field for it.
        gt (global-types-of ast)]
    (vary-meta 
      (insta/transform
      (ast->clj-map gt prop-map)
      ast) assoc :global-types gt)))



