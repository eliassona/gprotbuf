(comment 
  (ns gprotbuf.codec
    (:use [clojure.pprint])
    (:require [gprotbuf.core :refer [dbg]])
    (:import [com.google.protobuf CodedOutputStream CodedInputStream]
             [java.io ByteArrayInputStream ByteArrayOutputStream]))

(defn decode-var-length [in]
  (let [l (.readRawVarint64 in)]
    (map (fn [_] (.readRawVarint64 in)) (range l))))

(defn decode-value [in]
  (let [tag-type (.readTag in)
        type (bit-and tag-type 7)
        tag (bit-shift-right tag-type 3)]
    [tag 
     {:type type 
      :value (condp = type
               0 (.readRawVarint64 in)
               1 (.readFixed64 in)
               2 (decode-var-length in)
               5 (.readFixed32 in)
               )}]))

(defn update-tag [m tag value]
  (let [v (get m tag)]
    (cond 
      (instance? java.util.List v)
      (update m tag conj value)
      (nil? v)
      (assoc m tag value)
      :else
      (assoc m tag [v value])
  )))

(defn in-of [data] 
  (CodedInputStream/newInstance 
    (ByteArrayInputStream. 
      (byte-array data))))

(defn decode [data]
  (let [in (in-of data)]
    (loop [m {}]
      (if (.isAtEnd in)
        m
        (let [[tag value] (decode-value in)]
          (recur (update-tag m tag value)))))))

(defn encode-var-length [out tag value]
  (let [n (count value)]
    (.writeRawVarint64 out n)
    (doseq [v value] (.writeRawVarint64 out v))
    )
  )

(defn encode-value [out tag v]  
  (let [type (:type v)
              value (:value v)]
    (condp = type
               0 (do 
                   (.writeTag out (int tag) (int type))
                   (.writeRawVarint64 out value))
               1 (.writeFixed64 out tag value)
               2 (do 
                   (.writeTag out (int tag) (int type))
                   (encode-var-length out tag value))
               5 (.writeFixed32 out tag value))))

(defn encode-entry [out e]
  (let [tag (key e)
        v (val e)]
    (if (instance? java.util.List v)
      (doseq [m v]
        (encode-value out tag m))
      (encode-value out tag v))))

(defn encode [m]
  (let [ba (ByteArrayOutputStream.)
        out (CodedOutputStream/newInstance ba)]
    (doseq [e m]
      (encode-entry out e))
    (.flush out)
    (vec (.toByteArray ba))))



)