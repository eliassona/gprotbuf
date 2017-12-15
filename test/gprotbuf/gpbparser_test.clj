(ns gprotbuf.gpbparser_test
  (:require [clojure.test :refer :all])
  (:import [gprotbuf GPBParser])
  )

(def parser (GPBParser/instance))

(deftest verify-empty-parse
  (.parse parser ""))


(deftest verify-syntax-parse
  (let [ast (.parse parser "syntax=\"proto3\";")]
    (is (not (.isFailure parser ast)))))

(deftest verify-block-syntax-parse
  (let [ast (.parseBlock parser "{ syntax=\"proto3\"; };")]
    (is (not (.isFailure parser ast)))))


(deftest verify-block-syntax-parse-error
  (let [ast (.parseBlock parser "{ syntax=\"proto2\"; };")]
    (is (.isFailure parser ast))
    (let [cl (.getFailureColLine parser ast)]
      (is (= (.column cl) 11))
      (is (= (.line cl) 1))
      )))


