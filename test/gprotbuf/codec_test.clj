(ns gprotbuf.codec_test
  (:require [clojure.test :refer :all]
            [gprotbuf.codec :refer :all]
            ))

(defn verify-roundtrip [m]
  (is (= m (-> m encode decode))))

(deftest test-type-0
  (verify-roundtrip {1 [{:type 0, :value 255} {:type 0, :value 0xffff}]})
  (verify-roundtrip {1 {:type 0, :value 255}}))


(deftest test-type-1
  (verify-roundtrip {1 [{:type 1, :value 255} {:type 1, :value 0xffff}]})
  (verify-roundtrip {1 {:type 1, :value 255}}))


#_(deftest test-type-2
   (verify-roundtrip {1 {:type 2, :value "anders"}}))


(deftest test-type-5
  (verify-roundtrip {1 [{:type 5, :value 255} {:type 5, :value 0xffff}]})
  (verify-roundtrip {1 {:type 5, :value 255}}))