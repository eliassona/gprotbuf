(ns gprotbuf.check_test
  (:require [clojure.test :refer :all]
            [gprotbuf.check :refer :all]))


(deftest verify-rel-types-of 
  (is (= #{"Result", "Result.M2"} 
         (rel-types-of 
         '((:message 
             "Result" 
             ([:field string url 1] [:field string title 2] 
                                    [:field repeated string snippets 3] 
                                    (:message M2 ([:field string f1 1])) 
                                    [:field M2 m2 4])) 
            [:field Result results 1] [:field Result.M2 f2 2]))
  )))