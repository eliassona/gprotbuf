(ns gprotbuf.core-test
  (:require [clojure.test :refer :all]
            [gprotbuf.core :refer :all]))

(deftest a-service
  (is (= [:service "service" [:serviceName [:ident [:letter "S"] [:letter "e"] [:letter "a"] [:letter "r"] [:letter "c"] [:letter "h"] [:letter "S"] [:letter "e"] [:letter "r"] [:letter "v"] [:letter "i"] [:letter "c"] [:letter "e"]]] "{" [:rpc "rpc" [:rpcName [:ident [:letter "S"] [:letter "e"] [:letter "a"] [:letter "r"] [:letter "c"] [:letter "h"]]] "(" [:messageType [:messageName [:ident [:letter "S"] [:letter "e"] [:letter "a"] [:letter "r"] [:letter "c"] [:letter "h"] [:letter "R"] [:letter "e"] [:letter "q"] [:letter "u"] [:letter "e"] [:letter "s"] [:letter "t"]]]] ")" "returns" "(" [:messageType [:messageName [:ident [:letter "S"] [:letter "e"] [:letter "a"] [:letter "r"] [:letter "c"] [:letter "h"] [:letter "R"] [:letter "e"] [:letter "s"] [:letter "p"] [:letter "o"] [:letter "n"] [:letter "s"] [:letter "e"]]]] ")" ";"] "}"] 
        (parser 
          "service SearchService {
              rpc Search (SearchRequest) returns (SearchResponse);
           }" :start :service)
  )))



(def message "message Outer {
  option (my_option).a = true;
  message Inner {   // Level 2
    int64 ival = 1;
  }
  map<int32, string> my_map = 2;
}")

(deftest a-message
  (parser message))