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
  message Inner {   
    int64 ival = 1;
  }
  map<int32, string> my_map = 2;
}")



(deftest a-message
  (is (= [:message "message" [:messageName [:ident [:letter "O"] [:letter "u"] [:letter "t"] [:letter "e"] [:letter "r"]]] [:messageBody [:option "option" [:optionName "(" [:fullIdent [:ident [:letter "m"] [:letter "y"] "_" [:letter "o"] [:letter "p"] [:letter "t"] [:letter "i"] [:letter "o"] [:letter "n"]]] ")" "." [:ident [:letter "a"]]] "=" [:constant [:boolLit "true"]] ";"] [:message "message" [:messageName [:ident [:letter "I"] [:letter "n"] [:letter "n"] [:letter "e"] [:letter "r"]]] [:messageBody [:field [:type [:enumType [:enumName [:ident [:letter "i"] [:letter "n"] [:letter "t"] [:decimalDigit "6"] [:decimalDigit "4"]]]]] [:fieldName [:ident [:letter "i"] [:letter "v"] [:letter "a"] [:letter "l"]]] [:fieldNumber [:intLit [:decimalLit "1"]]] ";"]]] [:mapField "map" [:keyType "int32"] "," [:type [:enumType [:enumName [:ident [:letter "s"] [:letter "t"] [:letter "r"] [:letter "i"] [:letter "n"] [:letter "g"]]]]] [:mapName [:ident [:letter "m"] [:letter "y"] "_" [:letter "m"] [:letter "a"] [:letter "p"]]] [:fieldNumber [:intLit [:decimalLit "2"]]] ";"]]] (parser message :start :message))))


(deftest an-import
  (is (= [:import "import" "public" [:strLit [:charValue "o"] [:charValue "t"] [:charValue "h"] [:charValue "e"] [:charValue "r"] [:charValue "."] [:charValue "p"] [:charValue "r"] [:charValue "o"] [:charValue "t"] [:charValue "o"]]] 
         (parser "import public \"other.proto\";" :start :import))))


(deftest a-package 
  (is (= [:package "package" [:fullIdent [:ident [:letter "f"] [:letter "o"] [:letter "o"]] "." [:ident [:letter "b"] [:letter "a"] [:letter "r"]]] ";"]
         (parser "package foo.bar;" :start :package))))


(def one-of "oneof foo {
    string name = 4;
    SubMessage sub_message = 9;
}")


(deftest a-one-of 
  (is (= [:oneof "oneof" [:oneofName [:ident [:letter "f"] [:letter "o"] [:letter "o"]]] [:oneofField [:type [:enumType [:enumName [:ident [:letter "s"] [:letter "t"] [:letter "r"] [:letter "i"] [:letter "n"] [:letter "g"]]]]] [:fieldName [:ident [:letter "n"] [:letter "a"] [:letter "m"] [:letter "e"]]] [:fieldNumber [:intLit [:decimalLit "4"]]]] [:oneofField [:type [:enumType [:enumName [:ident [:letter "S"] [:letter "u"] [:letter "b"] [:letter "M"] [:letter "e"] [:letter "s"] [:letter "s"] [:letter "a"] [:letter "g"] [:letter "e"]]]]] [:fieldName [:ident [:letter "s"] [:letter "u"] [:letter "b"] "_" [:letter "m"] [:letter "e"] [:letter "s"] [:letter "s"] [:letter "a"] [:letter "g"] [:letter "e"]]] [:fieldNumber [:intLit [:decimalLit "9"]]]]] (parser one-of :start :oneof))))