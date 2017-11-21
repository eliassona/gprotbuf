(ns gprotbuf.core-test
  (:require [clojure.test :refer :all]
            [gprotbuf.core :refer :all]))






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

(deftest a-reserved
  (is (= [:reserved [:ranges [:range [:intLit [:decimalLit "2"]]] [:range [:intLit [:decimalLit "1" [:decimalDigit "5"]]]] [:range [:intLit [:decimalLit "9"]] "to" [:intLit [:decimalLit "1" [:decimalDigit "1"]]]]]] 
         (parser "reserved 2, 15, 9 to 11;" :start :reserved))))



(def enum "enum EnumAllowingAlias {
  option allow_alias = true;
  UNKNOWN = 0;
  STARTED = 1;
  RUNNING = 2 [(custom_option) = \"hello world\"];
}")
         
(deftest an-enum
  (is (= [:enum [:enumName [:ident [:letter "E"] [:letter "n"] [:letter "u"] [:letter "m"] [:letter "A"] [:letter "l"] [:letter "l"] [:letter "o"] [:letter "w"] [:letter "i"] [:letter "n"] [:letter "g"] [:letter "A"] [:letter "l"] [:letter "i"] [:letter "a"] [:letter "s"]]] [:enumBody [:option "option" [:optionName [:ident [:letter "a"] [:letter "l"] [:letter "l"] [:letter "o"] [:letter "w"] "_" [:letter "a"] [:letter "l"] [:letter "i"] [:letter "a"] [:letter "s"]]] "=" [:constant [:boolLit "true"]] ";"] [:enumField [:ident [:letter "U"] [:letter "N"] [:letter "K"] [:letter "N"] [:letter "O"] [:letter "W"] [:letter "N"]] [:intLit [:octalLit "0"]] ";"] [:enumField [:ident [:letter "S"] [:letter "T"] [:letter "A"] [:letter "R"] [:letter "T"] [:letter "E"] [:letter "D"]] [:intLit [:decimalLit "1"]] ";"] [:enumField [:ident [:letter "R"] [:letter "U"] [:letter "N"] [:letter "N"] [:letter "I"] [:letter "N"] [:letter "G"]] [:intLit [:decimalLit "2"]] [:enumValueOption [:optionName "(" [:fullIdent [:ident [:letter "c"] [:letter "u"] [:letter "s"] [:letter "t"] [:letter "o"] [:letter "m"] "_" [:letter "o"] [:letter "p"] [:letter "t"] [:letter "i"] [:letter "o"] [:letter "n"]]] ")"] [:constant [:strLit [:charValue "h"] [:charValue "e"] [:charValue "l"] [:charValue "l"] [:charValue "o"] [:charValue " "] [:charValue "w"] [:charValue "o"] [:charValue "r"] [:charValue "l"] [:charValue "d"]]]] ";"]]]
         (parser enum :start :enum))))
       


(def message "message Outer {
  option (my_option).a = true;
  message Inner {   
    int64 ival = 1;
  }
  map<int32, string> my_map = 2;
}")



(deftest a-message
  (is (= [:message "message" [:messageName [:ident [:letter "O"] [:letter "u"] [:letter "t"] [:letter "e"] [:letter "r"]]] [:messageBody [:option "option" [:optionName "(" [:fullIdent [:ident [:letter "m"] [:letter "y"] "_" [:letter "o"] [:letter "p"] [:letter "t"] [:letter "i"] [:letter "o"] [:letter "n"]]] ")" "." [:ident [:letter "a"]]] "=" [:constant [:boolLit "true"]] ";"] [:message "message" [:messageName [:ident [:letter "I"] [:letter "n"] [:letter "n"] [:letter "e"] [:letter "r"]]] [:messageBody [:field [:type [:enumType [:enumName [:ident [:letter "i"] [:letter "n"] [:letter "t"] [:decimalDigit "6"] [:decimalDigit "4"]]]]] [:fieldName [:ident [:letter "i"] [:letter "v"] [:letter "a"] [:letter "l"]]] [:fieldNumber [:intLit [:decimalLit "1"]]] ";"]]] [:mapField "map" [:keyType "int32"] "," [:type [:enumType [:enumName [:ident [:letter "s"] [:letter "t"] [:letter "r"] [:letter "i"] [:letter "n"] [:letter "g"]]]]] [:mapName [:ident [:letter "m"] [:letter "y"] "_" [:letter "m"] [:letter "a"] [:letter "p"]]] [:fieldNumber [:intLit [:decimalLit "2"]]] ";"]]] 
         (parser message :start :message))))


         
(deftest a-service
  (is (= [:service "service" [:serviceName [:ident [:letter "S"] [:letter "e"] [:letter "a"] [:letter "r"] [:letter "c"] [:letter "h"] [:letter "S"] [:letter "e"] [:letter "r"] [:letter "v"] [:letter "i"] [:letter "c"] [:letter "e"]]] "{" [:rpc "rpc" [:rpcName [:ident [:letter "S"] [:letter "e"] [:letter "a"] [:letter "r"] [:letter "c"] [:letter "h"]]] "(" [:messageType [:messageName [:ident [:letter "S"] [:letter "e"] [:letter "a"] [:letter "r"] [:letter "c"] [:letter "h"] [:letter "R"] [:letter "e"] [:letter "q"] [:letter "u"] [:letter "e"] [:letter "s"] [:letter "t"]]]] ")" "returns" "(" [:messageType [:messageName [:ident [:letter "S"] [:letter "e"] [:letter "a"] [:letter "r"] [:letter "c"] [:letter "h"] [:letter "R"] [:letter "e"] [:letter "s"] [:letter "p"] [:letter "o"] [:letter "n"] [:letter "s"] [:letter "e"]]]] ")" ";"] "}"] 
        (parser 
          "service SearchService {
              rpc Search (SearchRequest) returns (SearchResponse);
           }" :start :service)
  )))      




(def proto-file 
"syntax = “proto3”;
import public “other.proto”;
option java_package = \"com.example.foo\";
enum EnumAllowingAlias {
  option allow_alias = true;
  UNKNOWN = 0;
  STARTED = 1;
  RUNNING = 2 [(custom_option) = \"hello world\"];
}
message outer {
  option (my_option).a = true;
  message inner {   // Level 2
    int64 ival = 1;
  }
  repeated inner inner_message = 2;
  EnumAllowingAlias enum_field =3;
  map<int32, string> my_map = 4;
}")



 