(ns gprotbuf.core-test
  (:use [clojure.pprint])
  (:require [clojure.test :refer :all]
            [gprotbuf.core :refer :all]
            [instaparse.core :as insta]
            [clj-http.client :as client])
  (:import [gprotbuf.exception ParserException])
  )


(deftest an-import
  (is  (not (insta/failure? (parser "import public \"other.proto\";" :start :import)))))


(deftest a-package 
  (is (not (insta/failure? (parser "package foo.bar;" :start :package)))))


(def one-of "oneof foo {
    string name = 4;
    SubMessage sub_message = 9;
}")


(deftest a-one-of (is (not (->  one-of (parser :start :oneof) insta/failure?))))
(deftest a-reserved (is (not (->  "reserved 2, 15, 9 to 11;" (parser :start :reserved) insta/failure?))))

(def enum "enum EnumAllowingAlias {
  option allow_alias = true;
  UNKNOWN = 0;
  STARTED = 1;
  RUNNING = 2 [(custom_option) = \"hello world\"];
}")
         



(deftest an-enum (is (not (-> enum (parser :start :enum) insta/failure?))))
       


(def message "message Outer {
  option (my_option).a = true;
  message Inner {   
    int64 ival = 1;
  }
  map<int32, string> my_map = 2;
}")



(deftest a-message
  (is (not (insta/failure? (parser message :start :message)))))


         
(deftest a-service
  (is  
    (not (insta/failure? (parser 
                           "service SearchService {
          rpc Search (SearchRequest) returns (SearchResponse);
       }" :start :service)
  ))) )     




(def proto-file 
"syntax = \"proto3\";
import public \"other.proto\";
option java_package = \"com.example.foo\";
enum EnumAllowingAlias {
  reserved 15, 9 to 11, 40;
  option allow_alias = true;
  UNKNOWN = 0;
  STARTED = 01;
  RUNNING = 2 [(custom_option) = \"hello world\"];
}
message outer {
  option (my_option).a = true;
  bytes baField = 1;
  message inner {   
    int64 ival = 1;
  }
  repeated inner inner_message = 2;
  EnumAllowingAlias enum_field =3;
  map<int32, string> my_map = 4;
  oneof test_oneof {
     string name = 4;
     SubMessage sub_message = 9;
  }
}
service SearchService {
  rpc Search (SearchRequest) returns (SearchResponse);
}
message SearchRequest {
  string query = 1;
  int32 page_number = 2;
  int32 result_per_page = 3;
  enum Corpus {
    UNIVERSAL = 0;
    WEB = 1;
    IMAGES = 2;
    LOCAL = 3;
    NEWS = 4;
    PRODUCTS = 5;
    VIDEO = 6;
  }
  Corpus corpus = 4;
}
")


(deftest a-proto-file 
  #_(is (not (-> proto-file parse insta/failure?))))

;;parse-block


(defn successful-parse-block [text] 
  (let [ast (parse-block text)]
    (when (not (insta/failure? ast)) 
      (ast->clj ast))))




(defn failed-context-parse-block [text msg line column] 
  (let [ast (parse-block text)]
    (if (not (insta/failure? ast)) 
      (try 
        (ast->clj ast)
        (is false "This proto text should fail")
        (catch ParserException e
          (is (= (.getMessage e ) msg))
          (is (= line (.line e)))
          (is (= column (.column e)))
          ))
      (is false (insta/get-failure ast)))))

(deftest empty-block
  (successful-parse-block "{ syntax=\"proto3\"; };"))

(deftest reserved-to
    (successful-parse-block 
    "{
       syntax=\"proto3\";
       message M1 {
         reserved 1 to 10, 20 to 30;
       }
     };"))


(deftest repeated-type
    (successful-parse-block 
    "{
       syntax=\"proto3\";
       message M1 {
         string str = 1;
         repeated uint32 intList = 2;
       }
     };"))



(deftest verify-that-reserved-cannot-overlap-1 
  (failed-context-parse-block 
    "{
       syntax=\"proto3\";
       message M1 {
         reserved 1;
         reserved 1;
       }
     };" "M1. Fields overlap." #_"Reserved range 1 to 1 overlaps with already-defined range 1 to 1." 5 10) ;TODO
  )

(deftest verify-that-reserved-cannot-overlap-2 
  (failed-context-parse-block 
    "{
       syntax=\"proto3\";
       message M1 {
         reserved 1 to 10, 5 to 15;
       }
     };" "M1. Fields overlap." #_"Reserved range 5 to 15 overlaps with already-defined range 5 to 15." 4 10)) ;TODO

(deftest verify-that-reserved-cannot-overlap-3 
  (failed-context-parse-block 
    "{
       syntax=\"proto3\";
       message M1 {
         reserved 1 to 10, 20 to 25;
         string str1 = 5;
       }
     };" "M1. Fields overlap." #_"Field \"str1\" uses reserved number 5." 5 10)) ;TODO

(deftest verify-that-reserved-cannot-overlap-4 
  (failed-context-parse-block 
    "{
       syntax=\"proto3\";
       message M1 {
         string str1 = 5;
         string str2 = 5;
       }
     };" "M1. Fields overlap." #_"Field number 5 has already been used in \"M1\" by field \"str1\"." 5 10)) ;TODO
(deftest verify-that-reserved-cannot-overlap-5 
  (failed-context-parse-block 
    "{
       syntax=\"proto3\";
       message M1 {
         reserved 5 to max;
         string str1 = 1000;
       }
     };" "M1. Fields overlap." #_"Field \"str1\" uses reserved number 1000." 5 10) ;TODO
  )

(deftest verify-that-19000-to-19999-cannot-be-used
    (failed-context-parse-block 
    "{
       syntax=\"proto3\";
       message M1 {
         string str1 = 19000;
       }
     };" "M1. Fields overlap." #_"Field numbers 19000 through 19999 are reserved for the protocol buffer library implementation." 4 10) ;TODO
  )

(deftest verify-that-19000-to-19999-cannot-be-used-when-alias-is-used
    (failed-context-parse-block 
    "{
       syntax=\"proto3\";
       message M1 {
         option allow_alias = true;
         string str1 = 19000;
       }
     };" "M1. Fields overlap." #_"Field numbers 19000 through 19999 are reserved for the protocol buffer library implementation." 5 10) ;TODO
  )

(deftest verify-that-enum-with-same-value-causes-error
    (failed-context-parse-block 
    "{
       syntax=\"proto3\";
       enum E1 {
          V0 = 0;
          V1 = 1;
          V2 = 1;
       }
     };" "V2. \"V2\" is already defined in \"E1\"." 6 11)
  )

(deftest verify-that-allow-alias-work
    (successful-parse-block 
    "{
       syntax=\"proto3\";
       enum E1 {
          option allow_alias = true;
          V0 = 0;
          V1 = 1;
          V2 = 1;
       }
     };")
  )

(deftest verify-that-empty-enum-causes-error
    (failed-context-parse-block 
    "{
       syntax=\"proto3\";
       enum E1 {
       }
     };" "E1. Enums must contain at least one value." 3 13)
  )

(deftest verify-that-enum-without-zero-causes-error
    (failed-context-parse-block 
    "{
       syntax=\"proto3\";
       enum E1 {
         V1 = 1;
       }
     };" "E1. The first enum value must be zero in proto3." 4 10)
  )
(deftest verify-that-enum-with-same-name-causes-error
    (failed-context-parse-block 
    "{
       syntax=\"proto3\";
       enum E1 {
         V0 = 0;
         V0 = 1;
       }
     };" "V0. \"V0\" is already defined in \"E1\"." 5 10)
  )

(deftest verify-that-fields-with-same-name-causes-error
    (failed-context-parse-block 
    "{
       syntax=\"proto3\";
       message Message1 {
         string str1 = 1;
         string str1 = 2;
       }
     };" "str1. \"str1\" is already defined in \"Message1\"." 4 10)
  )

(deftest verify-that-oneof-field-same-name-causes-error
    (failed-context-parse-block 
    "{
       syntax=\"proto3\";
       message Message1 {
         oneof One {
           string str1 = 1;
           string str1 = 2;
         }
       }
     };" "str1. \"str1\" is already defined in \"Message1\"." 5 12)
  )
(deftest verify-that-oneof-field-same-name-as-in-message-causes-error
    (failed-context-parse-block 
    "{
       syntax=\"proto3\";
       message Message1 {
         oneof One {
           string str1 = 1;
         }
         string str1 = 2;
       }
     };" "str1. \"str1\" is already defined in \"Message1\"." 5 12)
  )
(deftest verify-that-oneof-field-same-value-causes-error
    (failed-context-parse-block 
    "{
       syntax=\"proto3\";
       message Message1 {
         oneof One {
           string str1 = 1;
           string str2 = 1;
         }
       }
     };" "Message1. Fields overlap." 6 12)
  )
(deftest verify-that-oneof-field-same-value-as-in-message-causes-error
    (failed-context-parse-block 
    "{
       syntax=\"proto3\";
       message Message1 {
         oneof One {
           string str1 = 1;
         }
         string str2 = 1;
       }
     };" "Message1. Fields overlap." 7 10)
  )


(def mz-test 
     "{
	syntax = \"proto3\";
	import public \"other.proto\";
	option java_package = \"com.example.foo\";
	enum EnumAllowingAlias {
	  reserved 15, 9 to 11, 40;
	  option allow_alias = true;
	  UNKNOWN = 0;
	  STARTED = 01;
	  RUNNING = 2 [(custom_option) = \"hello world\"];
	}
	message outer {
	  option (my_option).a = true;
	  bytes baField = 1;
	  message inner {   
	    int64 ival = 1;
	  }
	  repeated inner inner_message = 2;
	  EnumAllowingAlias enum_field =3;
	  map<int32, string> my_map = 4;
	  oneof test_oneof {
	     string name = 4;
	     SubMessage sub_message = 9;
	  }
	}
	service SearchService {
	  rpc Search (SearchRequest) returns (SearchResponse);
	}
	message SearchRequest {
	  string query = 1;
	  int32 page_number = 2;
	  int32 result_per_page = 3;
	  enum Corpus {
	    UNIVERSAL = 0;
	    WEB = 1;
	    IMAGES = 2;
	    LOCAL = 3;
	    NEWS = 4;
	    PRODUCTS = 5;
	    VIDEO = 6;
	  }
	  Corpus corpus = 4;
	}
};

//
// Message UDR encoder and decoder
//
in_map Udr_in: external(Udr), target_internal(Udr) {
   automatic;
};

decoder Dec: in_map(Udr_in);

out_map Udr_out: external(Udr), internal(Udr) {
   automatic;
};

encoder Enc: out_map(Udr_out);

//
// Message Container encoder and decoder
//
in_map in2: external(Container), target_internal(Container) {
  automatic: use_external_names {
    Udr: using in_map Udr_in;
  };
};

decoder Dec2: in_map(in2);

out_map out2: external(Container), internal(Container) {
  automatic {
    Udr: using out_map Udr_out;
  };
};

encoder Enc2: out_map(out2);

//
// Message Packed encoder and decoder
//
in_map Packed_in: external(Packed), target_internal(Packed) {
   automatic;
};

decoder Dec3: in_map(Packed_in);

out_map Packed_out: external(Packed), internal(Packed) {
   automatic;
};

encoder Enc3: out_map(Packed_out);
")


(def mz-test1 "
{
	syntax = \"proto3\";
   message Udr {
      bytes baField = 1;
      string strField = 2;
      uint32 intField = 3;
      
      repeated uint32 intList = 4;
      
      sint32 signed32 = 8;
      repeated sint64 signed64 = 9;
      
      bool boolField = 20;
      float floatField = 21;
      double doubleField = 22;
      fixed32 f32field = 23;
      fixed64 f64field = 24;

      int32 int32 = 30;
      int64 int64 = 31;
   }
   
   message Container {
      Udr udrField = 1;
      repeated Udr udrList = 2;
   }
   
   message Packed {
      repeated int32 data = 4 [packed=true];
      int32 stop = 3;
   }
};
")


(defn http-call [url]
  (let [res (client/get url)]
    (if (= (:status res) 200) 
      (-> res :body)
      (throw (IllegalStateException. (pr-str res))))))


;;TODO add more examples to this list
(def gpb-examples-urls ["https://raw.githubusercontent.com/google/protobuf/master/examples/addressbook.proto"])


(deftest examples-from-internet 
  (is (every? complement (map (comp insta/failure? parse http-call) gpb-examples-urls)))) 


