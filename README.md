# gprotbuf

A Java/Clojure library for parsing Google Protocol Buffers proto3

## Usage
###Java

Add the gprotbuf jar to your dependencies.

```java
import gprotbuf.GPBParser;

final Object ast = GPBParser.instance().parse("syntax = \"proto3\";
  message M1 {
    string str1 = 1;
    string str2 = 2;
  }
");

if (!GPBParser.instance().isFailure(ast)) {
  System.out.println(GPBParser.instance().transform(ast));
}
```
```clojure
((:message "M1" ([:field "string" "str1" 1] [:field "string" "str2" 2])))        
```



###Clojure
Add the following line to your leinigen dependencies:
```clojure
[gprotbuf "0.1.0-SNAPSHOT"]
```
```clojure
=> (use 'gprotbuf.core)
=> (require '[instaparse.core :as insta])
nil
```
```clojure

=> (def proto-file 
"syntax = \"proto3\";
  message M1 {
    string str1 = 1;
    string str2 = 2;
  }
")
nil

=> (let [ast (parse proto-file)] ;do parsing
     (when (not (insta/failure? ast))
       (ast->clj ast))) ;transform/clean the ast and validate structure
((:message "M1" ([:field "string" "str1" 1] [:field "string" "str2" 2])))        
```
The returned value is a data structure in the hiccup format.

## License

Copyright © 2017 FIXME

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
