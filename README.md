# gprotbuf

A Clojure library for parsing Google Protocol Buffers proto3

## Usage
Add the following line to your leinigen dependencies:
```clojure
[gprotbuf "0.1.0-SNAPSHOT"]
```
```clojure
=> (use 'gprotbuf.core)
nil
```
```clojure

=> (def proto-file 
"syntax = \"proto3\";
import public \"other.proto\";
option java_package = \"com.example.foo\";
enum EnumAllowingAlias {
  option allow_alias = true;
  UNKNOWN = 0;
  STARTED = 1;
  RUNNING = 2 [(custom_option) = \"hello world\"];
}
message outer {
  option (my_option).a = true;
  message inner {   
    int64 ival = 1;
  }
  repeated inner inner_message = 2;
  EnumAllowingAlias enum_field =3;
  map<int32, string> my_map = 4;
}")
nil

=> (parse proto-file)
[:proto [:syntax [:quote "\""] "proto3" [:quote "\""]] [:import "import" "public" [:strLit [:charValue "o"] [:charValue "t"] [:charValue "h"] [:charValue "e"] [:charValue "r"] [:charValue "."] [:charValue "p"] [:charValue "r"] [:charValue "o"] [:charValue "t"] [:charValue "o"]]] [:option "option" [:optionName [:ident [:letter "j"] [:letter "a"] [:letter "v"] [:letter "a"] "_" [:letter "p"] [:letter "a"] [:letter "c"] [:letter "k"] [:letter "a"] [:letter "g"] [:letter "e"]]] "=" [:constant [:strLit [:charValue "c"] [:charValue "o"] [:charValue "m"] [:charValue "."] [:charValue "e"] [:charValue "x"] [:charValue "a"] [:charValue "m"] [:charValue "p"] [:charValue "l"] [:charValue "e"] [:charValue "."] [:charValue "f"] [:charValue "o"] [:charValue "o"]]] ";"] [:topLevelDef [:enum [:enumName [:ident [:letter "E"] [:letter "n"] [:letter "u"] [:letter "m"] [:letter "A"] [:letter "l"] [:letter "l"] [:letter "o"] [:letter "w"] [:letter "i"] [:letter "n"] [:letter "g"] [:letter "A"] [:letter "l"] [:letter "i"] [:letter "a"] [:letter "s"]]] [:enumBody [:option "option" [:optionName [:ident [:letter "a"] [:letter "l"] [:letter "l"] [:letter "o"] [:letter "w"] "_" [:letter "a"] [:letter "l"] [:letter "i"] [:letter "a"] [:letter "s"]]] "=" [:constant [:boolLit "true"]] ";"] [:enumField [:ident [:letter "U"] [:letter "N"] [:letter "K"] [:letter "N"] [:letter "O"] [:letter "W"] [:letter "N"]] [:intLit [:octalLit "0"]]] [:enumField [:ident [:letter "S"] [:letter "T"] [:letter "A"] [:letter "R"] [:letter "T"] [:letter "E"] [:letter "D"]] [:intLit [:decimalLit "1"]]] [:enumField [:ident [:letter "R"] [:letter "U"] [:letter "N"] [:letter "N"] [:letter "I"] [:letter "N"] [:letter "G"]] [:intLit [:decimalLit "2"]] [:enumValueOption [:optionName "(" [:fullIdent [:ident [:letter "c"] [:letter "u"] [:letter "s"] [:letter "t"] [:letter "o"] [:letter "m"] "_" [:letter "o"] [:letter "p"] [:letter "t"] [:letter "i"] [:letter "o"] [:letter "n"]]] ")"] [:constant [:strLit [:charValue "h"] [:charValue "e"] [:charValue "l"] [:charValue "l"] [:charValue "o"] [:charValue " "] [:charValue "w"] [:charValue "o"] [:charValue "r"] [:charValue "l"] [:charValue "d"]]]]]]]] [:topLevelDef [:message "message" [:messageName [:ident [:letter "o"] [:letter "u"] [:letter "t"] [:letter "e"] [:letter "r"]]] [:messageBody [:option "option" [:optionName "(" [:fullIdent [:ident [:letter "m"] [:letter "y"] "_" [:letter "o"] [:letter "p"] [:letter "t"] [:letter "i"] [:letter "o"] [:letter "n"]]] ")" "." [:ident [:letter "a"]]] "=" [:constant [:boolLit "true"]] ";"] [:message "message" [:messageName [:ident [:letter "i"] [:letter "n"] [:letter "n"] [:letter "e"] [:letter "r"]]] [:messageBody [:field [:type [:enumType [:enumName [:ident [:letter "i"] [:letter "n"] [:letter "t"] [:decimalDigit "6"] [:decimalDigit "4"]]]]] [:fieldName [:ident [:letter "i"] [:letter "v"] [:letter "a"] [:letter "l"]]] [:fieldNumber [:intLit [:decimalLit "1"]]] ";"]]] [:field "repeated" [:type [:enumType [:enumName [:ident [:letter "i"] [:letter "n"] [:letter "n"] [:letter "e"] [:letter "r"]]]]] [:fieldName [:ident [:letter "i"] [:letter "n"] [:letter "n"] [:letter "e"] [:letter "r"] "_" [:letter "m"] [:letter "e"] [:letter "s"] [:letter "s"] [:letter "a"] [:letter "g"] [:letter "e"]]] [:fieldNumber [:intLit [:decimalLit "2"]]] ";"] [:field [:type [:enumType [:enumName [:ident [:letter "E"] [:letter "n"] [:letter "u"] [:letter "m"] [:letter "A"] [:letter "l"] [:letter "l"] [:letter "o"] [:letter "w"] [:letter "i"] [:letter "n"] [:letter "g"] [:letter "A"] [:letter "l"] [:letter "i"] [:letter "a"] [:letter "s"]]]]] [:fieldName [:ident [:letter "e"] [:letter "n"] [:letter "u"] [:letter "m"] "_" [:letter "f"] [:letter "i"] [:letter "e"] [:letter "l"] [:letter "d"]]] [:fieldNumber [:intLit [:decimalLit "3"]]] ";"] [:mapField "map" [:keyType "int32"] "," [:type [:enumType [:enumName [:ident [:letter "s"] [:letter "t"] [:letter "r"] [:letter "i"] [:letter "n"] [:letter "g"]]]]] [:mapName [:ident [:letter "m"] [:letter "y"] "_" [:letter "m"] [:letter "a"] [:letter "p"]]] [:fieldNumber [:intLit [:decimalLit "4"]]] ";"]]]]]
```


## License

Copyright © 2017 FIXME

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
