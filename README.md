# inversify

Parsing and printing in a single syntax description

Inversify uses the ideas from https://www.informatik.uni-marburg.de/~rendel/unparse/  to implement a featureful parser and pretty-printer from an invertible syntax description.

To use this library, first create a grammar by writing an expression of type `Syntax p => p t a` where `a` is the type you are parsing or pretty-printing and `t` is the type of token you are working with, usually `Char`.

Then, you can parse a value of this grammar with the expression `parse grammar tokens`, which will return a value of type `[a]` representing the list of possible values that could be parsed. You can print a value of this grammar by `print grammar value`, which will return a value of type `Maybe [t]` where `t` is the token type. 

### To-do list

- Add better error messaages, perhaps through labels to the parser
- More combinators