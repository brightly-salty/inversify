# inversify

Parsing and printing in a single syntax description

Inversify uses the ideas from https://www.informatik.uni-marburg.de/~rendel/unparse/  to implement a featureful parser and pretty-printer from an invertible syntax description.

To use this library, first create a grammar by writing an expression of type `Syntax d => d a` where `a` is the type you are parsing or pretty-printing.

Then, you can parse a value of this grammar with the expression `parse grammar string`, which will return a value of type `Maybe a`. You can print a value of this grammar by `print grammar value`, which will return a value of type `Maybe Text`. 

### To-do list

- Add user state/user-defined tokens to the parser
- More combinators
- Add better error messaages, perhaps through labels to the parser