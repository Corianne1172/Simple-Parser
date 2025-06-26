# IMP Language Interpreter (Haskell)

A simple interpreter and parser for an educational language called **IMP**, implemented in Haskell. This was developed as part of a course assignment to demonstrate recursive descent parsing, expression evaluation, and small language design.

## 🧠 Features

- Custom parser combinators using `StateT String (Either String)`
- Parses and evaluates a toy language (IMP) with:
  - Integer literals and variables
  - Arithmetic and comparison operators
  - Assignment statements
  - `if-then-else` conditionals
  - `while` loops
- Full test coverage using `Hspec`

## 📁 Project Structure

- `src/Parser.hs` — Core parser combinator library
- `src/IMP.hs` — AST definitions, parser for IMP syntax, and evaluator
- `test/IMPSpec.hs` — Tests for the IMP parser and interpreter
- `test/ParserSpec.hs` — Tests for base parser combinators

## 🚀 Running the project

1. **Clone the repo**
   ```bash
   git clone https://github.com/Corianne1172/mp4-imp-interpreter.git
   cd mp4-imp-interpreter
   ```

2. **Build and test with Stack**
   ```bash
   stack build
   stack test
   ```

## 📝 Example Input

```plaintext
x = 10 ;
y = x + 2 ;
if y > 10 then z = 1 ; else z = 0 ; end ;
```

## 🛠 Technologies

- Haskell (GHC 9.4.8)
- Stack
- Hspec (unit testing)

## 🙋🏽‍♀️ Author

Otioh Konan — [@Corianne1172](https://github.com/Corianne1172)

## 📚 Disclaimer

This project was originally created as part of coursework for CS 340 at Illinois Tech. All code shared here is my own work and is intended for educational and portfolio purposes only.
