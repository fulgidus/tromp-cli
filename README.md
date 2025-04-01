# 🧠 Lambda Diagrams Interpreter (Tromp-style) in Haskell

A Haskell interpreter that parses lambda calculus expressions and renders them as **ASCII diagrams** in the style of **John Tromp's Lambda Diagrams**.

![Lambda Diagram Example](https://raw.githubusercontent.com/your-username/lambda-diagrams/main/example.png) <!-- Replace with real image or remove -->

---

## ✨ Features

- ✅ Parses lambda expressions written in standard Haskell notation: `\x.\y.x y`
- ✅ Supports full variable names (`\func.\arg.func arg`)
- ✅ Visualizes lambda abstractions, applications, and variable bindings as ASCII diagrams
- ✅ Handles nested lambdas and multi-argument applications
- ✅ Optional beta-reduction support *(coming soon)*

---

## 🖼 Example

**Input:**

```haskell
\func.\arg.func arg
```

```shell
────func
    │
────arg
    │
    ├──func
    │
    └──arg
```

## 📦 Project Structure

.
├── Main.hs         -- Entry point of the CLI app
├── Parser.hs       -- Megaparsec parser for lambda expressions
├── LambdaAST.hs    -- Lambda calculus data structure (AST)
├── Renderer.hs     -- ASCII rendering engine (Tromp-style diagrams)
└── README.md       -- This file 😄

## 🚀 How to Use

### 📥 Clone the repository
```shell
git clone https://github.com/your-username/lambda-diagrams.git
cd lambda-diagrams
```
### 🛠 Build with GHC
```
ghc Main.hs -o lambda-diagram
./lambda-diagram
```
### 🧑‍💻 Input a lambda expression when prompted

Example:
```
\func.\arg.func arg
```
### ✅ You'll see a diagram rendered directly in your terminal!

