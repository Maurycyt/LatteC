# Table of Contents

1. [LatteC Specification](#lattec-specification)
   1. [Language Specification](#language-specification)
   2. [Aim of this Project](#aim-of-this-project)
2. [Building and Running](#building-and-running)

# LatteC Specification

## Language Specification

The Latte language is a simple Java-like language. The ANTLR syntax of Latte is detailed in [Latte.cf](src/main/resources/Latte.g4). Examples of Latte programs can be found in [the test resources folder](src/test/resources) in `good` and `extensions`.

The core features of Latte include:
- Integer, floating point, boolean and string constants and variables.
- One-dimensional arrays.
- Conditional statements and loops.
- Data structuration via classes with member variables and member functions.
- Class inheritance with method overriding.
- Garbage collection.

A Latte program consists of a list of top-level definitions, each being a function definition or class definition. A definition of a `main` function taking no arguments and returning an integer must be given. As one would expect, `main` is the entry point to the program.

There are five predefined functions: `void printInt(int)`, `void printString(string)`, `void error()`, `int readInt()`, and `string readString()`.

Here is a short example program in Latte:
```java
class A {
    void print() { printString("A"); return; }
}

class B extends A {
    int value;
    void print() { printString("B("); printInt(value); printString(")"); return; }
}

int main() {
    A[] arrayOfA = new A[2];
    arrayOfA[0] = new A;
    B  myB = new B;
    myB.value = readInt();
    arrayOfA[1] = myB;
    
    for(A a : arrayOfA) {
        a.print();
        printString("\n");
    }
    
    return 0;
}
```

For input
```
1
```

Its output should be:
```
A
B(1)
```

## Aim of this Project

The aim of this project is to write a Latte compiler to LLVM. For a Latte program, it should generate LLVM bitcode which, when executed with `lli`, behaves according to the semantics of the language.

Moreover, the compiler must feature:
- Static type checking.
- Function return checking (whether every function necessarily returns).
- Unreachable code reporting (if some code is unreachable due to a proven `return` statement before it).
- Various optimisations, like:
   - Constant expression simplification.
   - Function inlining.
   - And more.

# Building and Running

You need `sbt` installed in your environment.

To build the compiler, simply run `make` in the root directory. This will generate all the BNFC sources and build the compiler itself. It will create one executable file, `latc_llvm`, which takes a file as its only argument and creates the corresponding `.ll` and `.bc` files in the same directory as the given file. To execute the generated files, use `lli`.
