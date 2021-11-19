# whilelang_serial

This folder contains the serial version of the While language reversible interpreter.

## Overview

As the section title suggests, this is just an overview; implementation details can be seen on [Github pages](https://yagotzirck.github.io/whilelang-reverse/) and in the comments inside source files.

The interpreter consists of an executable which interprets a While language file passed as a parameter; when the executable is launched, the following steps are performed:

1. The *lexer* converts the language keywords into tokens, which are then fed to the *parser* which builds the **sigma store** (that is, the variables declared at the beginning of the script) and the **abstract syntax tree** (the program statements/constructs converted into a format the interpreter can interpret unambiguously);

2. The abstract syntax tree (which we'll call **AST** from now on) created by the parser  isn't suitable for reverse execution yet, in fact whenever a construct/statement is read and we proceed to the next one there's no way to go back.
Therefore, we need to *augment* the **AST** by converting it to a *zipper* (read Ast_aug module documentation to understand what a zipper is and how it works) and adding several other *meta-statements* that define the boundaries for the main program and the programs inside *If* and *While* statements;

3. The augmented **AST**, the **sigma store** (**sigma**, from now on) and an empty **delta store** are used to create the initial **state** (which constitutes the core of the interpreter, as we'll see);

4. The **state** is passed to a looping manager function which takes care of
	- Asking the user how many steps to execute and in which direction (forward with positive values, backward with negative values, and 0 for quitting);
	- Printing the ** state ** each time the steps entered by the user have been executed.


## Usage

### Main executable

Open a terminal inside this folder (`<project root>/serial`) and type

    dune build src/bin/whilelang_serial.exe

The executable can be retrieved at (starting from this directory):

    _build/default/src/bin/whilelang_serial.exe

Once you've got it, launch it from a terminal by typing

    ./whilelang_serial.exe <While language source file>
You can see some While language source examples inside the folder `src/while_prgs/`.

### Generating Ocamldoc files
Open a terminal inside this folder (`<project root>/serial`) and type

    dune build @doc-private
The generated doc files can then be retrieved at (starting from this directory):

    _build/default/_doc

### Tests
I've written 3 test files: one for the **sigma store**, one for the **delta store** and one for the **interpreter**; they're far from being exhaustive (especially the interpreter tests), but better than nothing, I guess :)

To execute each one, open a terminal inside this folder (`<project root>/serial`) and type:

    dune exec src/bin/test_sigma.exe

    dune exec src/bin/test_delta.exe

    dune exec src/bin/test_interpreter.exe