# whilelang_par

This folder contains the parallel version of the While language reversible interpreter.

## Overview

As the section title suggests, this is just an overview; implementation details can be seen on [Github pages](https://yagotzirck.github.io/whilelang-reverse/) and in the comments inside source files.

The interpreter consists of an executable which interprets a While language file passed as a parameter; when the executable is launched, the following steps are performed:

1. The *lexer* converts the language keywords into tokens, which are then fed to the *parser* which builds the **sigma store** (that is, the variables declared at the beginning of the script) and the **abstract syntax tree** (the program statements/constructs converted into a format the interpreter can interpret unambiguously);

2. The abstract syntax tree (which we'll call **AST** from now on) created by the parser  isn't suitable for reverse execution yet, in fact whenever a construct/statement is read and we proceed to the next one there's no way to go back.
Therefore, we need to *annotate* the **AST** by converting it to a *zipper* (read Ast_ann module documentation to understand what a zipper is and how it works), associating a **statement stack** to each statement (except for meta-statements) in order to keep track of statement execution order, and adding several other *meta-statements* that define the boundaries for the main program and the programs inside *Par* statements;

3. The annotated **AST**, the **sigma store** (**sigma**, from now on) and an empty **delta store** are used to create the initial **state** (which constitutes the core of the interpreter, as we'll see);

4. *Parallelism* is achieved by using *threads*: each thread acts as a program container, associating other info to the program such as the thread ID, the ID of the parent thread which created it, and whether its program belongs to the first or the second item of the **Par** statement contained in the parent thread from which the thread itself was generated.
The **state** contains (among the other things) a list of **running threads** and a list of **waiting threads**: the initial state begins with a root thread in the running threads list containing the main program, and as execution moves forward, each statement is annotated with an integer counter (which we'll refer to as **stmt_counter** from now on) which increases by 1 at each step.
Whenever a **Par** statement is executed, the thread where the statement has been encountered is moved to the **waiting threads** and two new threads are created in the **running threads** list, in order to execute the programs contained inside the **Par** statement itself; when a child thread's program's last statement is executed, the program is used to update its previous version in the parent thread's **Par** statement (since it contains the statement annotations and the current statement is now the last rather than the first one), and as soon as both child threads are done, the parent thread is resumed from the waiting threads and moved back to the running threads, in order to continue its execution to the statement coming after the **Par** statement.

5. As for **reverse execution**: in order to perform each step, the value (**stmt_counter** -1) is searched among all programs' last executed statement stacks contained in both running and waiting threads, and in waiting threads' current **Par** statements: the found statement is then executed in reverse mode, the value on top of its statement stack gets popped (removed), and **stmt_counter** gets decremented by one.

6. The **state** is passed to a looping manager function which takes care of asking the user which action to execute (including: printing the state, printing a thread's program, performing forward execution for a specific thread for a given amount of steps, and performing reverse execution for a given amount of steps) and acting accordingly.



## Usage

### Main executable

Open a terminal inside this folder (`<project root>/parallel`) and type

    dune build src/bin/whilelang_par.exe

The executable can be retrieved at (starting from this directory):

    _build/default/src/bin/whilelang_par.exe

Once you've got it, launch it from a terminal by typing

    ./whilelang_par.exe <While language source file>
You can see some While language source examples inside the folder `src/while_prgs/`.

### While language syntax

The syntax for a While language is rather simple: a While source file begins by defining the variables inside a block delimited by keywords `sigmadef` and `end`, for example:

	sigmadef
		var_1 = 6;
		var_2 = 10;
		var_3 = 50;
	end

It's important to define all variables used by the program inside the `sigmadef` block specified above; any attempt to use a variable inside the program that wasn't defined in there would result in an `Unbound_variable` exception being raised at runtime.

A variable's name **must begin with a letter** (either uppercase or lowercase), and can contain **letters**, **numbers** and **underscores**; also, only integer values can be assigned to a variable.

Statements consist of the following operations:
- Destructive assignments: `var_name = (integer-expression);`
- Constructive assignments: `var_name += (integer-expression);` and `var_name -= (integer-expression);`
- Skip statement: `skip`
- **Par** statement: 
	
	
        {
          <statements' list for parallel program 1>
        }
        par
        {
          <statements' list for parallel program 2>
        }

An `<integer-expression>` consists of the following operations/constructs:
- Integer constants
- Variables (previously defined in the `sigmadef` block at the beginning of the program)
- Sum (`+` operator)
- Subtraction (`-` operator)

### Generating Ocamldoc files
Open a terminal inside this folder (`<project root>/serial`) and type

    dune build @doc-private
The generated doc files can then be retrieved at (starting from this directory):

    _build/default/_doc
