Module 04: Syntax and semantics
===============================

* Write your team names here:

* You may again choose whoever you want to start as the driver.  Write
  your choice here:


**Remember**:

+ You should cite at least two sources on each assignment.  If you
  don't have anything to cite, go learn something and cite it.
+ Be sure that your module loads into GHCi with no errors before
  turning it in.
+ Write in complete sentences, with capital letters and punctuation.
  Presentation matters!

> {-# LANGUAGE GADTSyntax #-}
>
> import Data.Char   -- so you can use the 'isDigit' function later

Some examples
-------------

**Example 1**

    <mirror> ::= '.'
               | 'L' <mirror> 'R'

Example strings that match `<mirror>`:

    "."
    "L.R"
    "LL.RR"

Example strings that do not match `<mirror>`:

    "Q"
    "LR"
    "LL.RRR"

**Example 2**

    <tree> ::= '#'
             | '(' <tree> ')'
             | '(' <tree> <tree> ')'

Example strings that match `<tree>`:

    "(##)"
    "((#)(#(##)))"

Example strings that do not match `<tree>`:

    "((#)"
    "(###)"

**Example 3**

    <digit>   ::= '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9'
    <natural> ::= <digit> | <digit> <natural>
    <integer> ::= <natural> | '-' <natural>

Example strings that match `<integer>`:

    "0023"
    "-25"

Example strings that do not match `<integer>`:

    "x27"
    "--25"
    "4-2"

* For each of `<mirror>`, `<tree>`, and `<integer>`, give three more
  examples of strings that match, and three more examples that do not
  match.

* What does `|` mean?  (Note for this question and the next: this is
  **not** Haskell syntax!  Just say what you think these notations mean
  based on the examples above.)

* What is the difference between something in single quotes (like '*')
  and something in angle brackets (like `<tree>`)?

The things in single quotes are usually called *terminals*, and the
things in brackets are *nonterminals*.  These sorts of definitions are
known as (context-free) *grammars*, written in **Backus-Naur Form** or
**Backus Normal Form** (BNF), named for John Backus and Peter Naur.

* In what context was BNF first developed?

* What else are John Backus and Peter Naur known for?

Now, back to your regularly scheduled grammars...

* Does `<natural>` match the empty string ""?

* An alternative, equivalent way to define `<natural>` is as follows:

        <natural> ::= <digit>+

    Given that this is equivalent to the original definition, what do
    you think + means?

Technically this sort of + notation was not included in the original
form of BNF, but it is a common extension.

* `<natural> ::= <digit>*` would match all the same strings as
  `<digit>+`, but _also_ matches the empty string.  What do you
  think `*` means in this context?

* Describe how to modify the definition of `<natural>` so it does not
  allow unnecessary leading zeroes.  That is, `"203"` should still match
  but `"0023"` should not; however, `"0"` should still be a valid
  `<natural>`. If you wish, you can also introduce more definitions or
  modify definitions besides `<natural>`.

* Write down a grammar (as concisely as possible) that matches all these
  strings:

        "(XX)Y"
        "(XXXX)YZ"
        "(XX)ZZZZZ"
        "(XXXXXX)YZZ"

    ...but does not match any of these strings:

        "()Y"
        "(XXX)YZ"
        "(X)ZZZ"
        "(XX)YY"

![](../images/stop.gif)

Mirror, mirror
--------------

* **ROTATE ROLES** and write the name of the new driver here:

> {-
>    <mirror> ::= '.'
>               | 'L' <mirror> 'R'
> -}
>
> data Mirror where
>   Middle :: Mirror
>   Layer  :: Mirror -> Mirror
>   deriving Show
>
> prettyMirror :: Mirror -> String
> prettyMirror Middle    = "."
> prettyMirror (Layer m) = "L" ++ prettyMirror m ++ "R"
>
> parseMirror :: String -> (Mirror, String)
> parseMirror ('.' : rest) = (Middle, rest)
> parseMirror ('L' : rest) =
>   case parseMirror rest of
>     (m, 'R' : rest') -> (Layer m, rest')

* Write down three different example values of type `Mirror`.

* Try calling `prettyMirror` on your example `Mirror` values above, and
  record the results.

* For this language, how are the concrete syntax (represented by the
  grammar `<mirror>`) and abstract syntax (represented by the data type
  `Mirror`) different?

* Try calling `parseMirror` on five different example inputs.  An
  example input can be *any* `String`.  Try to pick a variety of
  examples that show the range of behavior of `parseMirror`.  Record
  the results here.

* Describe the behavior of `parseMirror`.  Your answer should refer to
  the grammar `<mirror>`.

* Why does `parseMirror` return a `(Mirror, String)` pair instead of
  just a `Mirror`? (*Hint*: if you are not sure, try writing a function
  `parseMirror2 :: String -> Mirror` which behaves the same as
  `parseMirror` but does not return the extra `String`.)

* Modify `parseMirror` so that it has type `String -> Maybe (Mirror,
  String)` and never crashes.  Instead of crashing on inputs that do not
  match `<mirror>`, it should return `Nothing`.  Call your modified
  function `parseMirrorSafe` and write it below.

![](../images/stop.gif)

BIN (aka Your First Language)
------------------------------

* **ROTATE ROLES** and write the name of the new driver here:

Consider the following BNF grammar:

    <bin> ::= '#'
            | '(' <bin> <bin> ')'

* Write an algebraic data type called `Bin` which corresponds to
  `<bin>`.  That is, `Bin` should encode the abstract syntax trees
  corresponding to the concrete syntax `<bin>`.

* Write a function `prettyBin` which turns an abstract syntax tree into
  concrete syntax.

* Write a function `parseBin :: String -> Maybe (Bin, String)` which
  turns concrete syntax into abstract syntax.

We can give a *semantics* (meaning) to abstract `Bin` trees as
follows: a leaf has value 1; the value of a branch with left and right
subtrees is the value of the left subtree plus twice the value of the
right subtree.

* Write a function `interpBin :: Bin -> Integer` which implements this
  semantics.

We can think of this as a little programming language for computing
integers; let's call it BIN.  (Of course, BIN is not a very useful
language, but don't let it hear you say that because you might hurt
its feelings.)  For example, `"((##)(##))"` is a program to compute
the value 9.  `parseBin` is a *parser* for the language, and `interp`
is an *interpreter*.

* Put the pieces together to create a function `evalBin :: String ->
  Maybe Integer`. For example,

        evalBin "#"          --> Just 1
        evalBin "((##)(##))" --> Just 9
        evalBin "(##"        --> Nothing

* At the beginning of class we considered three different sources of
  error in a programming language.  Which kind(s) of errors are
  possible in BIN?

* Why can't the other kind(s) of errors occur in BIN?

![](../images/green.png)

EBIN
----

* **ROTATE ROLES** and write the name of the new driver here:

The language EBIN (which stands, of course, for *extended* BIN) is
just like BIN except it also allows single digits, which can stand in
for an entire tree.  In other words, a digit can go anywhere a `#` can
go in a BIN program.  For example, `"((4#)(#2))"` is a valid EBIN
program.  Note that `"((##)(42))"` is also a valid EBIN program, which
does not contain the number 42 but rather the single digits `4` and
`2`.

The digit `n` stands for a full binary tree of height `n`.  For
example,

+ `0` stands for `#`
+ `1` stands for `(##)`
+ `2` stands for `((##)(##))`

and so on.  Thus, EBIN is not a completely new, separate language, but
just adds some *syntax sugar* to BIN.  That is, single digits are just
a convenient shorthand for more cumbersome but equivalent expressions
in BIN.  We define EBIN in terms of BIN, and we can think of EBIN
programs as abbreviated BIN programs.

* Write down a BNF grammar for EBIN.

* Make an algebraic data type `EBin` to represent EBIN abstract syntax.

* Write a parser for EBIN. (*Hints*: you may find the `isDigit` and
  `read` functions useful.  Note that `Data.Char` was imported at the
  beginning of the module so you can use `isDigit`.  You are always
  welcome to import other library modules as needed. `read` has a more
  general type but for the purposes of this assignment you can think
  of it as having type `String -> Integer`; for example, `read "34" =
  34`.)

* Write a function `desugar :: EBin -> Bin` which *desugars* EBIN
  abstract syntax into equivalent BIN abstract syntax.  (*Hint*: you
  will probably find it useful to write a separate function `grow ::
  Integer -> Bin` which converts an integer into a full tree of the
  given height.)

* Could you also do the desugaring phase *before* the parsing phase?
  If so, what would be the type of `desugar`?

* Now put all the pieces together to define an evaluator `evalEBin ::
  String -> Maybe Integer`.  That is, `evalEBin` should encompass the
  following phases:

    Concrete syntax $\rightarrow$ *parse* $\rightarrow$ EBIN AST $\rightarrow$ *desugar* $\rightarrow$ BIN AST $\rightarrow$ *interpret* $\rightarrow$ Integer

    For example, `evalEBin "(#2)"` should yield 19, and `"((4#)(#2))"`
    should evaluate to 121.

As an (optional) fun aside, make a conjecture about the value of EBIN
programs consisting of a single digit.  That is, what can you say
about `evalEBin "0"`, `evalEBin "1"`, `evalEBin "2"`, and so on?  Can
you prove your conjecture?
