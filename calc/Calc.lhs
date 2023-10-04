Project 2: Calculator
=====================

For this project, you will implement the guts of a (fancy) calculator.
I have provided you with a simple read-eval-print interface (in
[CalcREPL.hs](CalcREPL.hs)) where the user can type in expressions to
be evaluated.  You will provide a function of type `String -> String`
which accepts the user's input and produces a response.  Of course,
your `String -> String` function should be decomposed into multiple
phases, just like all of the language implementations we have been
considering (such as parsing, pretty-printing, interpreting, and so
on).

Getting started
---------------

- Download the [provided zip file](calc.zip), which should contain several
  files including [`CalcREPL.hs`](CalcREPL.hs),
  [`Calc.lhs`](Calc.lhs), [`Parsing.hs`](../code/Parsing.hs), and a
  few configuration files such as `calc.cabal` and `stack.yaml`.

- Extract the contents of the zip file into a directory.

- You should be able to compile the project by running `stack build` at
  the command line.

- After `stack build` completes successfully (it may take a while the
  first time), run the resulting executable from the command line
  using `stack exec calc`.  You should get a prompt where you can
  enter expressions (though it will not do anything yet).

- Simply rerun `stack build` every time you want to test changes you
  have made to `Calc.lhs`.

- A slightly more advanced trick is to run `stack build --file-watch`
  in one command window, which will automatically rebuild every time a
  source file changes; you can then run `stack exec calc` in a
  separate window.  Remember that after recompiling, you will have to
  exit the calculator and rerun with `stack exec calc` to see the
  changes.

Basic assignment
----------------

Your calculator must support the following features:

- Floating-point numbers (represented as `Double` values)
- Standard arithmetic operations `+`, `-`, `*`, `/`, and `^`
  as well as prefix negation
- Re-display a nicely formatted version of the user's input as
  confirmation
- Display appropriate error messages to the user instead of crashing

For example, a sample interaction with your calculator might look like
this:

    > 2+3
    2.0 + 3.0
      = 5.0
    > (((3*5)  -   9)  + -8.3)
    3.0 * 5.0 - 9.0 + -8.3
      = -2.3000000000000007
    > 2 ^ 2 ^ 2 ^ 2
    2.0 ^ 2.0 ^ 2.0 ^ 2.0
      = 65536.0
    > (3+3)*3
    (3.0 + 3.0) * 3.0
      = 18.0

Extensions
----------

In addition, you should complete **at least one** of the following
extensions (ordered roughly from easier to harder):

1. Add support for the constants $\pi$ and $e$, along with at least
   five functions such as sine, cosine, tangent, log, floor, ceiling,
   round, square root, or absolute value.  For example, a sample
   interaction might look like this:

    ```
    > sin(pi/6)
    sin(π / 6.0)
      = 0.49999999999999994
    > cos(tan(log(abs(-2))))
    cos(tan(log(abs(-2.0))))
      = 0.6744026976311414
    > ((1 + sqrt(5))/2)^2 - 1
    ((1.0 + sqrt(5.0)) / 2.0) ^ 2.0 - 1.0
      = 1.618033988749895
    ```

2. Support for complex numbers.  For example, the user should be able
   to enter expressions like `3 + 2i`.  Note that real numbers should
   never be pretty-printed with an imaginary component, and purely
   imaginary numbers should not be pretty-printed with a real
   component.  For example,

    ```
    > 2
    2.0
      = 2.0
    > 3i
    3.0i
      = 3.0i
    > i + 2
    i + 2.0
      = 2.0 + i
    > 2 + 3i
    2.0 + 3.0i
      = 2.0 + 3.0i
    > (2 + 3i) * (4 + 6i)
    (2.0 + 3.0i) * (4.0 + 6.0i)
      = -10.0 + 24.0i
    > sqrt(2 + 3i)
    sqrt(2.0 + 3.0i)
      = 1.6741492280355401 + 0.8959774761298381i
    ```

    (The last example works only if you have also implemented the
    first extension.)

    You can import the `Complex.Double` module to work with complex
    numbers in Haskell.

    Note there is a slight wrinkle to deal with when parsing a literal
    imaginary value: if you see a number you do not yet know whether
    it will be followed by `i` or not.  The problem is that by
    default, if a parsec parser consumes some input before failing, it
    does *not* backtrack to try re-parsing the same input.  So, as an example,
    something like this:
    ```
    (integer <* reserved "i") <|> integer
    ```
    does *not* work, since if there is an integer not followed by an
    `i`, the first parser will consume the integer before failing to
    find an `i`.

    The solution is that any parser which you would like to backtrack
    can be wrapped in the `try` function.  So
    ```
    try (integer <* reserved "i") <|> integer
    ```
    works as expected: if there is no `i` following an integer and the
    first parser fails, it rewinds the input to the beginning of the
    integer before trying the second parser.

3. Support for units of measurement.  Pick a domain (*e.g.* length,
   mass, time, ...) and allow the user to add units in that domain to their
   calculations.  This is a bit tricky, but it's also really fun when you
   get it to work.  For example:
    ```
    > 1
    1.0
      = 1.0
    > 1 inch
    1.0 in
      = 1.0 in
    > 1 inch + 3 inches
    1.0 in + 3.0 in
      = 4.0 in
    > 1 meter + 1 inch
    1.0 m + 1.0 in
      = 1.0254 m
    > (1 meter + 1 inch) as inches
    (1.0 m + 1.0 in) as in
      = 40.370078740157474 in
    > ((1.6 ft * 700 + 8.1 ft) / 2) as miles
    ((1.6 ft * 700.0 + 8.1 ft) / 2.0) as mi
      = 0.10678412422360248 mi
    > 5 feet * 2 meters
    5.0 ft * 2.0 m
      = Error: tried to multiply values both with units, namely 5.0 ft and 2.0 m
    > 5 km + 6
    5.0 km + 6.0
      = Error: tried to add values with and without units, namely 5.0 km and 6.0
    > (5 km) mi
    5.0 km mi
      = Error: tried to apply units mi to a value that already had units km
    > (5 km) as mi
    5.0 km as mi
      = 3.105590062111801 mi
    > 6 as cm
    6.0 as cm
      = Error: can't convert scalar 6.0 to cm
    ```

    Some hints:

    + It should be possible to add two values with units, with
      conversion as appropriate.  It should be an error to add a value
      with units to a value without units.
    + It should be possible to multiply a value with units by a value
      without units, or vice versa.  It should be an error to multiply
      two values with units.
    + It is an error to do exponentiation with anything other than
      unitless values.
    + You will need to change your interpreter quite a bit: it will
      need to keep track of which values have units attached and which
      do not.  It also now has the possibility of generating a runtime
      error.
    + In the example above, units can be introduced by adding a unit
      to a value as a suffix: this makes a unitless value into a value
      with a unit, or checks that a value with units has the indicated
      units.  Alternatively, a conversion can be indicated by writing
      "as <unit>"; this convets a value with units into the indicated
      units, and is an error for values without units.  See the above
      examples.  This is just a suggestion; you do not have to
      organize your calculator in exactly this way.

4. You should also feel free to propose your own extensions; just be
   sure to run them by me to make sure you choose something with an
   appropriate level of difficulty.

General notes and hints
-----------------------

+ You can use the `reserved` token parser to parse things like
  function names, names of constants or units, *etc.*
+ You can use the `naturalOrFloat` token parser to parse literal
  values that can either be an integer or a floating-point value. Note
  it does not handle negatives; that should be taken care of
  automatically by your prefix negation operator.
+ You can use `fromIntegral` to convert from `Integer` to `Double`.
+ You should use the `parse` function to run your parser.  If it
  returns an error wrapped in a `Left` constructor, you can simply
  call `show` on the resulting error to turn it into a `String`
  appropriate for displaying to the calculator user.
+ The `parseSome` function can be used as before for experimenting
  with parsers in GHCi.
+ Exponentiation for `Double` values in Haskell is done with the
  `(**)` operator.  (The `(^)` operator is only for integers.)

Grading
-------

The **maximum** grade for a project that implements only the "Basic
assignment" section above is a B.

Projects will be graded on the following criteria:

* Correctness: 50\%

    Does the calculator work as advertised?  Are the results accurate?
    Does it always give an appropriate error message instead of crashing?
    Is the pretty-printing accurate? *etc.*

* Style: 25\%

    Is the implementation well decomposed into individual pieces that
    each do a single job?  Does it use pattern-matching as appropriate
    (instead of *e.g.* functions like `fst`, `snd`, `head`, *etc*.)?
    Is the calculator interface itself stylish---does it use correct
    grammar, nicely formatted output, *etc.*?

* Documentation: 25\%

    Is the calculator implementation well documented with comments
    explaining the purpose and function of the different parts?  Do
    the calculator startup message and help messages give an accurate
    description of the calculator features and how to use them? *etc.*

Starter code
------------

> {-# LANGUAGE GADTs #-}
>
> module Calc where
>
> import           Parsing2
> import qualified Data.Map as M

Edit this description and replace it with your own!  It gets printed
when the calculator interface first starts up.

> description :: String
> description = unlines
>   [ "Welcome to my calculator."
>   , "This boring message is being shown because"
>   , "I have not bothered to update it."
>   , "Features this calculator supports: none."
>   , "Type an expression, :help, or :quit."
>   ]

Edit this help message and replace it with your own! It gets printed
when the user types `:help`.  Adding some well-chosen examples could
be a good way to concisely show off the different features of your
calculator.

> helpMsg :: String
> helpMsg = unlines
>   [ "You can use integers or floating point values,"
>   , "negation, or standard arithmetic operators + - * / ^ ."
>   ]

This is the main function that is called by `CalcREPL` to evaluate
user input.

> calc :: String -> String
> calc input = "Implement me!"
