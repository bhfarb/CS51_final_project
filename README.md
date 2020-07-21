## CS51_final_project

Final project for Harvard class CS51: Abstraction and Design in Computer Science taken in 2019. Used OCaml to impliment a similar language called MiniMl that can use lexical and dynamic scoping. See writeup.pdf for more details.

I implemented:
- evaluation.ml - all functions defined within Env module
- expr.ml - all functions
- expr.mli - added `Unit` type
- miniml_lex.mll - added `Unit` token symbol to hash
- env_tests.ml - all tests
- env_tests.ml - all tests
- expr_tests.ml - all tests

Run Instructions: compile with make launch `MiniMl` with `./miniml.byte` run unit tests with `./stests.byte`.
