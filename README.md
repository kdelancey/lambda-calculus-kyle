CMSC498V - Lambda Calculus
Written By Kyle DeLancey

## To Install
With Simply-Typed-Lambda-Calc.cabal present with source files:
> cabal install

... To get dependencies.

## Goals
- To implement a simply typed lambda calculus:
    - parser (lowest goal)
    - type-checker (highest goal)
    - interpreter

## What it looks like
It uses the Regex.Applicative library to create extremely legible parsing on an input string, and creates a syntax tree of various matched operations, variables, and type declarations.

## Example output of STParser

Match Name/Variable Identifier
>"nameOfAVariable" =~ name
>
>Just "nameOfAVariable"

Match Type
>":Int" =~ type'
>
>Just Number

Variable Declarations:
>"nameOfAVariable:Int" =~ variableDec
>
>Just (Variable "nameOfAVariable" Number)

Variable Declarations:
>"& a   :   Int   b:Int -> " =~ variableDecs
>
>Just [Variable "a" Number,Variable "b" Number]

Expression:
>"a * b" =~ expr
>
>Just (Mult (Var (Variable "a" X)) (Var (Variable "b" X)))

Single Lambda:
>"& a   :   Int   b:Int -> a * b" =~ lambda
>
>Just (Lambda [Variable "a" Number,Variable "b" Number] [Mult (Var (Variable "a" X)) (Var (Variable "b" X))])

Nested Lambdas:
>"& a:Int   b:Int -> & x:Int y:Int -> x + y a * b" =~ lambda
>
>Just (Lambda [Variable "a" Number,Variable "b" Number] [ILmbd (Lambda [Variable "x" Number,Variable "y" Number] [Add (Var (Variable "x" X)) (Var (Variable "y" X)),Mult (Var (Variable "a" X)) (Var (Variable "b" X))])])