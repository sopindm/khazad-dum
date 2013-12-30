# khazad-dum

## "You shall not PASS!!!" ©Gandalf

A clojure unit test framework. Designed to be minimalistic and usable.

## Usage

Usage pattern is quite simple:

1. Add reference to khazad-dum to your project [khazad-dum "0.1.0"]
2. Require khazad-dum.core namespace in your tests.
3. Write some tests with deftest form.
4. Run tests with run-test, run-tests forms.

## Examples

Let's define some ariphmetics tests:

      (ns ariphmetics-test
        (:require [khazad-dum.core :refer :all]))
      
      (deftest +-test
        (?= (+ 2 2) 4))

      (deftest *-test
        (?= (* 6 6) 36))

      (deftest wrong-+-test ;this one must fall
        (?= (+ 2 2) 5))

Now we can run new tests in REPL:

      (run-test #'+-test)
      >> 1 tests of 1 success
      
      (run-test #'wrong-+-test)
      >> (+ 2 2) is
      >> 4
      >> Expected 5 that is
      >> 5
      >>
      >> ariphmetics-test/wrong-+-test failed
      >> 0 tests of 1 success

      (run-tests 'ariphmetics-test)
      >> (+ 2 2) is
      >> 4
      >> Expected 5 that is
      >> 5
      >>
      >> ariphmetics-test/wrong-+-test failed
      >> 2 tests of 3 success

## API Reference

General functions:

### deftest
Arguments: [name & body]

Defines test with name 'name' and source 'body

### run-test
Arguments: [name]

If 'name' is a variable:
   1. Tries to find test sources
   2. If fails - throws IllegalArgumentException
   3. Otherwise runs got test form

Otherwise treats 'name' as a function and tries to run it.

### run-tests
Arguments: [& namespaces]

Searches gives namespaces for tests and runes them

Predicates:

### ?false
Arguments: [expr]

Checks that expr value is false. Fails otherwise

### ?true
Arguments: [expr]

Checks that expr value is true. Fails otherwise

### ?= 
Arguments: [expr1 expr2]

Checks that 'expr1' value equals to 'expr2' value. Fails otherwise

### ?lines=
Arguments: [expr & lines]

Checks that 'expr' value is equal to string constructed of 'lines'.
Fails otherwis

### ?throws
Arguments: [form exception]
           [form exception message & args]
        
Checks that 'form' throws 'exception'
With 'message' argument checks that exception message equals to `(format ~message ~@ args)` 

## License

Copyright © 2013 sopindm@gmail.com

Distributed under the Apache License version 2.0

