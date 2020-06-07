# Languages All the Way Down

This document is the outline for my ZuriHac 2020 talk _Languages All the Way Down_.


## Introduction

- me
- languages?
- law of the implement

- will define algebraic effects & effect handlers later

- effects & handlers are useful for pragmatic software engineering
- effects & handlers allow us to view application architecture & design through the lens of language design


## Haskell <3

- laziness
- pure functional programming
- types


## Functional programming <3

- function composition
- modularity
- more complicated control flow
  - errors
  - state
- Kleisli composition


## Effects, informally

- for now, effects ~= side effects
- “interesting” control flow
  - errors
  - state
  - nondet


## Partial approaches

- `IO`
  - kitchen sink
  - exception leaking
  - state leaking
- monads: `Either e`, `(,) w`, &c.
  - not composable
- monad transformers: `ExceptT`, `StateT`, &c.
  - brittle: `lift` ordering, changes leak everywhere

- `lift` brittleness hints at problem w/ coupling
- pragmatic problems with coupling:
  - resistant to change
  - testing
  - concurrency


## Desired properties

- modularity
- composability
- abstraction


## Effects, more formally

- an effect defines a set of operations as an interface
- a handler gives implementations for each operation within a specific scope
- `transformers` doesn’t qualify
- `mtl` does


## Abstraction

- separation of interface and implementation
- allows clear delineation between the code we wish to write & the code we wish to execute
- allows distinct execution strategies which can be selected arbitrarily at compile time or runtime
- compilers!
