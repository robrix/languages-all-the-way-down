# Languages All the Way Down

This document is the outline for my ZuriHac 2020 talk _Languages All the Way Down_.


## Introduction

- me
- languages?

- caveats
  - law of the implement/“everything looks like a compilers problem”
  - assume some familiarity with monads/monad transformers
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
- also, effects ~= monads


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
  - polymorphism
    - cf typeclasses; dependency injection; inversion of control


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


## Effects as languages

- language design is concerned with syntax and semantics
  - syntax gives structure of expressions
    - structure ~ symbols & where they can occur (e.g. types) ~ operations
  - semantics assigns them meaning
    - “operational semantics”: meaning ~ control flow ~ implementation
- literature on effects often refers to operations as defining syntax and handlers as defining semantics
- syntax/semantics ~= interface/implementation
- effect/handler ~= language


## “Language” scope

- “language” sounds really big! Haskell, Go, etc.
- “domain specific language” is closer
- but SQL is a DSL, still really big!
- “embedded domain specific language” is closer
- but Rails DSLs can still be pretty big
- aside: Chomsky/universal grammar/“merge” operator; language ~ recursive combination

- effects can be factored arbitrarily small
  - `Error` ~ the language of catchable failures with an error
  - `Reader` ~ the language of a single locally-configurable parameter
  - `State` ~ the language of a single mutable variable
  - aside: “the” rather than “a” is notional; might be other expressions of same concept


## Costs of abstraction

- indirection
- potentially, efficiency
- semantics; assumptions


## Laws

- `put`’s type doesn’t constrain the use of the parameter
- assumptions of relationship between `get` & `put` are key to using `State`
  - e.g. using `State` to guard recursive graph traversal
  - if `put` silently drops writes, cyclic graph -> divergence
- laws relate operations of an effect to one another

- refined definition of effect:
  - effect specifies syntax (interface) & _relationships between them (laws)_
  - handler specifies semantics (implementation) _satisfying these laws_

- laws also relate to control flow: `return` & `>>=`

- examples:
  - `State`
    - handler for `put` cannot drop writes
  - `Error`

- NB: simplest expression of laws may not be obvious; start with desired properties


## Modularity

- laws enable greater modularity
  - reason/test in isolation
  - apply systematically
- express behaviour as equations
- typically relate effects to “canonical” handlers
- admit _useful_ variance in implementations
- admit interactions with other effects


## Application

- identify suitable portions of domain
  - orthogonal concept/behaviour
  - reasoning/testing in isolation
  - multiple implementation strategies
- phrase important concepts in domain using effects
