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
  - abstraction
  - modularity
  - systematization

  - recipe:
    - select effectful operations
    - define laws
    - sometimes: write actions against the interface
    - define handlers
    - refine guided by use

- effects & handlers allow us to view application architecture & design through the lens of language design
  - syntax/semantics ~ interface/implementation
  - gives modularity at appropriate boundaries
  - compiler approaches in library
  - analysis/instrumentation
  - most importantly, perspective


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
- `mtl`, `fused-effects`, `eff`, `polysemy`, etc. do

- NB: monad transformers aren’t the only model for effect handlers

- demo:

    FE.runError @String . FE.runState @Int 0 $ errorAndState
    FE.runState @Int 0 . FE.runError @String $ errorAndState

    FE.run . FE.runError @String $ FE.get @Int


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

- symbols provided by effect system (`>>=` & `return`) & constrained by types

- effects can be factored arbitrarily small
  - `Error` ~ the language of catchable failures with an error
  - `Reader` ~ the language of a single locally-configurable parameter
  - `State` ~ the language of a single mutable variable
  - aside: “the” rather than “a” is notional; might be other expressions of same concept

- we may think in terms of DSLs already

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


## Conclusion

- recap
  - pragmatic
  - perspective

- other applications & future work:
  - database
  - filesystem
  - network
  - time
  - profiling
  - debugging tools
  - instrumentation
  - analysis
  - repl

  - specific:
    - labelling is useful for more than just logging
    - logging & profiling are both instances of tracing

- homework:
  - how could we express the laws for logging s.t. we could define property tests for them?
    - should messages/laws involve time?
    - if so, what’s a useful way of modelling time so we can parameterize it?
  - define a Teletype effect
    - what laws should it have, if any?
    - what kind of implementations could you have?

- thanks/acknowledgements
  - my team, esp. Tim Clem, Ayman Nadeem, Rebecca Valentine, & Patrick Thomson
  - research community, esp. Nicholas Wu & Tom Schrijvers, and also Paul Hudak for “DSLs are the ultimate abstraction”
