# Examples for this package, fitted for the parser


## identity
`λX.λx:X.x`

## functional extensionality
`λA.λa:A.((λX.λx:X.x [A]) a)`

## self application
`λx:∀X.(X->X).((x [∀X.(X->X)]) x)`

## exercise 2 from assignment
`λC.(((const [Nat]) [C]) five)`
