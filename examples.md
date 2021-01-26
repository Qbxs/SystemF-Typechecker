# Examples for this package, fitted for the parser


## identity
`λX.λx:X.x`

## functional extensionality
`λA.λa:A.((λX.λx:X.x [A]) a)`

## self application
`\f:forall X.(X->X).((f [forall X.(X->X)]) f)`

## exercise 1 from assignment
`λC.(((const [Nat]) [C]) five)`

## exercise 2 from assignment
`\X.\f:forall Y.(Y -> Y).((f [(X -> X)]) (f [X]))`
