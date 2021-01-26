# Examples for this package, fitted for the parser


## identity
`λX.λx:X.x`

## functional extensionality
`λA.λa:A.((λX.λx:X.x [A]) a)`

## self application
`\f:∀X.(X → X).((f [∀X.(X → X)]) f)`

## exercise 1 from assignment
`λC.(((const [Nat]) [C]) five)`

## exercise 2 from assignment
`λX.λf:∀Y.(Y->Y).((f [(X->X)]) (f [X]))`

## capture-free substitution
`λB.(λA.λB.λx:A.x [B])`
