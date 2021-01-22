# Examples for this package, fitted for the parser


## identity
`\X.\x:X.x`

## functional extensionality
`\a:A.((\X.\x:X.x [A]) a)`

## self application
`\x:forall X.(X->X).((x [forall X.(X->X)]) x)`
