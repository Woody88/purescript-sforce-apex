{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name =
    "my-project"
, dependencies =
    [ "arrays"
    , "bigints"
    , "console"
    , "effect"
    , "generics-rep"
    , "newtype"
    , "node-buffer"
    , "node-fs-aff"
    , "node-process"
    , "parsing"
    , "psci-support"
    , "spec"
    , "unicode"
    , "unordered-collections"
    ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
