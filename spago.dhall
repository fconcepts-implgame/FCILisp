{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name =
    "fci-lisp"
, dependencies =
    [ "aff"
    , "arrays"
    , "console"
    , "effect"
    , "either"
    , "enums"
    , "foldable-traversable"
    , "lists"
    , "maybe"
    , "node-process"
    , "node-readline"
    , "options"
    , "ordered-collections"
    , "parsing"
    , "prelude"
    , "psci-support"
    , "spec"
    , "strings"
    , "transformers"
    , "tuples"
    ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
