{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name =
    "fci-lisp"
, dependencies =
    [ "arrays"
    , "console"
    , "effect"
    , "either"
    , "enums"
    , "foldable-traversable"
    , "lists"
    , "maybe"
    , "ordered-collections"
    , "parsing"
    , "prelude"
    , "psci-support"
    , "strings"
    , "transformers"
    , "tuples"
    ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
