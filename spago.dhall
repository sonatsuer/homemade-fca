{ name = "halogen-project"
, dependencies =
  [ "console"
  , "effect"
  , "prelude"
  , "either"
  , "string-parsers"
  , "bifunctors"
  , "arrays"
  , "control"
  , "lists"
  , "strings"
  , "test-unit"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
