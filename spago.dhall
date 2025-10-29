{ name = "halogen-project"
, dependencies =
  [ "console"
  , "effect"
  , "prelude"
  , "either"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
