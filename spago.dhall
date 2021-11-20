{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "supply"
, dependencies =
  [ "console"
  , "control"
  , "effect"
  , "lazy"
  , "prelude"
  , "refs"
  , "tuples"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
, license = "Apache-2.0"
, repository = "https://github.com/ajnsit/purescript-supply.git"
}
