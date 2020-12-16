{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "supply"
, dependencies =
  [ "console", "effect", "psci-support", "newtype", "tuples", "lazy", "refs" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
