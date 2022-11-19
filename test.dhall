let conf = ./spago.dhall

in conf // {
  sources = conf.sources # [ "example/**/*.purs", "test/**/*.purs" ],
  dependencies = conf.dependencies # [
    "assert",
    "aff",
    "console",
    "either",
    "exceptions",
    "node-http",
    "options"
  ]
}
