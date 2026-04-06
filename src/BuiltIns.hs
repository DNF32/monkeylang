module BuiltIns where

import Types

builtIns :: [(String, Type)]
builtIns =
  [ ("isInt", FnT [AnyT] BoolT),
    ("isString", FnT [AnyT] BoolT),
    ("isFloat", FnT [AnyT] BoolT),
    ("isBool", FnT [AnyT] BoolT)
  ]
