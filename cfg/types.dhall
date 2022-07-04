let Keycode = Natural

let Gesture = Text

let Option = Text

let Flag = Text

let Keycodes = List { mapKey : Text, mapValue : Keycode }

let Gestures = List { mapKey : Text, mapValue : Gesture }

let Options = List { mapKey : Text, mapValue : Option }

let Flags = List Flag

let CfgFile =
      { keycodes : Keycodes
      , gestures : Gestures
      , options : Options
      , flags : Flags
      }

in  { Keycode
    , Gesture
    , Option
    , Flag
    , Keycodes
    , Gestures
    , Options
    , Flags
    , CfgFile
    }
