let T = ./types.dhall

let L = ./locale/linux/enUS.dhall

let empty =
        { keycodes = [] : T.Keycodes
        , gestures = [] : T.Gestures
        , options = [] : T.Options
        , flags = [] : T.Flags
        }
      : T.CfgFile

let linux =
        empty // { keycodes = L.numbers # toMap L.codenames : T.Keycodes }
      : T.CfgFile

in  { linux, empty }
