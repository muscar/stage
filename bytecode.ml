open Common

type opcode =
    OpLock of name index
  | OpUnlock of name index
  | OpCall of name index
  | OpStFld of name index
  | OpLdFld of name index
  | OpAdd
  | OpLdcI4 of Int32.t
  | OpLdStr of string