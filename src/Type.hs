module Type where
import SCUtil

data Type = NoType | Type Symbol | Arr [Type] Type | Void | Int | Bool | String
          deriving (Eq,Show)
