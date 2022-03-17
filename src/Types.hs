module Types where
data Type
    = Unit
    | Bool
    | Int
    | String
    | TRef Lft RefType Type
  deriving (Eq)

data Lft
    = Loc Int
    | Ext Int
    | LftVar Int
  deriving (Eq)

data RefType
    = Uniq
    | Shr
  deriving (Eq)

shift :: Type -> Int -> Type
shift (TRef (Loc n) rt ty) m
    | n+m >= 0  = TRef (Loc n) rt $ shift ty m
    | otherwise = undefined
shift (TRef (Ext n) rt ty) m
    | n+m >= 0  = TRef (Ext n) rt $ shift ty m
    | otherwise = undefined
shift ty m = ty