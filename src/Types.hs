{-# LANGUAGE OverloadedStrings, LambdaCase #-}

module Types where

import Data.String ( IsString(..) )
import Data.List ( intercalate )
import Indices ( Ix )

data Type
    = Unit
    | Bool
    | Int
    | String
    | TRef Lft RefType Type
    | TFn Int [Type] Type
  deriving (Eq, Show)

data Lft
    = Loc Int
    | LftVar !Int !Int
  deriving (Eq, Show)

data RefType
    = Uniq
    | Shr
  deriving (Eq, Show)

isBaseType :: Type -> Bool
isBaseType = \case
    Int -> True
    Bool -> True
    Unit -> True
    String -> True
    _ -> False

shift :: Type -> Int -> Maybe Type
shift (TRef (Loc n) rt ty) m
    | n+m >= 0  = TRef (Loc n) rt <$> shift ty m
    | otherwise = Nothing
shift (TRef (LftVar n k) rt ty) m
    | n+m >= 0  = TRef (LftVar (n+m) k) rt <$> shift ty m
    | otherwise = Nothing
shift ty m = pure ty

-- | Lifetime inclusion
incl :: Lft -> Lft -> Bool
(Loc n) `incl` (Loc m) = n <= m
(Loc n) `incl` (LftVar m _) = n < m
a `incl` b = a == b

-- | Subtyping relation
(<:) :: Type -> Type -> Bool
(TRef lft Shr ty) <: (TRef lft' Shr ty')
    = lft `incl` lft' && ty <: ty'
(TRef lft Uniq ty) <: (TRef lft' Uniq ty')
    = lft `incl` lft' && ty == ty'
(TFn n params out) <: (TFn m params' out')
    = (n == m) 
    && out <: out' 
    && length params == length params' 
    && and (zipWith (<:) params' params)  -- (and) checks if all the elements in the list are true
t1 <: t2 = t1 == t2 

prettyType :: (IsString str, Semigroup str) => Type -> str
prettyType Unit = "()"
prettyType Bool = "bool"
prettyType Int = "int"
prettyType String = "String"
prettyType (TRef lft Shr ty) = "&" <> prettyLft lft <> prettyType ty
  where
    prettyLft :: (IsString s, Semigroup s) => Lft -> s
    prettyLft (Loc _) = ""
    prettyLft (LftVar _ k) = "'a_" <> fromString (show k)
prettyType (TRef lft Uniq ty) = "&mut " <> prettyLft lft <> prettyType ty
  where
    prettyLft :: (IsString s, Semigroup s) => Lft -> s
    prettyLft (Loc _) = ""
    prettyLft (LftVar _ k) = "'a_" <> fromString (show k)
prettyType (TFn n params out) 
    = "for<" <> prettyLfts n <> ">(" <> (prettyTypes params) <> ") -> " <> prettyType out
  where
    prettyLfts :: (IsString s, Semigroup s) => Int -> s
    prettyLfts 0 = ""
    prettyLfts 1 = "'a"
    prettyLfts n = "'a_1, ..., 'a_" <> fromString (show n)

    prettyTypes :: (IsString s, Semigroup s) => [Type] -> s
    prettyTypes [] = ""
    prettyTypes [t] = prettyType t
    prettyTypes (t:ts) = prettyType t <> ", " <> prettyTypes ts