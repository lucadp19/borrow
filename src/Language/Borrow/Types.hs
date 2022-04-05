{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Borrow.Types where

import Data.String ( IsString(..) )

import Language.Borrow.Indices ( Ix )

-- | A type in Borrow.
data Type
    = Unit                    -- ^ Unit type
    | Bool                    -- ^ Booleans
    | Int                     -- ^ Integers
    | String                  -- ^ Strings
    | TRef Lft RefType Type   -- ^ References
    | TFn Int [Type] Type     -- ^ Function type
  deriving (Eq, Show)

-- | A lifetime value.
-- 
-- It can either be a local lifetime or a lifetime variable.
data Lft
    = Loc !Int           -- ^ Local lifetime
    | LftVar !Int !Int  -- ^ Generic lifetime
  deriving (Eq, Show)

-- ^ Used to distinguish @Uniq@ue references from @Shr@ed ones.
data RefType
    = Uniq  -- ^ For unique references
    | Shr   -- ^ For shared references
  deriving (Eq, Show)

-- | Returns true if and only if the given type is a base type.
isBaseType :: Type -> Bool
isBaseType = \case
    Int -> True
    Bool -> True
    Unit -> True
    String -> True
    _ -> False

-- | Shifts all lifetimes in the given type by the given offset.
shift 
    :: Type       -- ^ The original type
    -> Int        -- ^ The shift
    -> Maybe Type -- ^ The shifted type
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

-- | Subtyping relation. 
--
-- A type @ty@ is subtype of @ty'@ if and only if 
-- a value of type @ty@ can be assigned to a variable of type @ty'@.
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

-- | Pretty-prints a type.
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
prettyType (TRef lft Uniq ty) = "&" <> prettyLft lft <> "mut " <> prettyType ty
  where
    prettyLft :: (IsString s, Semigroup s) => Lft -> s
    prettyLft (Loc _) = ""
    prettyLft (LftVar _ k) = "'a_" <> fromString (show k) <> " "
prettyType (TFn n params out) 
    = "for<" <> prettyLfts n <> ">(" <> prettyTypes params <> ") -> " <> prettyType out
  where
    prettyLfts :: (IsString s, Semigroup s) => Int -> s
    prettyLfts 0 = ""
    prettyLfts 1 = "'a"
    prettyLfts n = "'a_1, ..., 'a_" <> fromString (show n)

    prettyTypes :: (IsString s, Semigroup s) => [Type] -> s
    prettyTypes [] = ""
    prettyTypes [t] = prettyType t
    prettyTypes (t:ts) = prettyType t <> ", " <> prettyTypes ts

-- | Eliminates all references from the type and returns the dereferenced type. 
baseType :: Type -> Type
baseType (TRef _ _ ty) = baseType ty
baseType ty = ty