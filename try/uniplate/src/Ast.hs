{-# LANGUAGE DeriveDataTypeable #-}

module Ast where

import Data.Typeable
import Data.Data
import Data.Generics.Uniplate.Direct

data TranslUnit = 
  TranslUnit [ExtDecl] 
  deriving (Data, Typeable)

data ExtDecl =
    DeclExt CDecl
  | FDefExt FunDef
  deriving (Data, Typeable)

data CDecl = CDecl Int
  deriving (Data, Typeable)

data FunDef = FunDef Int
  deriving (Data, Typeable)

{-!
deriving instance UniplateDirect TranslUnit
deriving instance UniplateDirect ExtDecl
deriving instance UniplateDirect CDecl
deriving instance UniplateDirect FunDef

deriving instance UniplateDirect TranslUnit FunDef
deriving instance UniplateDirect ExtDecl FunDef
!-}
-- GENERATED START

 
instance Uniplate TranslUnit where
         
        {-# INLINE uniplate #-}
        uniplate x = plate x

 
instance Uniplate ExtDecl where
         
        {-# INLINE uniplate #-}
        uniplate x = plate x

 
instance Uniplate CDecl where
         
        {-# INLINE uniplate #-}
        uniplate x = plate x

 
instance Uniplate FunDef where
         
        {-# INLINE uniplate #-}
        uniplate x = plate x

 
instance Biplate TranslUnit FunDef where
         
        {-# INLINE biplate #-}
        biplate (TranslUnit x1) = plate TranslUnit ||+ x1

 
instance Biplate ExtDecl FunDef where
         
        {-# INLINE biplate #-}
        biplate (FDefExt x1) = plate FDefExt |* x1
        biplate x = plate x
-- GENERATED STOP
