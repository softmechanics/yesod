{-# LANGUAGE TypeFamilies
           , FlexibleContexts
           , FlexibleInstances
           , MultiParamTypeClasses
           , UndecidableInstances
           , FunctionalDependencies
           , OverlappingInstances
           #-}
module Yesod.Form.HForm where

import Yesod.Form

import Data.HList
import Data.HList.Record 
import Data.HList.Utils
import Data.HList.Tuple hiding (Construct(..))
import Data.HList.Field
import Data.HList.Field.Label
import Data.HList.Field.Constructor

import Control.Applicative

type family FormData a
type instance FormData (Record fs) = Record (FormData' fs)

type family FormData' a
type instance FormData' HNil = HNil
type instance FormData' (HCons f fs) = (HCons (FormDataField f) (FormData' fs))

type family FormDataField a
type instance FormDataField (LVPair nm fld) = (LVPair nm (FieldData fld))

class (FieldTyImpl (FieldData fld)
      ) => GFormField fld where
  type FieldForm fld
  type FieldData fld 
  type FieldName fld

  formFieldSettings :: fld -> FormFieldSettings

  formFieldDefault :: fld -> Maybe (FieldData fld)
  formFieldDefault _ = Nothing
  
  formFieldImpl :: (Monad m) => fld -> m (FormField sub y (FieldData fld))
  formFieldImpl fld = return $ fieldTyImpl sets def
      where def = formFieldDefault fld
            sets = formFieldSettings fld

  formFieldChecker :: fld -> Maybe (a -> Either String b)
  formFieldChecker _ = Nothing

class FieldTyImpl a where
  fieldTyImpl :: FormFieldSettings -> FormletField sub y a

instance FieldTyImpl Int where
  fieldTyImpl = intField

instance FieldTyImpl [Char] where
  fieldTyImpl = stringField

data FormFieldImpl = FormFieldImpl

instance (GFormField f
         ,Monad m
         ,d ~ FieldData f
         ,impl ~ FormField sub y d
         ) => Apply FormFieldImpl f (m impl) where
  apply _ f = formFieldImpl f

data IntField nm = IntField nm FormFieldSettings 
data SelectField nm h t = SelectField nm FormFieldSettings (h [(t,String)]) 

instance (
         ) => GFormField (QualField form (IntField nm)) where
  type FieldForm (QualField form (IntField nm)) = form
  type FieldData (QualField form (IntField nm)) = Int
  type FieldName (QualField form (IntField nm)) = nm
 
  formFieldSettings (QualField _ (IntField _ settings)) = settings

instance (FieldTyImpl t
         ,Eq t
         ) => GFormField (QualField form (SelectField nm h t)) where
  type FieldForm (QualField form (SelectField nm h t)) = form
  type FieldData (QualField form (SelectField nm h t)) = t
  type FieldName (QualField form (SelectField nm h t)) = nm
 
  formFieldSettings (QualField _ (SelectField _ settings _)) = settings

  formFieldImpl (QualField _ (SelectField _ settings opts)) = do
                   opts' <- opts
                   return $ selectField opts' settings Nothing

data QualFieldF a = QualFieldF a
data QualField a b = QualField a b

instance (MkQualField form field
         ,QualField form field ~ field'
         ,nm ~ FieldName field'
         ) => Apply (QualFieldF form) field (LVPair nm field') where
  apply (QualFieldF form) field = LVPair $ mkQualField form field

class MkQualField form field where
  mkQualField :: form -> field -> QualField form field
  
instance (
         ) => MkQualField form (SelectField nm h t) where
  mkQualField form field = QualField form field

instance MkQualField form field where
  mkQualField form field = QualField form field

class MkQualForm form fields qual | form fields -> qual where
  qualForm :: form -> fields -> qual

instance (Untuple fields fields'
         ,HMap (QualFieldF form) fields' qualFlds
         ,Monad (FormHandler form)
         ) => MkQualForm form fields (Record qualFlds) where
  qualForm form fields = Record $ hMap (QualFieldF form) (untuple fields)

formData :: f -> FormData f
formData _ = undefined

formConstructor :: (Construct (FormData f)
                   ,Apply Constructor (FormData f) c
                   ) => f -> c
formConstructor = constructor . formData

formFieldlets :: (RecordValues r vs
                 ,HMap FormFieldImpl vs vs'
                 ,HSequence vs' vs''
                 ) => Record r -> vs''
formFieldlets = hSequence . hMap FormFieldImpl . recordValues

hFormlet f = do fieldlets <- formFieldlets f
                return $ hFoldl ApplicativeF (pure $ formConstructor f) fieldlets

runHFormGet f = do fl <- hFormlet f
                   runFormGet fl

