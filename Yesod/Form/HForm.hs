{-# LANGUAGE TypeFamilies
           , FlexibleContexts
           , FlexibleInstances
           , MultiParamTypeClasses
           , UndecidableInstances
           , FunctionalDependencies
           , OverlappingInstances
           , ScopedTypeVariables
           , GADTs
           #-}
module Yesod.Form.HForm where

import Yesod.Form
import Yesod.Handler

import Data.HList
import Data.HList.Utils
import Data.HList.Tuple hiding (Construct(..))
import Data.HList.Field.Constructor

import Control.Applicative

type family FormData a
type instance FormData (Record fs) = Record (FormData' fs)

type family FormData' a
type instance FormData' HNil = HNil
type instance FormData' (HCons f fs) = (HCons (FormDataField f) (FormData' fs))

type family FormDataField a
type instance FormDataField (LVPair nm fld) = (LVPair nm (FieldData fld))

type HFormHandler f = GHandler (HFormSub f) (HFormMaster f) 

type family HFormSub f
type instance HFormSub (Record (HCons (LVPair l field) fs)) = FieldSub field

type family HFormMaster f
type instance HFormMaster (Record (HCons (LVPair l field) fs)) = FieldMaster field

type FieldHandler f = GHandler (FieldSub f) (FieldMaster f)
type FieldFormField f = FormField (FieldSub f) (FieldMaster f)

class (FieldTyImpl (FieldData fld)
      ) => GFormField fld where
  type FieldSub fld
  type FieldMaster fld
  type FieldData fld 
  type FieldName fld

  formFieldSettings :: fld -> FormFieldSettings

  formFieldDefault :: fld -> Maybe (FieldData fld)
  formFieldDefault _ = Nothing

  formField :: fld -> FieldFormField fld (FieldData fld)
  formField fld = fieldTyImpl sets def
      where def = formFieldDefault fld
            sets = formFieldSettings fld

  formFieldParser :: fld -> FieldHandler fld (FieldFormField fld (FieldData fld))
  formFieldParser = formFieldRenderer

  formFieldRenderer :: fld -> FieldHandler fld (FieldFormField fld (FieldData fld))
  formFieldRenderer = return . formField

  formFieldChecker :: fld -> Maybe (a -> Either String b)
  formFieldChecker _ = Nothing

  setFieldValue :: fld -> (FieldData fld) -> fld

instance (FieldTyImpl t) => GFormField (HField sub y nm t) where
  type FieldSub (HField sub y nm t) = sub
  type FieldMaster (HField sub y nm t) = y
  type FieldData (HField sub y nm t) = t
  type FieldName (HField sub y nm t) = nm
 
  formFieldSettings (IntField _ s _) = s
  formFieldSettings (StringField _ s _) = s

  formFieldDefault (IntField _ _ d) = d
  formFieldDefault (StringField _ _ d) = d

  setFieldValue (IntField nm s _) v = IntField nm s $ Just v
  setFieldValue (StringField nm s _) v = StringField nm s $ Just v

instance (FieldTyImpl t
         ,Eq t
         ) => GFormField (SelectField sub y nm t) where
  type FieldSub (SelectField sub y nm t) = sub
  type FieldMaster (SelectField sub y nm t) = y
  type FieldData (SelectField sub y nm t) = t
  type FieldName (SelectField sub y nm t) = nm
 
  formFieldDefault (SelectField _ _ _ d) = d

  formFieldSettings (SelectField _ settings _ _) = settings
  formFieldRenderer (SelectField _ settings opts _) = do
      opts' <- opts
      return $ selectField opts' settings Nothing

  setFieldValue (SelectField nm s o _) v = SelectField nm s o $ Just v

class FieldTyImpl a where
  fieldTyImpl :: FormFieldSettings -> FormletField sub y a

instance FieldTyImpl Int where
  fieldTyImpl = intField

instance FieldTyImpl [Char] where
  fieldTyImpl = stringField

data HFormFieldParser = HFormFieldParser
data HFormFieldRenderer = HFormFieldRenderer


instance (GFormField f
         ,h ~ FieldHandler f
         ,Monad h
         ,d ~ FieldData f
         ,impl ~ (FieldFormField f) d
         ) => Apply HFormFieldParser f (h impl) where
  apply _ f = formFieldParser f

instance (GFormField f
         ,h ~ FieldHandler f
         ,Monad h
         ,d ~ FieldData f
         ,impl ~ (FieldFormField f) d
         ) => Apply HFormFieldRenderer f (h impl) where
  apply _ f = formFieldRenderer f

data FormFieldF = FormFieldF
instance (GFormField f
         ,d ~ FieldData f
         ,fld ~ FieldFormField f d
         ) => Apply FormFieldF f fld where
  apply _ f = formField f

data HField sub y nm t where
    IntField :: nm -> FormFieldSettings -> Maybe Int -> HField sub y nm Int
    StringField :: nm -> FormFieldSettings -> Maybe String -> HField sub y nm String

data SelectField sub y nm t = SelectField nm FormFieldSettings (GHandler sub y [(t,String)]) (Maybe t)

data FieldToLVPair = FieldToLVPair

instance (nm ~ FieldName f) => Apply FieldToLVPair f (LVPair nm f) where
  apply _ f = LVPair f

data InitFormField = InitFormField

hForm :: (Untuple t l
         ,HMap FieldToLVPair l l'
         ) => t -> Record l'
hForm = Record . hMap FieldToLVPair . untuple

formData :: f -> FormData f
formData _ = undefined

formConstructor :: (Apply Constructor (FormData f) c
                   ) => f -> c
formConstructor = constructor . formData

hFormFields :: (RecordValues r vs
               ,HMap f vs vs'
               ,HSequence vs' vs''
               ) => f -> Record r -> vs''
hFormFields f = hSequence . hMap f . recordValues

formFields :: (RecordValues r vs
              ,HMap FormFieldF vs vs'
              ,HSequence vs' vs''              
              ) => Record r -> vs''
formFields = hFormFields FormFieldF

class HFormParsers f ps | f -> ps where
  hFormParsers :: f -> ps

instance (RecordValues r vs
         ,HMap HFormFieldParser vs vs'
         ,HSequence vs' vs''
         ) => HFormParsers (Record r) vs'' where
  hFormParsers = hFormFields HFormFieldParser

class HFormRenderers f rs | f -> rs where
  hFormRenderers :: f -> rs

instance (RecordValues r vs
         ,HMap HFormFieldRenderer vs vs'
         ,HSequence vs' vs''
         ) => HFormRenderers (Record r) vs'' where
  hFormRenderers = hFormFields HFormFieldRenderer

type HGForm f xml = GForm (HFormSub f) (HFormMaster f) xml (FormData f)

-- | ConstructHForm
class ConstructHForm form fields xml | form fields -> xml where
  constructHForm :: form -> fields -> (HGForm form xml)

instance (Apply Constructor (FormData form) ctor
         ,Applicative f
         ,HFoldl ApplicativeF (f ctor) fields (HGForm form xml)
         ) => ConstructHForm form fields xml where
  constructHForm = runHFormConstructor . formConstructor
    where runHFormConstructor ctor fields = hFoldl ApplicativeF (pure ctor :: f ctor) fields

-- | ParseHFormlet
class ParseHFormlet f xml | f -> xml where
  parseHFormlet :: f -> (HFormHandler f) (HGForm f xml)

instance (Monad m
         ,HFormParsers f (m ps)
         ,ConstructHForm f ps xml
         ,m ~ HFormHandler f
         ) => ParseHFormlet f xml where
  parseHFormlet f = do ps <- hFormParsers f
                       return $ constructHForm f ps

class RenderHFormlet f xml | f -> xml where
  renderHFormlet :: f -> (HFormHandler f) (HGForm f xml)

instance (m ~ HFormHandler f
         ,Monad m
         ,HFormRenderers f (m rs)
         ,ConstructHForm f rs xml
         ) => RenderHFormlet f xml where
  renderHFormlet f = do fs <- hFormRenderers f
                        return $ constructHForm f fs

class ParseHFormGet f xml | f -> xml where
  parseHFormGet :: f -> (HFormHandler f) (FormResult (FormData f), xml, Enctype)

instance (ParseHFormlet f xml
         ) => ParseHFormGet f xml where
  parseHFormGet f = do fl <- parseHFormlet f
                       runFormGet fl

renderHFormGet :: (RenderHFormlet f xml
                  ,sub ~ HFormSub f
                  ,y ~ HFormMaster f
                  ,h ~ HFormHandler f
                  ) => f -> (HGForm f xml -> HGForm f xml') -> h (FormResult (FormData f), xml', Enctype)
renderHFormGet f r = do fl <- renderHFormlet f
                        runFormGet $ r fl

class HUpdateForm form values form' | form values -> form' where
  hUpdateForm :: form -> values -> form'

instance HUpdateForm form HNil form where
  hUpdateForm f _ = f

instance (HUpdateForm form values form'
         ) => HUpdateForm (Record form) (Record values) (Record form') where
  hUpdateForm (Record f) (Record vs) = Record $ hUpdateForm f vs

instance (ApplyAtLabel (SetFieldValue v) l form form'
         ,HUpdateForm form' values form''
         ) => HUpdateForm form (HCons (LVPair l v) values) form'' where
  hUpdateForm form (HCons (LVPair v) values) = form''
      where form' = applyAtLabel (SetFieldValue v) (undefined::l) form
            form'' = hUpdateForm form' values

data SetFieldValue v = SetFieldValue v
instance (GFormField fld
         ,fld ~ fld'
         ,v ~ FieldData fld
         ) => Apply (SetFieldValue v) fld fld' where
  apply (SetFieldValue v) fld = setFieldValue fld v

hUpdateForm' :: (HZip f d a
               ,HMap SetFieldValues a f'
               ,f ~ f'
               ,d ~ FormData' f
               ) => Record f -> Record d -> Record f
hUpdateForm' (Record f) (Record d) = Record . hMap SetFieldValues $ hZip f d

data SetFieldValues = SetFieldValues

instance (GFormField fld
         ,n ~ n'
         ,fld ~ fld'
         ,v ~ FieldData fld
         ) => Apply SetFieldValues ((LVPair n fld),(LVPair n' v)) (LVPair n fld') where
    apply _ (LVPair fld,LVPair v) = LVPair $ setFieldValue fld v
    
