{-# LANGUAGE QuasiQuotes, TypeFamilies, OverloadedStrings #-}
import Yesod
import Control.Applicative

data FreeForm = FreeForm
mkYesod "FreeForm" [$parseRoutes|
/ RootR GET
|]
instance Yesod FreeForm where approot _ = ""

data Person = Person String Int String
    deriving Show

getRootR = do
    ((merr, mperson, form), enctype) <- runFormMonadGet $ do
        (name, namef) <- stringField "Hello, my name is" Nothing
        (age, agef) <- intField "I'm " $ Just 25
        (color, colorf) <- stringField "My favorite color is" Nothing
        let (merr, mperson) =
                case Person <$> name <*> age <*> color of
                    FormSuccess p -> (Nothing, Just p)
                    FormFailure e -> (Just e, Nothing)
                    FormMissing -> (Nothing, Nothing)
        let form = [$hamlet|
%div
  %fieldset
    %legend Here's a nice form!
    %div   
      %fieldset
        %legend Name
          ^fieldToDiv.namef^
    %div   
      %fieldset
        %legend Age
           ^fieldToDiv.agef^
    %div   
      %fieldset
        %legend Favorite color
           ^fieldToDiv.colorf^
|]
        return (merr, mperson, form)
    defaultLayout [$hamlet|
$maybe merr err
    %ul!style=color:red
        $forall err e
            %li $e$
$maybe mperson person
    %p Last person: $show.person$
%form!method=get!action=@RootR@!enctype=$enctype$
    %p ^form^
    %input!type=submit!value=Submit
|]

main = basicHandler 3000 FreeForm
