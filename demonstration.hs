#!/usr/bin/env stack
{-# LANGUAGE
    GADTs,
    GeneralizedNewtypeDeriving,
    OverloadedStrings,
    MultiParamTypeClasses,
    QuasiQuotes,
    TemplateHaskell,
    TypeFamilies,
    ViewPatterns,
    DeriveGeneric
#-}
import Yesod
import qualified Yesod.Form.Jquery as YJ
import qualified ClassyPrelude.Yesod as CP
import Database.Persist.Sqlite
import qualified Database.Persist.Quasi as Q
import Control.Monad.Trans.Resource (runResourceT)
import Control.Monad.Logger (runStderrLoggingT)
import qualified Data.Aeson as A
import Data.Either
import Data.Text (Text,pack)
import qualified Control.Exception as E
import qualified Text.JSON as J

-- COMMENT: HERE'S OUR "EXTRA" PACKAGE
import qualified GraphQL as GL

-- COMMENT: CONFIGURE DATABASE AND DATA MODELS
share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith Q.lowerCaseSettings "models")

-- COMMENT: MAKE APP/SITE INSTANCE
data App = App ConnectionPool
 
-- COMMENT: DEFINE URLS
mkYesod "App" [parseRoutes|
/ HomeR GET
!/person/#PersonId Person1R GET
!/person Person2R POST
!/family/#FamilyId Family1R GET
!/family Family2R POST
!/genus/#GenusId Genus1R GET
!/genus Genus2R POST
!/species/#SpeciesId Species1R GET
!/species Species2R POST
!/breed/#BreedId Breed1R GET
!/breed Breed2R POST
!/pet/#PetId Pet1R GET
!/pet Pet2R POST
!/pettype/#PetTypeId PetType1R GET
!/pettype PetType2R POST
!/petownership/#PetOwnershipId PetOwnership1R GET
!/petownership PetOwnership2R POST
/query QueryR POST
|] 
 
-- COMMENT: MAKE MORE INSTANCES
instance Yesod App
instance YesodPersist App where
    -- COMMENT: DEFINE AND CONFIGURE CONNECTION SETTINGS
    type YesodPersistBackend App = SqlBackend

    -- COMMENT: DEFINE FUNCTION TO RUN QUERIES
    runDB action = do
        App pool <- getYesod
        runSqlPool action pool
 
-- Tells our application to use the standard English messages.
-- If you want i18n, then you can supply a translating function instead.
instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage 
 
-- And tell us where to find the jQuery libraries. We'll just use the defaults,
-- which point to the Google CDN.
instance YJ.YesodJquery App 

-- The datatype we wish to receive from the form (as database Person Entity)
-- COMMENT: DATA TYPE ATTRIBUTES ARE MAPPED TO FORM FIELDS
data GivenQuery = GivenQuery {
      str :: Textarea
    } deriving (CP.Generic, Show)
-- COMMENT: DECLARE MORE SETTINGS
instance ToJSON GivenQuery where
    toEncoding = A.genericToEncoding A.defaultOptions
instance FromJSON GivenQuery
-- COMMENT: Declare the form.
-- COMMENT: MAKE FORMS AND FIELDS
queryForm :: Html -> MForm Handler (FormResult GivenQuery, Widget)
queryForm = renderBootstrap2 $ GivenQuery
    <$> areq textareaField "Query" Nothing
personForm :: Html -> MForm Handler (FormResult Person, Widget)
personForm = renderBootstrap2 $ Person
    <$> areq textField "Name" Nothing
    <*> areq (selectFieldList genders) "Gender" Nothing
  where
    genders :: [(Text, Bool)]
    genders = [("Male", True),("Female", False)]
familyForm :: Html -> MForm Handler (FormResult Family, Widget)
familyForm = renderBootstrap2 $ Family
    <$> areq textField "Name" Nothing
genusForm :: [(Text, FamilyId)] -> (Html -> MForm Handler (FormResult Genus, Widget))
genusForm families = renderBootstrap2 $ Genus
    <$> areq textField "Name" Nothing
    <*> areq (selectFieldList families) "Family" Nothing
speciesForm :: [(Text, GenusId)] -> (Html -> MForm Handler (FormResult Species, Widget))
speciesForm genera = renderBootstrap2 $ Species
    <$> areq textField "Name" Nothing
    <*> areq (selectFieldList genera) "Genus" Nothing
breedForm :: [(Text, SpeciesId)] -> (Html -> MForm Handler (FormResult Breed, Widget))
breedForm species = renderBootstrap2 $ Breed
    <$> areq textField "Name" Nothing
    <*> areq (selectFieldList species) "Species" Nothing
petForm :: Html -> MForm Handler (FormResult Pet, Widget)
petForm = renderBootstrap2 $ Pet
    <$> areq textField "Name" Nothing
    <*> areq (selectFieldList genders) "Gender" Nothing
  where
    genders :: [(Text, Bool)]
    genders = [("Male", True),("Female", False)]
-- COMMENT: BELOW FORMS ARE POPULATED BY DATA FROM DATABASE
petTypeForm :: [(Text, PetId)] -> [(Text, BreedId)] -> (Html -> MForm Handler (FormResult PetType, Widget))
petTypeForm pets breeds = renderBootstrap2 $ PetType
    <$> areq (selectFieldList pets) "Pet" Nothing
    <*> areq (selectFieldList breeds) "Breed" Nothing
petOwnershipForm :: [(Text, PersonId)] -> [(Text, PetId)] -> (Html -> MForm Handler (FormResult PetOwnership, Widget))
petOwnershipForm people pets = renderBootstrap2 $ PetOwnership
    <$> areq (selectFieldList people) "Owner" Nothing
    <*> areq (selectFieldList pets) "Pet" Nothing

-- The GET handler displays the form
-- COMMENT: DEFINE HANDLERS FOR OUR URLS
getHomeR :: Handler Html
getHomeR = do
    -- COMMENT: RUN ORM ABSTRACTED FUNCTIONS TO GET DATA
    people <- runDB $ selectList [] [Asc PersonId]
    families <- runDB $ selectList [] [Asc FamilyId]
    genera <- runDB $ selectList [] [Asc GenusId]
    species <- runDB $ selectList [] [Asc SpeciesId]
    breeds <- runDB $ selectList [] [Asc BreedId]
    pets <- runDB $ selectList [] [Asc PetId]
    -- COMMENT: MAKE RAW QUERIES TO APPLY SQL JOIN
    let typeSql = "select pet_type.id as pettype_id, pet.name as pet_name, breed.name as breed_name from pet, pet_type, breed where pet.id = pet_type.pet_id and pet_type.breed_id = breed.id"
    -- COMMENT: FROM A HANDLER WRAPPER, DATA IS STREAMED TO A LISTS
    queryPetTypes <- runDB $ CP.sourceToList $ rawQuery typeSql []
    let petTypes = [(if isRight $ (fromPersistValue tid :: Either Text PetTypeId) then fromRight (toSqlKey (-1) :: PetTypeId) $ (fromPersistValue tid :: Either Text PetTypeId) else (toSqlKey (-1) :: PetTypeId),if isRight $ (fromPersistValue ptn :: Either Text Text) then fromRight "no name" $ (fromPersistValue ptn :: Either Text Text) else "no name",if isRight $ (fromPersistValue bn :: Either Text Text) then fromRight "no name" $ (fromPersistValue bn :: Either Text Text) else "no name") | (tid:ptn:bn:t) <- queryPetTypes] :: [(PetTypeId, Text, Text)]
    let ownerSql = "select pet_ownership.id as ownership_id, person.name as person_name, pet.name as pet_name from person, pet_ownership, pet where person.id = pet_ownership.owner_id and pet_ownership.animal_id = pet.id"
    queryPetOwnerships <- runDB $ CP.sourceToList $ rawQuery ownerSql []
    let petOwnerships = [(if isRight $ (fromPersistValue oid :: Either Text PetOwnershipId) then fromRight (toSqlKey (-1) :: PetOwnershipId) $ (fromPersistValue oid :: Either Text PetOwnershipId) else (toSqlKey (-1) :: PetOwnershipId),if isRight $ (fromPersistValue prn :: Either Text Text) then fromRight "no name" $ (fromPersistValue prn :: Either Text Text) else "no name",if isRight $ (fromPersistValue ptn :: Either Text Text) then fromRight "no name" $ (fromPersistValue ptn :: Either Text Text) else "no name") | (oid:prn:ptn:t) <- queryPetOwnerships] :: [(PetOwnershipId, Text, Text)]
    let familyPairs = map (\(Entity i f) -> (familyName f, i :: FamilyId)) families
    let genusPairs = map (\(Entity i g) -> (genusName g, i :: GenusId)) genera
    let speciesPairs = map (\(Entity i s) -> (speciesName s, i :: SpeciesId)) species
    let petPairs = map (\(Entity i p) -> (petName p, i :: PetId)) pets
    let breedPairs = map (\(Entity i b) -> (breedName b, i :: BreedId)) breeds
    let personPairs = map (\(Entity i p) -> (personName p, i :: PersonId)) people
    -- Generate the form to be displayed
    (queryWidget, queryEnctype) <- generateFormPost queryForm
    (personWidget, personEnctype) <- generateFormPost personForm
    (familyWidget, familyEnctype) <- generateFormPost familyForm
    (genusWidget, genusEnctype) <- generateFormPost $ genusForm familyPairs
    (speciesWidget, speciesEnctype) <- generateFormPost $ speciesForm genusPairs
    (breedWidget, breedEnctype) <- generateFormPost $ breedForm speciesPairs
    (petWidget, petEnctype) <- generateFormPost petForm
    (petTypeWidget, petTypeEnctype) <- generateFormPost $ petTypeForm petPairs breedPairs
    (petOwnershipWidget, petOwnershipEnctype) <- generateFormPost $ petOwnershipForm personPairs petPairs
    -- COMMENT: USE MULTIPLE LANGUAGE FEATURES TO RETURN GENERATED HTML WHILE PASSING ABOVE DATA VIA INFIX VARIABLE EVALUATION.
    defaultLayout 
        [whamlet|
            <h1>Query by subject
            <form method=post action=@{QueryR} enctype=#{queryEnctype}>
                ^{queryWidget}
                <button>Submit
            <h1>Person
            <h3>delete Person
            <ul>
                $forall Entity personid person <- people
                    <li>
                        <a href=@{Person1R personid}>#{personName person}
            <h3>add new Person
            <form method=post action=@{Person2R} enctype=#{personEnctype}>
                ^{personWidget}
                <button>Submit
            <h1>Family
            <h3>delete Family
            <ul>
                $forall Entity familyid family <- families
                    <li>
                        <a href=@{Family1R familyid}>#{familyName family}
            <h3>add new Family
            <form method=post action=@{Family2R} enctype=#{familyEnctype}>
                ^{familyWidget}
                <button>Submit
            <h1>Genus
            <h3>delete Genus
            <ul>
                $forall Entity genusid genus <- genera
                    <li>
                        <a href=@{Genus1R genusid}>#{genusName genus}
            <h3>add new Genus
            <form method=post action=@{Genus2R} enctype=#{genusEnctype}>
                ^{genusWidget}
                <button>Submit
            <h1>Species
            <h3>delete Species
            <ul>
                $forall Entity speciesid spec <- species
                    <li>
                        <a href=@{Species1R speciesid}>#{speciesName spec}
            <h3>add new Species
            <form method=post action=@{Species2R} enctype=#{speciesEnctype}>
                ^{speciesWidget}
                <button>Submit
            <h1>Breed
            <h3>delete Breed
            <ul>
                $forall Entity breedid breed <- breeds
                    <li>
                        <a href=@{Breed1R breedid}>#{breedName breed}
            <h3>add new Breed
            <form method=post action=@{Breed2R} enctype=#{breedEnctype}>
                ^{breedWidget}
                <button>Submit
            <h1>Pet
            <h3>delete Pet
            <ul>
                $forall Entity petid pet <- pets
                    <li>
                        <a href=@{Pet1R petid}>#{petName pet}
            <h3>add new Pet
            <form method=post action=@{Pet2R} enctype=#{petEnctype}>
                ^{petWidget}
                <button>Submit
            <h1>Pet Type
            <h3>delete Pet Type
            <ul>
                $forall (petTypeId,petName,breedName) <- petTypes
                    <li>
                        <a href=@{PetType1R petTypeId}>#{breedName} of #{petName}
            <h3>add new Pet Type
            <form method=post action=@{PetType2R} enctype=#{petTypeEnctype}>
                ^{petTypeWidget}
                <button>Submit
            <h1>Ownership
            <h3>delete Ownership
            <ul>
                $forall (petOwnershipId,personName,petName) <- petOwnerships
                    <li>
                        <a href=@{PetOwnership1R petOwnershipId}>#{petName} from #{personName}
            <h3>add new Ownership
            <form method=post action=@{PetOwnership2R} enctype=#{petOwnershipEnctype}>
                ^{petOwnershipWidget}
                <button>Submit
        |] 
 
-- COMMENT: MORE URL "HANDLERS"...
-- The POST handler processes the form. If it is successful, it displays the
-- parsed person. Otherwise, it displays the form again with error messages. postPersonR :: Handler Html
getPerson1R :: PersonId -> Handler String
getPerson1R personId = do
    person <- runDB $ delete personId
    redirect HomeR
postPerson2R = do
    ((result, widget), enctype) <- runFormPost personForm
    case result of
        FormSuccess person -> do
            _ <- runDB $ insert person
            redirect HomeR
        _ -> defaultLayout
            [whamlet|
                <p>Invalid input, let's try again.
                <form method=post action=@{Person2R} enctype=#{enctype}>
                    ^{widget}
                    <button>Submit
                <a href=@{HomeR}>Cancel
            |]
getFamily1R :: FamilyId -> Handler String
getFamily1R familyId = do
    _ <- runDB $ delete familyId
    redirect HomeR
postFamily2R = do
    ((result, widget), enctype) <- runFormPost familyForm
    case result of
        FormSuccess family -> do
            _ <- runDB $ insert family
            redirect HomeR
        _ -> defaultLayout
            [whamlet|
                <p>Invalid input, let's try again.
                <form method=post action=@{Family2R} enctype=#{enctype}>
                    ^{widget}
                    <button>Submit
                <a href=@{HomeR}>Cancel
            |]
getGenus1R :: GenusId -> Handler String
getGenus1R genusId = do
    _ <- runDB $ delete genusId
    redirect HomeR
postGenus2R = do
    families <- runDB $ selectList [] [Asc FamilyName]
    let familyPairs = map (\(Entity i f) -> (familyName f, i :: FamilyId)) families
    ((result, widget), enctype) <- runFormPost $ genusForm familyPairs
    case result of
        FormSuccess genus -> do
            _ <- runDB $ insert genus
            redirect HomeR
        _ -> defaultLayout
            [whamlet|
                <p>Invalid input, let's try again.
                <form method=post action=@{Genus2R} enctype=#{enctype}>
                    ^{widget}
                    <button>Submit
                <a href=@{HomeR}>Cancel
            |]
getSpecies1R :: SpeciesId -> Handler String
getSpecies1R speciesId = do
    _ <- runDB $ delete speciesId
    redirect HomeR
postSpecies2R = do
    genera <- runDB $ selectList [] [Asc GenusName]
    let genusPairs = map (\(Entity i g) -> (genusName g, i :: GenusId)) genera
    ((result, widget), enctype) <- runFormPost $ speciesForm genusPairs
    case result of
        FormSuccess species -> do
            _ <- runDB $ insert species
            redirect HomeR
        _ -> defaultLayout
            [whamlet|
                <p>Invalid input, let's try again.
                <form method=post action=@{Species2R} enctype=#{enctype}>
                    ^{widget}
                    <button>Submit
                <a href=@{HomeR}>Cancel
            |]
getBreed1R :: BreedId -> Handler String
getBreed1R breedId = do
    _ <- runDB $ delete breedId
    redirect HomeR
postBreed2R = do
    species <- runDB $ selectList [] [Asc SpeciesName]
    let speciesPairs = map (\(Entity i s) -> (speciesName s, i :: SpeciesId)) species
    ((result, widget), enctype) <- runFormPost $ breedForm speciesPairs
    case result of
        FormSuccess breed -> do
            _ <- runDB $ insert breed
            redirect HomeR
        _ -> defaultLayout
            [whamlet|
                <p>Invalid input, let's try again.
                <form method=post action=@{Breed2R} enctype=#{enctype}>
                    ^{widget}
                    <button>Submit
                <a href=@{HomeR}>Cancel
            |]
getPet1R :: PetId -> Handler String
getPet1R petId = do
    _ <- runDB $ delete petId
    redirect HomeR
postPet2R = do
    ((result, widget), enctype) <- runFormPost petForm
    case result of
        FormSuccess pet -> do
            _ <- runDB $ insert pet
            redirect HomeR
        _ -> defaultLayout
            [whamlet|
                <p>Invalid input, let's try again.
                <form method=post action=@{Pet2R} enctype=#{enctype}>
                    ^{widget}
                    <button>Submit
                <a href=@{HomeR}>Cancel
            |]
getPetType1R :: PetTypeId -> Handler String
getPetType1R petTypeId = do
    _ <- runDB $ delete petTypeId
    redirect HomeR
postPetType2R = do
    pets <- runDB $ selectList [] [Asc PetId]
    breeds <- runDB $ selectList [] [Asc BreedId]
    let petPairs = map (\(Entity i p) -> (petName p, i :: PetId)) pets
    let breedPairs = map (\(Entity i b) -> (breedName b, i :: BreedId)) breeds
    ((result, widget), enctype) <- runFormPost $ petTypeForm petPairs breedPairs
    case result of
        FormSuccess petType -> do
            _ <- runDB $ insert petType
            redirect HomeR
        _ -> defaultLayout
            [whamlet|
                <p>Invalid input, let's try again.
                <form method=post action=@{PetType2R} enctype=#{enctype}>
                    ^{widget}
                    <button>Submit
                <a href=@{HomeR}>Cancel
            |]

getPetOwnership1R :: PetOwnershipId -> Handler String
getPetOwnership1R petOwnershipId = do
    _ <- runDB $ delete petOwnershipId
    redirect HomeR
postPetOwnership2R = do
    people <- runDB $ selectList [] [Asc PersonId]
    pets <- runDB $ selectList [] [Asc PetId]
    let personPairs = map (\(Entity i p) -> (personName p, i :: PersonId)) people
    let petPairs = map (\(Entity i p) -> (petName p, i :: PetId)) pets
    ((result, widget), enctype) <- runFormPost $ petOwnershipForm personPairs petPairs
    case result of
        FormSuccess petOwnership -> do
            _ <- runDB $ insert petOwnership
            redirect HomeR
        _ -> defaultLayout
            [whamlet|
                <p>Invalid input, let's try again.
                <form method=post action=@{PetOwnership2R} enctype=#{enctype}>
                    ^{widget}
                    <button>Submit
                <a href=@{HomeR}>Cancel
            |]

-- COMMENT: DEFINE A FUNCTION TO ABSTRACT AWAY MONAD FUNCTION SEQUENCE TO BE USED MULTIPLE TIMES IN BELOW CODE BLOCK
runQuery :: String -> HandlerFor App [[Text]]
runQuery qry = do
    results <- runDB $ CP.sourceToList $ rawQuery (pack qry) []
    let iteratedResults = [[if (isRight $ (fromPersistValue y :: Either Text Text)) then (fromRight (pack "no parse") $ (fromPersistValue y :: Either Text Text)) else (fromLeft (pack "no parse") $ (fromPersistValue y :: Either Text Text)) | y<-x] | x<-results]
    return iteratedResults


-- COMMENT: DEFINE OUR DATA SCHEMA
-- COMMENT: MORE INFORMATION IS FOUND ON OUR HACKAGE PACKAGE PAGE <https://hackage.haskell.org/package/graphql-w-persistent>.
svrobjs :: [(String,[String])]
svrobjs = [("Person",["Person","person","owner"]),("Family",["Family","family"]),("Genus",["Genus","genus"]),("Species",["Species","species"]),("Breed",["Breed","breed"]),("Pet",["Pet","pet"]),("Taxonomy",["Taxonomy","taxonomy"])]
sss :: [(String,[String])]
sss = [("Person",["name","gender"]),("Family",["name"]),("Genus",["name"]),("Species",["name"]),("Breed",["name"]),("Pet",["name","gender"]),("Taxonomy",["name"])]
sos :: [(String,[String])]
sos = [("Person",["pet"]),("Family",["genus","species","breed","pet"]),("Genus",["family","species","breed","pet"]),("Species",["family","genus","breed","pet"]),("Breed",["family","genus","species","pet"]),("Pet",["owner","breed","species","genus","family"]),("Taxonomy",["pet"])]
sodn :: [(String,[String])]
sodn = [("Person",["person"]),("Family",["family"]),("Genus",["genus"]),("Species",["species"]),("Breed",["breed"]),("Pet",["pet"]),("Taxonomy",["family","genus","species","breed"])]
sor :: [(String,String,[String])]
sor = [("person","pet",["person","id","pet","id","pet_ownership","owner_id","animal_id"]),("person","breed",["person","id","breed","id","pet_ownership","owner_id","animal_id","pet","id","id","pet_type","pet_id","breed_id"]),("person","species",["person","id","species","id","pet_ownership","owner_id","animal_id","pet","id","id","pet_type","pet_id","breed_id","breed","id","species_id"]),("person","genus",["person","id","genus","id","pet_ownership","owner_id","animal_id","pet","id","id","pet_type","pet_id","breed_id","breed","id","species_id","species","id","genus_id"]),("person","family",["person","id","family","id","pet_ownership","owner_id","animal_id","pet","id","id","pet_type","pet_id","breed_id","breed","id","species_id","species","id","genus_id","genus","id","family_id"]),("family","pet",["family","id","pet","id","genus","family_id","id","species","genus_id","id","breed","species_id","id","pet_type","breed_id","pet_id"]),("family","genus",["family","id","genus","family_id"]),("family","species",["family","id","species","genus_id","genus","family_id","id"]),("family","breed",["family","id","breed","species_id","genus","family_id","id","species","genus_id","id"]),("genus","pet",["genus","id","pet","id","species","genus_id","id","breed","species_id","id","pet_type","breed_id","pet_id"]),("genus","family",["genus","family_id","family","id"]),("genus","species",["genus","id","species","genus_id"]),("genus","breed",["genus","id","breed","species_id","species","genus_id","id"]),("species","pet",["species","id","pet","id","breed","species_id","id","pet_type","breed_id","pet_id"]),("species","breed",["species","id","breed","species_id"]),("species","genus",["species","genus_id","genus","id"]),("species","family",["species","id","family","genus_id","genus","species_id","id"]),("breed","pet",["breed","id","pet","id","pet_type","breed_id","pet_id"]),("breed","species",["breed","species_id","species","id"]),("breed","genus",["breed","species_id","genus","id","species","id","genus_id"]),("breed","family",["breed","species_id","family","id","species","id","genus_id","genus","id","family_id"]),("pet","person",["pet","id","person","id","pet_ownership","animal_id","owner_id"]),("pet","breed",["pet","id","breed","id","pet_type","pet_id","breed_id"]),("pet","species",["pet","id","species","id","pet_type","pet_id","breed_id","breed","id","species_id"]),("pet","genus",["pet","id","genus","id","pet_type","pet_id","breed_id","breed","id","genus_id"]),("pet","family",["pet","id","family","id","pet_type","pet_id","breed_id","breed","id","genus_id","genus","id","family_id"])]
-- COMMENT: THIS IS OUR GRAPHQL FORM. QUERIES ARE INPUT HERE.
postQueryR = do
    ((result, widget), enctype) <- runFormPost queryForm
    case result of
        -- COMMENT: IF FORM IS SUCCESSFUL INPUTS, DATA IS PROCESSED
        FormSuccess (GivenQuery str) -> do
            let string = tail $ init $ show $ unTextarea str
            -- -- parse the given query string to make desired query
            -- let (rootObjs,sqlQueries) = GL.processQueryString string svrobjs sss sos sodn sor 
            -- -- query
            -- queryResults <- mapM (\y -> mapM (\x -> runQuery x) y) sqlQueries
            -- -- process data
            -- let processedResults = GL.processPersistentData queryResults rootObjs

            -- with json file
            parse <- GL.processQueryStringWithJson string "serverschema.json"
            let (serverObjects,queries) = parse
            queryResults <- mapM (\y -> mapM (\x -> runQuery x) y) queries
            let processedResults = GL.processPersistentData queryResults serverObjects
            
            defaultLayout
                [whamlet|
                    <p>#{processedResults}
                    <a href=@{HomeR}>Home
                |]
        -- COMMENT: IN CASE FORM IS INCOMPLETE, WE RETURN TO THE FORM
        _ -> do
            defaultLayout
                [whamlet|
                    <p>Invalid input, let's try again.
                    <form method=post action=@{QueryR} enctype=#{enctype}>
                        ^{widget}
                        <button>Submit
                    <a href=@{HomeR}>Cancel
                |]

-- COMMENT: CONFIGURE CONNECTIONS AND OPEN PORT
openConnectionCount :: Int
openConnectionCount = 10

main :: IO ()
main = runStderrLoggingT $ withSqlitePool "test.db" openConnectionCount $ \pool -> liftIO $ do
    runResourceT $ flip runSqlPool pool $ do
        -- COMMENT: MIGRATIONS ARE AUTOMATICALLY SYNCING DATABASE TABLES, SO YOU DO NOT NEED TO MAKE A DATABASE BEFORE RUNNING THIS PROGRAM.
        runMigration migrateAll

    warp 3000 $ App pool
