-- | Functions and data types to handle Hoogle results

module Docs.CLI.Hoogle where

import Prelude hiding (mod)
import Data.Maybe (fromMaybe)
import Data.Aeson (FromJSON(..))

import Docs.CLI.Types
import Docs.CLI.Haddock (Html, parseHoogleHtml, HasCompletion(..), innerString)
import qualified Hoogle

data Item
  = Declaration Declaration
  | Module Module
  | Package Package
  deriving (Eq)

instance HasCompletion Item where
  completion = \case
   Declaration d -> dCompletion d
   Module m      -> mTitle m
   Package p     -> pTitle p

instance FromJSON Item where
  parseJSON = fmap fromHoogleTarget . parseJSON

fromHoogleTarget :: Hoogle.Target -> Item
fromHoogleTarget target =
  case Hoogle.targetType target of
    "module" ->
      let
          (pkg, pkgUrl) = fromMaybe
            (error "Hoogle module without package info")
            (Hoogle.targetPackage target)

          desc = parseHoogleHtml $ Hoogle.targetItem target
      in
      Module $ Module_
        { mUrl         = ModuleUrl $ Hoogle.targetURL target
        , mPackageUrl  = PackageUrl pkgUrl
        , mPackage     = pkg
        , mDescription = desc
        , mDocs        = parseHoogleHtml $ Hoogle.targetDocs target
        , mTarget      = target
        , mTitle       = innerString desc
        }
    "package" ->
      let desc = parseHoogleHtml $ Hoogle.targetItem target
      in
      Package $ Package_
        { pUrl         = PackageUrl $ Hoogle.targetURL target
        , pDescription = desc
        , pDocs        = parseHoogleHtml $ Hoogle.targetDocs target
        , pTarget      = target
        , pTitle       = innerString desc
        }
    _ ->
      let
          (pkg, pkgUrl) = fromMaybe
            (error "Hoogle declaration without package info")
            (Hoogle.targetPackage target)

          (mod, modUrl) = fromMaybe
            (error "Hoogle declaration without module info")
            (Hoogle.targetModule target)

          anchor = fromMaybe
            (error "Hoogle declaration without anchor in Link URL")
            (takeAnchor $ Hoogle.targetURL target)

          moduleUrl = ModuleUrl modUrl
          desc = parseHoogleHtml $ Hoogle.targetItem target
      in
      Declaration $ Declaration_
        { dUrl         = DeclUrl moduleUrl anchor
        , dPackage     = pkg
        , dPackageUrl  = PackageUrl pkgUrl
        , dModule      = mod
        , dModuleUrl   = moduleUrl
        , dDescription = desc
        , dDocs        = parseHoogleHtml $ Hoogle.targetDocs target
        , dCompletion  = innerString desc
        , dTarget      = target
        }

data Declaration = Declaration_
  { dUrl         :: DeclUrl
  , dPackage     :: String
  , dPackageUrl  :: PackageUrl
  , dModule      :: String
  , dModuleUrl   :: ModuleUrl
  , dDescription :: Html
  , dDocs        :: Html
  , dTarget      :: Hoogle.Target
  , dCompletion  :: String
  }
  deriving (Eq)

data Module = Module_
  { mUrl         :: ModuleUrl
  , mPackage     :: String
  , mPackageUrl  :: PackageUrl
  , mDescription :: Html
  , mDocs        :: Html
  , mTarget      :: Hoogle.Target
  , mTitle       :: String
  }
  deriving (Eq)

data Package = Package_
  { pUrl         :: PackageUrl
  , pDescription :: Html
  , pDocs        :: Html
  , pTarget      :: Hoogle.Target
  , pTitle       :: String
  }
  deriving (Eq)

description :: Item -> Html
description = \case
  Declaration d -> dDescription d
  Module      m -> mDescription m
  Package     p -> pDescription p

docs :: Item -> Html
docs = \case
  Declaration d -> dDocs d
  Module      m -> mDocs m
  Package     p -> pDocs p
