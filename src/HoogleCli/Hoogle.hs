-- | Functions and data types to handle Hoogle results

module HoogleCli.Hoogle where

import Prelude hiding (mod)
import Data.Maybe (fromMaybe)

import HoogleCli.Types
import HoogleCli.Haddock (Html, parseHoogleHtml)
import qualified Hoogle

data Item
  = Declaration Declaration
  | Module Module
  | Package Package

fromHoogleTarget :: Hoogle.Target -> Item
fromHoogleTarget target =
  case Hoogle.targetType target of
    "module" ->
      let
          (pkg, pkgUrl) = fromMaybe
            (error "Hoogle module without package info")
            (Hoogle.targetPackage target)
      in
      Module $ Module_
        { mUrl         = ModuleUrl $ Hoogle.targetURL target
        , mPackageUrl  = PackageUrl pkgUrl
        , mPackage     = pkg
        , mDescription = parseHoogleHtml $ Hoogle.targetItem target
        , mDocs        = parseHoogleHtml $ Hoogle.targetDocs target
        }
    "package" ->
      Package $ Package_
        { pUrl         = PackageUrl $ Hoogle.targetURL target
        , pDescription = parseHoogleHtml $ Hoogle.targetItem target
        , pDocs        = parseHoogleHtml $ Hoogle.targetDocs target
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
      in
      Declaration $ Declaration_
        { dUrl         = DeclUrl moduleUrl anchor
        , dPackage     = pkg
        , dPackageUrl  = PackageUrl pkgUrl
        , dModule      = mod
        , dModuleUrl   = moduleUrl
        , dDescription = parseHoogleHtml $ Hoogle.targetItem target
        , dDocs        = parseHoogleHtml $ Hoogle.targetDocs target
        }

data Declaration = Declaration_
  { dUrl         :: DeclUrl
  , dPackage     :: String
  , dPackageUrl  :: PackageUrl
  , dModule      :: String
  , dModuleUrl   :: ModuleUrl
  , dDescription :: Html
  , dDocs        :: Html
  }

data Module = Module_
  { mUrl         :: ModuleUrl
  , mPackage     :: String
  , mPackageUrl  :: PackageUrl
  , mDescription :: Html
  , mDocs        :: Html
  }

data Package = Package_
  { pUrl         :: PackageUrl
  , pDescription :: Html
  , pDocs        :: Html
  }

