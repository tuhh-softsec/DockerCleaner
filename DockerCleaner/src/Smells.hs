{-# LANGUAGE NamedFieldPuns #-}
module Smells where

import qualified Command.Apk (proxy)
import qualified Command.AptGet (proxy)
import qualified Command.Gem (proxy)
import qualified Command.Npm (proxy)
import qualified Command.Pip (proxy)
import Data.Functor ((<&>))
import Data.Map (Map, findWithDefault, empty)
import qualified FaultInjector.DoNotHaveSecrets (injectFault)
import qualified FaultInjector.DoNotUseAptGetUpdateAlone (injectFault)
import qualified FaultInjector.HaveAHealthcheck (injectFault)
import qualified FaultInjector.HaveAUser (injectFault)
import qualified FaultInjector.PinBaseImageVersion (injectFault)
import qualified FaultInjector.PinPackageManagerVersions (injectFault)
import qualified FaultInjector.UseCopyInsteadOfAdd (injectFault)
import qualified FaultInjector.UseNoInstallRecommends (injectFault)
import qualified FaultInjector.UseWgetInsteadOfAdd (injectFault)
import qualified Fixer.DoNotHaveSecrets (fix)
import qualified Fixer.DoNotUseAptGetUpdateAlone (fix)
import qualified Fixer.HaveAHealthcheck (fix)
import qualified Fixer.PinPackageManagerVersions (fix)
import qualified Fixer.UseCopyInsteadOfAdd (fix)
import qualified Fixer.HaveAUser (fix)
import qualified Fixer.UseNoInstallRecommends (fix)
import Language.Docker (InstructionPos)
import ShellCheck.AST (Token)
import System.Random (RandomGen (split), StdGen, uniformR)
import Data.List (subsequences)
import Dockerfile (Dockerfile)
import Fixer.PinBaseImageVersion (BaseImageVersions (BaseImageVersions), getVersion, fix)
import PackageVersions (PackageVersions)

type Fixer = Dockerfile -> Dockerfile

data Smell = Smell {smellId :: String, injector :: Maybe (StdGen -> Dockerfile -> Dockerfile), fixer :: Maybe Fixer}

newtype AptGetVersions = AptGetVersions PackageVersions

newtype PipVersions = PipVersions PackageVersions

newtype NpmVersions = NpmVersions PackageVersions

newtype GemVersions = GemVersions PackageVersions

newtype ApkVersions = ApkVersions PackageVersions

data PackageManagerPackageVersions = PackageManagerPackageVersions {
  apkVersions :: ApkVersions,
  aptGetVersions :: AptGetVersions,
  gemVersions :: GemVersions,
  pipVersions :: PipVersions,
  npmVersions :: NpmVersions
}

defaultPackageManagerVersions = PackageManagerPackageVersions {
  apkVersions = ApkVersions empty,
  aptGetVersions = AptGetVersions empty,
  gemVersions = GemVersions empty,
  pipVersions = PipVersions empty,
  npmVersions = NpmVersions empty
}

getSmellIds :: [String]
getSmellIds = map smellId $ getSmells defaultPackageManagerVersions ("empty_image", "empty_tag")

getSmellInjectors :: [Smell]
getSmellInjectors =
  [ Smell
      { smellId = "use-no-install-recommends",
        injector = Just FaultInjector.UseNoInstallRecommends.injectFault,
        fixer = Nothing
      },
    Smell
      { smellId = "do-not-use-apt-get-update-alone",
        injector = Just FaultInjector.DoNotUseAptGetUpdateAlone.injectFault,
        fixer = Nothing
      },
    Smell
      { smellId = "pin-package-manager-versions-apt-get",
        injector = Just (FaultInjector.PinPackageManagerVersions.injectFault Command.AptGet.proxy),
        fixer = Nothing
      },
    Smell
      { smellId = "pin-package-manager-versions-pip",
        injector = Just (FaultInjector.PinPackageManagerVersions.injectFault Command.Pip.proxy),
        fixer = Nothing
      },
    Smell
      { smellId = "pin-package-manager-versions-npm",
        injector = Just (FaultInjector.PinPackageManagerVersions.injectFault Command.Npm.proxy),
        fixer = Nothing
      },
    Smell
      { smellId = "pin-package-manager-versions-gem",
        injector = Just (FaultInjector.PinPackageManagerVersions.injectFault Command.Gem.proxy),
        fixer = Nothing
      },
    Smell
      { smellId = "pin-package-manager-versions-apk",
        injector = Just (FaultInjector.PinPackageManagerVersions.injectFault Command.Apk.proxy),
        fixer = Nothing
      },
--    Smell
--      { smellId = "pin-base-image-version",
--        injector = Just FaultInjector.PinBaseImageVersion.injectFault,
--        fixer = Nothing
--      },
    Smell
      { smellId = "use-copy-instead-of-add",
        injector = Just FaultInjector.UseCopyInsteadOfAdd.injectFault,
        fixer = Nothing
      },
    Smell
      { smellId = "use-wget-instead-of-add",
        injector = Just FaultInjector.UseWgetInsteadOfAdd.injectFault,
        fixer = Nothing
      },
    Smell
      { smellId = "do-not-have-secrets",
        injector = Just FaultInjector.DoNotHaveSecrets.injectFault,
        fixer = Nothing
      },
    Smell
      { smellId = "have-a-healthcheck",
        injector = Just FaultInjector.HaveAHealthcheck.injectFault,
        fixer = Nothing
      },
    Smell
      { smellId = "have-a-user",
        injector = Just FaultInjector.HaveAUser.injectFault,
        fixer = Nothing
      }
  ]

getSmells :: PackageManagerPackageVersions -> (String, String) -> [Smell]
getSmells PackageManagerPackageVersions {aptGetVersions = AptGetVersions aptGetVersions, pipVersions = PipVersions pipVersions, npmVersions = NpmVersions npmVersions, gemVersions = GemVersions gemVersions, apkVersions = ApkVersions apkVersions} osImage =
  [ Smell
      { smellId = "use-no-install-recommends",
        injector = Just FaultInjector.UseNoInstallRecommends.injectFault,
        fixer = Just Fixer.UseNoInstallRecommends.fix
      },
    Smell
      { smellId = "do-not-use-apt-get-update-alone",
        injector = Just FaultInjector.DoNotUseAptGetUpdateAlone.injectFault,
        fixer = Just Fixer.DoNotUseAptGetUpdateAlone.fix
      },
    Smell
      { smellId = "pin-package-manager-versions-apt-get",
        injector = Just (FaultInjector.PinPackageManagerVersions.injectFault Command.AptGet.proxy),
        fixer = Just (Fixer.PinPackageManagerVersions.fix Command.AptGet.proxy (\x -> findWithDefault "" x aptGetVersions))
      },
    Smell
      { smellId = "pin-package-manager-versions-pip",
        injector = Just (FaultInjector.PinPackageManagerVersions.injectFault Command.Pip.proxy),
        fixer = Just (Fixer.PinPackageManagerVersions.fix Command.Pip.proxy (\x -> findWithDefault "" x pipVersions))
      },
    Smell
      { smellId = "pin-package-manager-versions-npm",
        injector = Just (FaultInjector.PinPackageManagerVersions.injectFault Command.Npm.proxy),
        fixer = Just (Fixer.PinPackageManagerVersions.fix Command.Npm.proxy (\x -> findWithDefault "" x npmVersions))
      },
    Smell
      { smellId = "pin-package-manager-versions-gem",
        injector = Just (FaultInjector.PinPackageManagerVersions.injectFault Command.Gem.proxy),
        fixer = Just (Fixer.PinPackageManagerVersions.fix Command.Gem.proxy (\x -> findWithDefault "" x gemVersions))
      },
    Smell
      { smellId = "pin-package-manager-versions-apk",
        injector = Just (FaultInjector.PinPackageManagerVersions.injectFault Command.Apk.proxy),
        fixer = Just (Fixer.PinPackageManagerVersions.fix Command.Apk.proxy (\x -> findWithDefault "" x apkVersions))
      },
--    Smell
--      { smellId = "pin-base-image-version",
--        injector = Just FaultInjector.PinBaseImageVersion.injectFault,
--        fixer = Just $ Fixer.PinBaseImageVersion.fix $ Fixer.PinBaseImageVersion.getVersion baseImageVersions
--      },
    Smell
      { smellId = "use-copy-instead-of-add",
        injector = Just FaultInjector.UseCopyInsteadOfAdd.injectFault,
        fixer = Just Fixer.UseCopyInsteadOfAdd.fix
      },
    Smell
      { smellId = "use-wget-instead-of-add",
        injector = Just FaultInjector.UseWgetInsteadOfAdd.injectFault,
        fixer = Nothing -- integrated in use-copy-instead-of-add
      },
    Smell
      { smellId = "do-not-have-secrets",
        injector = Just FaultInjector.DoNotHaveSecrets.injectFault,
        fixer = Just Fixer.DoNotHaveSecrets.fix
      },
    Smell
      { smellId = "have-a-healthcheck",
        injector = Just FaultInjector.HaveAHealthcheck.injectFault,
        fixer = Just Fixer.HaveAHealthcheck.fix
      },
    Smell
      { smellId = "have-a-user",
        injector = Just FaultInjector.HaveAUser.injectFault,
        fixer = Just (Fixer.HaveAUser.fix osImage)
      }
  ]

injectAll :: [Smell] -> StdGen -> Dockerfile -> Dockerfile
injectAll smells generator = fst $ foldr helper (id, generator) smells
  where
    helper Smell {injector = Just injector} (combinedInjectors, generator) = (combinedInjectors <&> injector g1, g2)
      where
        (g1, g2) = split generator
    helper _ (combinedInjectors, generator) = (combinedInjectors, generator)

fixAll :: [Smell] -> Fixer
fixAll = foldr helper id
  where
    helper Smell {fixer = Just fixer} combinedFixers = combinedFixers <&> fixer
    helper _ combinedFixers = combinedFixers

selectRandomSmells :: StdGen -> [Smell] -> [Smell]
selectRandomSmells gen smells = p !! selectedPremutation
  where
    p = subsequences smells
    n = length p
    (selectedPremutation, _) = uniformR (0, n -1) gen
