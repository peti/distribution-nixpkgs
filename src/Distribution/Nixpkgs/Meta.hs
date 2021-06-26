{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}

{- |
   A representation of the @meta@ section used in Nix expressions. A
   detailed description can be found in section 4, \"Meta-attributes\",
   of the Nixpkgs manual at <http://nixos.org/nixpkgs/docs.html>.
 -}

module Distribution.Nixpkgs.Meta
  ( Meta, nullMeta
  , NixpkgsPlatform
  , homepage, description, license, platforms, badPlatforms, hydraPlatforms, maintainers, broken
  ) where

-- Avoid name clash with Prelude.<> exported by post-SMP versions of base.
#if MIN_VERSION_base(4,11,0)
import Prelude hiding ( (<>) )
#endif
import Control.DeepSeq
import Control.Lens
import Data.Either ( isLeft, fromLeft, fromRight )
import Data.Set ( Set )
import qualified Data.Set as Set
import Distribution.Nixpkgs.License
import Distribution.System
import GHC.Generics ( Generic )
import Language.Nix.Identifier
import Language.Nix.Path
import Language.Nix.PrettyPrinting

-- | Representation of platform(s) as supported by nixpkgs:
--
--     * 'Left' represents the name of a platform list as
--       found in @lib.platforms@. E. g. at the time of
--       writing @Left "darwin"@ would represent the
--       platform tuples @x86_64-darwin@, @aarch64-darwin@,
--       @i686-darwin@ and @armv7a-darwin@. This is subject
--       to change as nixpkgs updates @lib.platforms@, of
--       course.
--     * 'Right' indicates a single platform tuple
--       represented using Cabal's 'Platform'.
type NixpkgsPlatform = Either Identifier Platform

-- | A representation of the @meta@ section used in Nix expressions.
--
-- >>> :set -XOverloadedStrings
-- >>> :{
--   print (pPrint (Meta "http://example.org" "an example package" (Unknown Nothing)
--                  (Just (Set.singleton (Right (Platform X86_64 Linux))))
--                  Nothing
--                  (Just Set.empty)
--                  (Set.fromList ["joe","jane"])
--                  True))
-- :}
-- homepage = "http://example.org";
-- description = "an example package";
-- license = "unknown";
-- platforms = [ "x86_64-linux" ];
-- hydraPlatforms = lib.platforms.none;
-- maintainers = with lib.maintainers; [ jane joe ];
-- broken = true;
data Meta = Meta
  { _homepage       :: String
  -- ^ URL of the package homepage
  , _description    :: String
  -- ^ short description of the package
  , _license        :: License
  -- ^ licensing terms
  , _platforms      :: Maybe (Set NixpkgsPlatform)
  -- ^ List of platforms that are supported by the package.
  --   'Nothing' prevents the attribute from being rendered.
  --   See 'NixpkgsPlatform' on the precise representation of platforms.
  , _badPlatforms   :: Maybe (Set NixpkgsPlatform)
  -- ^ List of platforms that are known to be unsupported. This is semantically
  --   equivalent to setting the following:
  --
  --   @
  --     platforms = lib.subtractLists
  --       (initialMeta.badPlatforms or []);
  --       (initialMeta.platforms or lib.platforms.all)
  --   @
  --
  --   'Nothing' prevents the attribute from being rendered.
  --   See 'NixpkgsPlatform' on the precise representation of platforms.
  , _hydraPlatforms :: Maybe (Set NixpkgsPlatform)
  -- ^ Platforms for which the package should be tested, built and added to the
  --   binary cache by Hydra. 'Nothing' prevents the attribute from being rendered.
  --  See 'NixpkgsPlatform' on the precise representation of platforms.
  , _maintainers    :: Set Identifier
  -- ^ list of maintainers from @pkgs\/lib\/maintainers.nix@
  , _broken         :: Bool
  -- ^ set to @true@ if the build is known to fail
  }
  deriving (Show, Eq, Ord, Generic)

makeLenses ''Meta

instance NFData Meta

instance Pretty Meta where
  pPrint Meta {..} = vcat
    [ onlyIf (not (null _homepage)) $ attr "homepage" $ string _homepage
    , onlyIf (not (null _description)) $ attr "description" $ string _description
    , attr "license" $ pPrint _license
    , maybe mempty (renderPlatforms "platforms") _platforms
    , maybe mempty (renderPlatforms "badPlatforms") _badPlatforms
    , maybe mempty (renderPlatforms "hydraPlatforms") _hydraPlatforms
    , setattr "maintainers" (text "with lib.maintainers;") (Set.map (view ident) _maintainers)
    , boolattr "broken" _broken _broken
    ]

partitionEithers :: (Ord a, Ord b) => Set (Either a b) -> (Set a, Set b)
partitionEithers s =
  let (a, b) = Set.partition isLeft s
  in (Set.map (fromLeft undefined) a, Set.map (fromRight undefined) b)

renderPlatforms :: String -> Set NixpkgsPlatform -> Doc
renderPlatforms field ps
  | Set.null ps = sep [ text field <+> equals <+> text "lib.platforms.none" <> semi ]
  | otherwise   = sep ([ text field <+> equals <+> lbrack
                       , nest 2 $ fsep renderedCabalPs
                       , rbrack
                       ] ++ renderedNixpkgsPs)
                  <> semi
  where -- render nixpkgs platforms and cabal platform tuples separately
        -- since the former represents multiple platforms and meta doesn't
        -- support nested lists.
        (nixpkgsPs, cabalPs) = partitionEithers ps
        renderedCabalPs = map text $ Set.toAscList $ Set.map fromCabalPlatform cabalPs
        -- append lib.platforms list via nix's ++ at the end
        platformPath p = path # [ ident # "lib", ident # "platforms", p ]
        renderedNixpkgsPs =
          map (\p -> nest 2 $ sep [ text "++", pPrint (platformPath p) ])
          $ Set.toAscList nixpkgsPs

nullMeta :: Meta
nullMeta = Meta
  { _homepage = error "undefined Meta.homepage"
  , _description = error "undefined Meta.description"
  , _license = error "undefined Meta.license"
  , _platforms = error "undefined Meta.platforms"
  , _badPlatforms = error "undefined Meta.badPlatforms"
  , _hydraPlatforms = error "undefined Meta.hydraPlatforms"
  , _maintainers = error "undefined Meta.maintainers"
  , _broken = error "undefined Meta.broken"
  }

fromCabalPlatform :: Platform -> String
fromCabalPlatform (Platform I386 Linux)                 = "\"i686-linux\""
fromCabalPlatform (Platform X86_64 Linux)               = "\"x86_64-linux\""
fromCabalPlatform (Platform X86_64 OSX)                 = "\"x86_64-darwin\""
fromCabalPlatform (Platform (OtherArch "armv7l") Linux) = "\"armv7l-linux\""
fromCabalPlatform (Platform AArch64 Linux)              = "\"aarch64-linux\""
fromCabalPlatform p                                     = error ("fromCabalPlatform: invalid Nix platform" ++ show p)
