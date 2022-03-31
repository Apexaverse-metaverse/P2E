{ source-repo-override ? { } }:
########################################################################
# default.nix -- The top-level nix build file for miles.
#
# This file defines various attributes that are used for building and
# developing p2e.
#
########################################################################

let
  # Here a some of the various attributes for the variable 'packages':
  #
  # { pkgs
  #   p2e: {
  #     haskell: {
  #       project # The Haskell project created by haskell-nix.project
  #       packages # All the packages defined by our project, including dependencies
  #       projectPackages # Just the packages in the project
  #     }
  #     hlint
  #     cabal-install
  #     stylish-haskell
  #     haskell-language-server
  #   }
  # }
  packages = import ./nix { inherit source-repo-override; };

  inherit (packages) pkgs p2e;
  project = p2e.haskell.project;
in
{
  inherit pkgs p2e;

  inherit project;
}
