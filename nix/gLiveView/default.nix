## Install gLiveView ###########################################################
#
# Note, that guild-operators does currently not have reliable releases
# Instead, we cherry pick a revision that we found to be good at
# build time of this image.
#
# [FR] Improvements to the release process
# https://github.com/cardano-community/guild-operators/issues/855
#
# The auto update feature of gLiveView is disabled
#
# https://github.com/cardano-community/guild-operators
{
  pkgs ? import <nixpkgs> {},
}:

pkgs.stdenv.mkDerivation {

  pname = "gLiveView";
  version = "alpha";

  src = builtins.fetchGit {
    name = "guild-operators";
    url = "https://github.com/cardano-community/guild-operators.git";
    # gLiveView version to 1.20.6
    rev = "0f3b1e3412f5002fcfb6dc539ba65a68635eaab4";
  };

  builder = ./builder.sh;
}
