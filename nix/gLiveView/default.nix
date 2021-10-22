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

  # gLiveView version to 1.23.1
  # https://github.com/cardano-community/guild-operators/blob/alpha/scripts/cnode-helper-scripts/gLiveView.sh#L55

  src = builtins.fetchGit {
    name = "guild-operators";
    url = "https://github.com/cardano-community/guild-operators.git";
    rev = "3b37dd0f4057269ea2cf4fdc67a69909b6e6cf82";
  };

  builder = ./builder.sh;
}
