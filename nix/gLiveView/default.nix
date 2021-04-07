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

  # Required version args
  cardanoVersion,
  nessusRevision
}:

pkgs.stdenv.mkDerivation {

  pname = "gLiveView";
  version = "alpha";

  src = builtins.fetchGit {
    name = "guild-operators";
    url = "https://github.com/cardano-community/guild-operators.git";
    # Moves the gLiveView version to 1.20.3
    rev = "b51dabd5af3bca682ddf1b79e5a1e8c905ec64b2";
  };

  builder = ./builder.sh;
}
