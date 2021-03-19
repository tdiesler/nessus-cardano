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
    # Moves the gLiveView version to 1.19.5
    rev = "0461fe51888a89b5a08f6799686bba82c046d827";
  };

  builder = ./builder.sh;
}
