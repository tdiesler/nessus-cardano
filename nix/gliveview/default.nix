## Install gLiveView ###########################################################
#
# Note, that this currently does not use a specific tag
# Instead it uses whatever happens to be HEAD at build time of this image
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
    # This commit moved gLiveView.sh to v1.19.4
    rev = "5ee20a20b5f74d01735130264913d0ae06f76f48";
  };

  builder = ./builder.sh;
}
