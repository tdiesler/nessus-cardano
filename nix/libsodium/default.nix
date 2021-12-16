{
  # Pinned packages with Niv
  sources ? import ../sources.nix,
  haskellNix ? import sources.haskellNix {},
  nixpkgsSrc ? haskellNix.sources.nixpkgs-2105,
  nixpkgsArgs ? haskellNix.nixpkgsArgs,
  pkgs ? import nixpkgsSrc nixpkgsArgs,
}:

pkgs.stdenv.mkDerivation {

  pname = "libsodium";
  version = "1.31.0";

  src = builtins.fetchGit {
    url = "https://github.com/input-output-hk/libsodium";
    rev = "c4f03ededb71073281529e47c78e91abffb323a3";
  };

  buildInputs = [
    pkgs.autoconf
    pkgs.automake
    pkgs.libtool
  ];

  builder = ./builder.sh;
}
