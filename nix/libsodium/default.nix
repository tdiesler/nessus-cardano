{
  # Pinned packages with Niv
  sources ? import ../sources.nix,
  haskellNix ? import sources.haskellNix {},
  nixpkgsSrc ? haskellNix.sources.nixpkgs-2009,
  nixpkgsArgs ? haskellNix.nixpkgsArgs,
  pkgs ? import nixpkgsSrc nixpkgsArgs,
}:

pkgs.stdenv.mkDerivation {

  pname = "libsodium";
  version = "23.3.0-rev1";

  src = builtins.fetchGit {
    url = "https://github.com/input-output-hk/libsodium";
    rev = "004952bb57b2a6d2c033969820c80255e8362615";
  };

  buildInputs = [
    pkgs.autoconf
    pkgs.automake
    pkgs.libtool
  ];

  builder = ./builder.sh;
}
