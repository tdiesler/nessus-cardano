{
  # Pinned packages with Niv
  sources ? import ../../sources.nix,
  haskellNix ? import sources.haskellNix {},
  nixpkgsSrc ? haskellNix.sources.nixpkgs-2009,
  nixpkgsArgs ? haskellNix.nixpkgsArgs,
  pkgs ? import nixpkgsSrc nixpkgsArgs,

  # Required version args
  debianVersion
}:

let
  # Map the Nix platform identifier to the Docker one
  imageArch = if builtins.currentSystem == "x86_64-linux" then "amd64"
    else if builtins.currentSystem == "aarch64-linux" then "arm64"
    else builtins.abort "[ERROR] Unsupported platform architecture: ${builtins.currentSystem}";
in
  pkgs.stdenv.mkDerivation {
    pname = "nessusio-debian";
    version = "${debianVersion}-${imageArch}";
    src = ./. + "/target/nessusio-debian-${debianVersion}-${imageArch}.tgz";
    builder = ./builder.sh;
  }
