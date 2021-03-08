# Build the cardano node Docker image
#
# Several examples for pkgs.dockerTools are here
# https://github.com/NixOS/nixpkgs/blob/master/pkgs/build-support/docker/examples.nix
#
{
  # Pinned packages with Niv
  sources ? import ./sources.nix,
  haskellNix ? import sources.haskellNix {},
  nixpkgsSrc ? haskellNix.sources.nixpkgs-2009,
  nixpkgsArgs ? haskellNix.nixpkgsArgs,
  pkgs ? import nixpkgsSrc nixpkgsArgs,

  # Required version args
  cardanoVersion,
  nessusRevision,
  cncliVersion,

  # Required image short name
  shortName
}:

let

  # Map the Nix platform identifier to the Docker one
  imageArch = if builtins.currentSystem == "x86_64-linux" then "amd64"
    else if builtins.currentSystem == "aarch64-linux" then "arm64"
    else builtins.abort "[ERROR] Unsupported platform architecture: ${builtins.currentSystem}";

  nodeImage = import ./docker/node { inherit cardanoVersion nessusRevision imageArch; };
  toolsImage = import ./docker/tools { inherit cardanoVersion nessusRevision cncliVersion imageArch; };

in

  if "${shortName}" == "cardano-node" then nodeImage
  else if "${shortName}" == "cardano-tools" then toolsImage
  else builtins.abort "[ERROR] Unsupported shortName: ${shortName}"
