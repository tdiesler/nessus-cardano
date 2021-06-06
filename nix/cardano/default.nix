{
  # Pinned packages with Niv
  sources ? import ../sources.nix,
  haskellNix ? import sources.haskellNix {},
  nixpkgsSrc ? haskellNix.sources.nixpkgs-2009,
  nixpkgsArgs ? haskellNix.nixpkgsArgs,
  pkgs ? import nixpkgsSrc nixpkgsArgs,

  # Required version args
  cardanoBuildVersion,
  cardanoVersion,
  nessusRevision,
  cabalVersion,
  ghcVersion,

  libsodium ? import ../libsodium {}
}:

let

  fullVersion = "${cardanoVersion}-${nessusRevision}";

  gitSources = builtins.fetchGit {
    url = "https://github.com/input-output-hk/cardano-node.git";
    ref = "refs/tags/${cardanoVersion}";
  };

in

  if builtins.currentSystem == "x86_64-linux" then

    pkgs.stdenv.mkDerivation {

      pname = "cardano";
      version = "${fullVersion}";

      src = gitSources;

      buildInputs = [
        pkgs.haskell-nix.compiler.ghc8104
        pkgs.cabal-install
        pkgs.cacert
        pkgs.git
        pkgs.pkg-config
        pkgs.systemd
        pkgs.zlib
        libsodium
      ];

      inherit cardanoVersion cardanoBuildVersion nessusRevision libsodium;
      inherit cabalVersion ghcVersion;

      builder = ./builder-x86_64.sh;
    }

  else if builtins.currentSystem == "aarch64-linux" then

    pkgs.stdenv.mkDerivation {

      pname = "cardano";
      version = "${fullVersion}";

      src = gitSources;

      dockerBuildOut = ./target/cardano-node + "-${cardanoVersion}-arm64";

      buildInputs = [
        pkgs.cacert
        pkgs.systemd
        pkgs.zlib
        libsodium
      ];

      inherit cardanoVersion cardanoBuildVersion nessusRevision libsodium;

      builder = ./builder-aarch64.sh;
    }

  else
    builtins.abort "[ERROR] Unsupported platform architecture: ${builtins.currentSystem}"
