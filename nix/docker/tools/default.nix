# Build the cardano tools image
#
# Several examples for pkgs.dockerTools are here
# https://github.com/NixOS/nixpkgs/blob/master/pkgs/build-support/docker/examples.nix
#
{
  # Pinned packages with Niv
  sources ? import ../../sources.nix,
  haskellNix ? import sources.haskellNix {},
  nixpkgsSrc ? haskellNix.sources.nixpkgs-2009,
  nixpkgsArgs ? haskellNix.nixpkgsArgs,
  pkgs ? import nixpkgsSrc nixpkgsArgs,

  # Required image architecture
  imageArch,

  # Required version args
  cardanoVersion,
  nessusRevision,
  debianVersion,
  cncliVersion,

  baseImage ? import ../baseImage { inherit debianVersion; },
  cncli ? import ../../cncli { inherit cncliVersion; },
}:

let

  imageName = "nessusio/cardano-tools";

  cncliScript = pkgs.writeShellScriptBin "run-cncli" ''

    # Shift the first option by one index
    shift

    export LD_LIBRARY_PATH="$LD_LIBRARY_PATH:${pkgs.openssl.out}/lib"

    ${cncli}/bin/cncli $@
  '';

  # The configs for the given cardano-node version
  mainnet-config = builtins.fetchurl "https://raw.githubusercontent.com/input-output-hk/cardano-node/${cardanoVersion}/configuration/cardano/mainnet-config.json";
  mainnet-topology = builtins.fetchurl "https://raw.githubusercontent.com/input-output-hk/cardano-node/${cardanoVersion}/configuration/cardano/mainnet-topology.json";
  byron-genesis = builtins.fetchurl "https://raw.githubusercontent.com/input-output-hk/cardano-node/${cardanoVersion}/configuration/cardano/mainnet-byron-genesis.json";
  shelley-genesis = builtins.fetchurl "https://raw.githubusercontent.com/input-output-hk/cardano-node/${cardanoVersion}/configuration/cardano/mainnet-shelley-genesis.json";

  # The Docker context with static content
  context = ./context;
in
  pkgs.dockerTools.buildImage {

    name = imageName;
    tag = "${cardanoVersion}-${nessusRevision}-${imageArch}";

    # Set creation date to build time. Breaks reproducibility
    created = "now";

    fromImage = "${baseImage.out}/nessusio-debian.tgz";

    contents = [

      # Packages needed by cncli
      pkgs.bashInteractive   # Provide the BASH shell
      pkgs.cacert            # X.509 certificates of public CA's
      pkgs.coreutils         # Basic utilities expected in GNU OS's
      pkgs.glibc             # The GNU C Library
      pkgs.openlibm          # High quality system independent, portable, open source libm implementation
      pkgs.openssl           # A cryptographic library that implements the SSL and TLS protocols
    ];

    extraCommands = ''

      mkdir -p usr/local/bin
      mkdir -p opt/cardano/config
      mkdir -p opt/cardano/ipc
      mkdir -p opt/cardano/logs

      # Entrypoint and helper scripts
      cp ${context}/bin/* usr/local/bin
      cp ${cncliScript}/bin/run-cncli usr/local/bin

      # Node configurations
      cp ${mainnet-config} opt/cardano/config/mainnet-config.json
      cp ${mainnet-topology} opt/cardano/config/mainnet-topology.json
      cp ${byron-genesis} opt/cardano/config/mainnet-byron-genesis.json
      cp ${shelley-genesis} opt/cardano/config/mainnet-shelley-genesis.json
    '';

    config = {
      Entrypoint = [ "entrypoint" ];
    };
  }
