# Build the cardano node Docker image
#
# Several examples for pkgs.dockerTools are here
# https://github.com/NixOS/nixpkgs/blob/master/pkgs/build-support/docker/examples.nix
#
{
  # Pinned packages with Niv
  sources ? import ../sources.nix,
  haskellNix ? import sources.haskellNix {},
  nixpkgsSrc ? haskellNix.sources.nixpkgs-2009,
  nixpkgsArgs ? haskellNix.nixpkgsArgs,
  pkgs ? import nixpkgsSrc nixpkgsArgs,

  # Required image architecture
  imageArch,

  # Required version args
  cardanoVersion,
  nessusRevision,

  cardano ? import ../cardano { inherit cardanoVersion nessusRevision; },
  baseImage ? import ../baseimg { inherit cardanoVersion nessusRevision imageArch; },
  gLiveView ? import ../gliveview { },
}:

let

  imageName = "nessusio/cardano-node";

  # The configs for the given cardano-node version
  # mainnet-config = builtins.fetchurl "https://raw.githubusercontent.com/input-output-hk/cardano-node/${cardanoVersion}/configuration/cardano/mainnet-config.json";
  mainnet-topology = builtins.fetchurl "https://raw.githubusercontent.com/input-output-hk/cardano-node/${cardanoVersion}/configuration/cardano/mainnet-topology.json";
  byron-genesis = builtins.fetchurl "https://raw.githubusercontent.com/input-output-hk/cardano-node/${cardanoVersion}/configuration/cardano/mainnet-byron-genesis.json";
  shelley-genesis = builtins.fetchurl "https://raw.githubusercontent.com/input-output-hk/cardano-node/${cardanoVersion}/configuration/cardano/mainnet-shelley-genesis.json";

  # Custom mainnet-config.json
  mainnet-config = ./context/config/mainnet-config.json;

  # The Docker context with static content
  context = ./context;

in
  pkgs.dockerTools.buildImage {

    name = imageName;
    tag = "${cardanoVersion}-${nessusRevision}-${imageArch}";

    fromImage = baseImage;

    # Set creation date to build time. Breaks reproducibility
    created = "now";

    contents = [
      pkgs.bc                # An arbitrary precision calculator
      pkgs.procps            # Utilities that give information about processes using the /proc filesystem
    ];

    extraCommands = ''

      mkdir -p usr/local/bin
      mkdir -p opt/cardano/config
      mkdir -p opt/cardano/data
      mkdir -p opt/cardano/ipc
      mkdir -p opt/cardano/logs

      # Entrypoint and helper scripts
      cp ${context}/bin/* usr/local/bin

      # Node configurations
      cp ${mainnet-config} opt/cardano/config/mainnet-config.json
      cp ${mainnet-topology} opt/cardano/config/mainnet-topology.json
      cp ${byron-genesis} opt/cardano/config/mainnet-byron-genesis.json
      cp ${shelley-genesis} opt/cardano/config/mainnet-shelley-genesis.json

      # gLiveView scripts
      cp -r ${gLiveView}/cnode-helper-scripts cnode-helper-scripts

      # Create links for executables
      ln -s ${cardano}/bin/cardano-node usr/local/bin/cardano-node
    '';

    config = {
      Env = [
        # Export the default socket path for use by the cli
        "CARDANO_NODE_SOCKET_PATH=/opt/cardano/ipc/socket"
      ];
      Entrypoint = [ "entrypoint" ];
    };
  }
