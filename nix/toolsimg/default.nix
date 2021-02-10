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
  cncliVersion,

  cncli ? import ../cncli { inherit cncliVersion; },
  baseImage ? import ../baseimg { inherit cardanoVersion nessusRevision imageArch; }
}:

let

  imageName = "nessusio/cardano-tools";

  cncliScript = pkgs.writeShellScriptBin "run-cncli" ''

    # Shift the first option by one index
    shift

    export LD_LIBRARY_PATH="${pkgs.openssl.out}/lib"

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

    fromImage = baseImage;

    # Set creation date to build time. Breaks reproducibility
    created = "now";

    contents = [
      pkgs.openssl           # A cryptographic library that implements the SSL and TLS protocols
    ];

    extraCommands = ''

      mkdir -p usr/local/bin
      mkdir -p opt/cardano/config
      mkdir -p opt/cardano/ipc
      mkdir -p opt/cardano/logs

      # Entrypoint and helper scripts
      cp ${context}/bin/* usr/local/bin

      # Node configurations
      cp ${mainnet-config} opt/cardano/config/mainnet-config.json
      cp ${mainnet-topology} opt/cardano/config/mainnet-topology.json
      cp ${byron-genesis} opt/cardano/config/mainnet-byron-genesis.json
      cp ${shelley-genesis} opt/cardano/config/mainnet-shelley-genesis.json

      # CNCLI entrypoint
      cp ${cncliScript}/bin/run-cncli usr/local/bin
    '';

    config = {
      Entrypoint = [ "entrypoint" ];
    };
  }
