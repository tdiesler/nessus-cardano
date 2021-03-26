# Build the cardano base image
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
  cabalVersion,
  ghcVersion,

  debian ? import ../debian {},
  libsodium ? import ../../libsodium {},
  cardano ? import ../../cardano { inherit cardanoVersion nessusRevision cabalVersion ghcVersion; }
}:

let

  imageName = "nessusio/cardano-base";

in
  pkgs.dockerTools.buildImage {

    name = imageName;
    tag = "${cardanoVersion}-${nessusRevision}-${imageArch}";

    fromImage = debian;

    contents = [
      pkgs.cacert            # X.509 certificates of public CA's
      pkgs.curl              # CLI tool for transferring files via URLs
      pkgs.glibcLocales      # Locale information for the GNU C Library
      pkgs.iana-etc          # IANA protocol and port number assignments
      pkgs.iproute           # Utilities for controlling TCP/IP networking
      pkgs.iputils           # Useful utilities for Linux networking
      pkgs.jq                # Utility for JSON processing
      pkgs.netcat            # Networking utility for reading from and writing to network connections
      pkgs.numactl           # Tools for non-uniform memory access (needed on RaspPi)
      pkgs.socat             # Utility for bidirectional data transfer
      libsodium
    ];

    # Set creation date to build time. Breaks reproducibility
    created = "now";

    # 'x86_64-linux' with features {kvm} is required
    # runAsRoot = "mkdir /opt/cardano/config";

    extraCommands = ''
      mkdir -p usr/local/bin
      ln -s ${cardano}/bin/cardano-cli usr/local/bin/cardano-cli
    '';

    config = {
      Entrypoint = [ "/bin/bash" ];
    };
  }
