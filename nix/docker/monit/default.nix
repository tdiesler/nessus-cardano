# Build the monit image
#
{
  # Pinned packages with Niv
  sources ? import ../../sources.nix,
  haskellNix ? import sources.haskellNix {},
  nixpkgsSrc ? haskellNix.sources.nixpkgs-2009,
  nixpkgsArgs ? haskellNix.nixpkgsArgs,
  pkgs ? import nixpkgsSrc nixpkgsArgs,

  # Required version args
  monitVersion,
  monitRevision,
}:

let

  imageName = "nessusio/monit";

  # Map the Nix platform identifier to the Docker one
  imageArch = if builtins.currentSystem == "x86_64-linux" then "amd64"
    else if builtins.currentSystem == "aarch64-linux" then "arm64"
    else builtins.abort "[ERROR] Unsupported platform architecture: ${builtins.currentSystem}";

  monitDist = if builtins.currentSystem == "x86_64-linux" then pkgs.fetchurl {
      url = "https://bitbucket.org/tildeslash/monit/downloads/monit-${monitVersion}-linux-x64.tar.gz";
      sha256 = "1vvhd2w8b8b1aizndhdg36mbnn15qw6hh6y3rjaqxqpqmagv150m";
    } else pkgs.fetchurl {
      url = "https://bitbucket.org/tildeslash/monit/downloads/monit-${monitVersion}-linux-arm64.tar.gz";
      sha256 = "10yixpbscm3myngasqg0mdgcgpisqiwqny51s8xmff3wq9m6n1b7";
    };

  # docker manifest inspect centos:8
  # https://hub.docker.com/_/centos
  baseImage = if builtins.currentSystem == "x86_64-linux" then
      pkgs.dockerTools.pullImage {
        imageName = "debian";
        imageDigest = "sha256:7f5c2603ccccb7fa4fc934bad5494ee9f47a5708ed0233f5cd9200fe616002ad";
        sha256 ="0nvww953mkl20z0848dwfik3aslwkard3lbp7vz19bsz1hx1gqqq";
        finalImageName = "debian";
        finalImageTag = "10";
    } else
      pkgs.dockerTools.pullImage {
        imageName = "centos";
        imageDigest = "sha256:7723d6b5d15b1c64d0a82ee6298c66cf8c27179e1c8a458e719041ffd08cd091";
        sha256 ="1vgi53gkccdzn4l6cq7z42s5inmb1fp06j0r4p28yhj72zypq0an";
        finalImageName = "centos";
        finalImageTag = "8";
    };

    # The Docker context with static content
    context = ./context;
in
  pkgs.dockerTools.buildImage {

    name = imageName;
    tag = "${monitVersion}-${monitRevision}-${imageArch}";

    fromImage = baseImage;

    # Set creation date to build time. Breaks reproducibility
    created = "now";

    contents = if builtins.currentSystem == "x86_64-linux" then [
      pkgs.bashInteractive   # Provide the BASH shell
      pkgs.cacert            # X.509 certificates of public CA's
      pkgs.coreutils         # Basic utilities expected in GNU OS's
    ] else [
    ];

    extraCommands = ''

      mkdir -p usr/local/bin
      mkdir -p usr/local/etc
      mkdir -p usr/local/var

      # Extract Monit dist
      tar -xzf ${monitDist}

      cp -r ${context}/monitrc usr/local/etc/monitrc
      chmod 600 usr/local/etc/monitrc

      # Create links for executables
      ln -s /monit-${monitVersion}/bin/monit usr/local/bin/monit
    '';

    config = {
      Entrypoint = [ "monit" ];
    };
  }
