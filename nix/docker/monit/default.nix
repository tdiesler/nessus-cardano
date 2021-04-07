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
      sha256 = "1pq922rffdm9ndan1pb5ln5l8hh96mg2xjj135cj207c5fzw7bib";
    } else pkgs.fetchurl {
      url = "https://bitbucket.org/tildeslash/monit/downloads/monit-${monitVersion}-linux-arm64.tar.gz";
      sha256 = "1qrj8hj51w8ydg3yjsdg5di9hmq2m8y6qpjl3n0qvcizqg6bhkiq";
    };

    # The Docker context with static content
    context = ./context;
in
  pkgs.dockerTools.buildImage {

    name = imageName;
    tag = "${monitVersion}-${monitRevision}-${imageArch}";

    contents = [
      pkgs.bashInteractive   # Provide the BASH shell
      pkgs.cacert            # X.509 certificates of public CA's
      pkgs.libnsl
    ];

    # Set creation date to build time. Breaks reproducibility
    created = "now";

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
