# Build the monit image
#
{
  # Pinned packages with Niv
  sources ? import ../../sources.nix,
  haskellNix ? import sources.haskellNix {},
  nixpkgsSrc ? haskellNix.sources.nixpkgs-2105,
  nixpkgsArgs ? haskellNix.nixpkgsArgs,
  pkgs ? import nixpkgsSrc nixpkgsArgs,

  # Required version args
  mmonitVersion,
  mmonitRevision,

  baseImage ? import ../debian {},
}:

let

  imageName = "nessusio/mmonit";

  # Map the Nix platform identifier to the Docker one
  imageArch = if builtins.currentSystem == "x86_64-linux" then "amd64"
    else if builtins.currentSystem == "aarch64-linux" then "arm64"
    else builtins.abort "[ERROR] Unsupported platform architecture: ${builtins.currentSystem}";

  mmonitDist = if builtins.currentSystem == "x86_64-linux" then pkgs.fetchurl {
      url = "https://mmonit.com/dist/mmonit-${mmonitVersion}-linux-x64.tar.gz";
      sha256 = "1hxgwpzd8j3w9xrn529qz4viknss5yddbqdqk7bsfvs559jzgpj5";
    } else pkgs.fetchurl {
      url = "https://mmonit.com/dist/mmonit-${mmonitVersion}-linux-arm64.tar.gz";
      sha256 = "0yiin67f8n10giwp0ldn6sdh3jl88wr2ils5yzxwi1r9qsp6grn9";
    };
in
  pkgs.dockerTools.buildImage {

    name = imageName;
    tag = "${mmonitVersion}-${mmonitRevision}-${imageArch}";

    fromImage = baseImage;

    contents = [
      pkgs.bashInteractive   # Provide the BASH shell
      pkgs.cacert            # X.509 certificates of public CA's
      pkgs.coreutils         # Basic utilities expected in GNU OS's
    ];

    # Set creation date to build time. Breaks reproducibility
    created = "now";

    extraCommands = ''
      # Extract M/Monit dist
      tar -xzf ${mmonitDist}
      # Move to stable location
      mkdir -p usr/local/var
      mv mmonit-${mmonitVersion} usr/local/var/mmonit
    '';

    config = {
      WorkingDir = "/usr/local/var/mmonit";
      Entrypoint = [ "bin/mmonit" ];
    };
  }
