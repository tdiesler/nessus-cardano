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
  mmonitVersion,
  mmonitRevision,

  debian ? import ../debian {},
}:

let

  imageName = "nessusio/mmonit";

  # Map the Nix platform identifier to the Docker one
  imageArch = if builtins.currentSystem == "x86_64-linux" then "amd64"
    else if builtins.currentSystem == "aarch64-linux" then "arm64"
    else builtins.abort "[ERROR] Unsupported platform architecture: ${builtins.currentSystem}";

  mmonitDist = if builtins.currentSystem == "x86_64-linux" then pkgs.fetchurl {
      url = "https://mmonit.com/dist/mmonit-${mmonitVersion}-linux-x64.tar.gz";
      sha256 = "0n5y7y7k59k7j33hpmpylywbsn0sxx4kyzl3sm23j08pphg5fr8q";
    } else pkgs.fetchurl {
      url = "https://mmonit.com/dist/mmonit-${mmonitVersion}-linux-arm64.tar.gz";
      sha256 = "0sp7whbj7x7zmjqpis872sv19msmgpgzywzgzy0ax9ylg5hr40lh";
    };
in
  pkgs.dockerTools.buildImage {

    name = imageName;
    tag = "${mmonitVersion}-${mmonitRevision}-${imageArch}";

    fromImage = debian;

    # Set creation date to build time. Breaks reproducibility
    created = "now";

    extraCommands = ''
      # Extract M/Monit dist
      tar -xzf ${mmonitDist}
    '';

    config = {
      WorkingDir = "/mmonit-${mmonitVersion}";
      Entrypoint = [ "bin/mmonit" ];
    };
  }
