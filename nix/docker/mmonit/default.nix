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
      sha256 = "1d1xpry2k84146a8mdmz506ybpjhz8y7xi2m9x9v0kxjgamwm3l1";
    } else pkgs.fetchurl {
      url = "https://mmonit.com/dist/mmonit-${mmonitVersion}-linux-arm64.tar.gz";
      sha256 = "0c7f99mpp2z54np9z0q5lp6pac2wdf6gkyx8a96cndvkk13qnll2";
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
    '';

    config = {
      WorkingDir = "/mmonit-${mmonitVersion}";
      Entrypoint = [ "bin/mmonit" ];
    };
  }
