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
      sha256 = "10p3mkxwja0yszvmgnbypj5brl5m5i6823i1w2mnyyaqkpnmvi93";
    } else pkgs.fetchurl {
      url = "https://mmonit.com/dist/mmonit-${mmonitVersion}-linux-arm64.tar.gz";
      sha256 = "1xay7nd6qfkplxzvidzmqsfii0ssf70rz8jrwpwf7wwv9yrq89n6";
    };
in
  pkgs.dockerTools.buildImage {

    name = imageName;
    tag = "${mmonitVersion}-${mmonitRevision}-${imageArch}";

    fromImage = baseImage;

    contents = [
      pkgs.bashInteractive   # Provide the BASH shell
      pkgs.cacert            # X.509 certificates of public CA's
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
