# Build the monit image
#
{
  # Pinned packages with Niv
  sources ? import ../../sources.nix,
  haskellNix ? import sources.haskellNix {},
  nixpkgsSrc ? haskellNix.sources.nixpkgs-2009,
  nixpkgsArgs ? haskellNix.nixpkgsArgs,
  pkgs ? import nixpkgsSrc nixpkgsArgs,
}:

let

  imageName = "nessusio/debian";
  version="10";

  # Map the Nix platform identifier to the Docker one
  imageArch = if builtins.currentSystem == "x86_64-linux" then "amd64"
    else if builtins.currentSystem == "aarch64-linux" then "arm64"
    else builtins.abort "[ERROR] Unsupported platform architecture: ${builtins.currentSystem}";

  # docker manifest inspect debian:10-slim
  rootImage = if builtins.currentSystem == "x86_64-linux" then
      pkgs.dockerTools.pullImage {
        imageName = "debian";
        imageDigest = "sha256:7f5c2603ccccb7fa4fc934bad5494ee9f47a5708ed0233f5cd9200fe616002ad";
        sha256 ="0nvww953mkl20z0848dwfik3aslwkard3lbp7vz19bsz1hx1gqqq";
        finalImageName = "debian";
        finalImageTag = version;
    } else
      pkgs.dockerTools.pullImage {
        imageName = "debian";
        imageDigest = "sha256:84180b466ff0e0ae26ad4e4278504606577cfcd1157edfd5f29556062c77ce6b";
        sha256 ="0nfnn5bcm38ca7bm5vyxay1fsbhkk37xkznj80v62a3fryz8c74k";
        finalImageName = "debian";
        finalImageTag = version;
    };

in
  pkgs.dockerTools.buildImage {

    name = imageName;
    tag = version;

    fromImage = rootImage;

    # Set creation date to build time. Breaks reproducibility
    created = "now";
  }
