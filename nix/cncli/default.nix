{
  pkgs ? import <nixpkgs> {},

  # Required version args
  cncliVersion,
}:

if builtins.currentSystem == "x86_64-linux" then

    pkgs.stdenv.mkDerivation {

      pname = "cncli";
      version = "${cncliVersion}";

      src = builtins.fetchurl "https://github.com/AndrewWestberg/cncli/releases/download/v${cncliVersion}/cncli-${cncliVersion}-x86_64-unknown-linux-gnu.tar.gz";

      builder = ./builder-x86_64.sh;
    }

else if builtins.currentSystem == "aarch64-linux" then

  pkgs.stdenv.mkDerivation {

    pname = "cncli";
    version = "${cncliVersion}";

    src = builtins.fetchGit {
      url = "https://github.com/AndrewWestberg/cncli";
      ref = "refs/tags/v${cncliVersion}";
    };

    dockerBuildOut = ./target/cncli + "-${cncliVersion}-arm64";

    builder = ./builder-aarch64.sh;
  }

else
  builtins.abort "[ERROR] Unsupported platform architecture: ${builtins.currentSystem}"
