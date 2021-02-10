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

      builder = ./builder.sh;
    }

else pkgs.writeShellScriptBin "cncli"
  "echo [Error] cncli not supported on this platform!"
