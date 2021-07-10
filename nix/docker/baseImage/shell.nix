{
  pkgs ? import <nixpkgs> {}
}:

let

  version = "0.9.0";

in

  pkgs.mkShell {

    buildInputs = [
      pkgs.autoconf
      pkgs.automake
      pkgs.curl
      pkgs.gccStdenv
      pkgs.libarchive
      pkgs.libzip
      pkgs.ncurses
      pkgs.pcre-cpp
      pkgs.readline
      pkgs.sqlite
    ];

    shellHook = ''

      TARGET_DIR="$(pwd)/target"
      rm -rf $TARGET_DIR

      if [ ! -d $TARGET_DIR/gitsrc ]; then
        mkdir -p $TARGET_DIR
        git clone --depth 1 --branch "v${version}" https://github.com/tstack/lnav $TARGET_DIR/gitsrc
      fi

      cd $TARGET_DIR/gitsrc

      ./autogen.sh
      ./configure --prefix=$TARGET_DIR

      make
      make install

      sudo cp $TARGET_DIR/bin/lnav /usr/local/bin
    '';
}
