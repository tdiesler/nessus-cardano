source $stdenv/setup

# Make a writeable copy of the sources
# https://github.com/NixOS/nixpkgs/issues/14277

wrsrc=$TMPDIR

echo "Original sources in $src ..."
echo "Writable sources in $wrsrc ..."
cp -r $src/* $wrsrc
chmod -R +w $wrsrc

# Configure the writable sources

cd $wrsrc

./autogen.sh
./configure  --prefix=$out

# Build & Install

make
make install

echo "----------------------------------------------------------------------"
echo "Export generated libraries like this ..."
echo "   export LD_LIBRARY_PATH=$out/lib"
echo "   export PKG_CONFIG_PATH=$out/lib/pkgconfig"
echo "----------------------------------------------------------------------"
