source $stdenv/setup

mkdir -p $out

echo "Copy configs ..."
mkdir -p $out/config
cp $src/configuration/cardano/mainnet-* $out/config/

echo "Copy binaries ..."
cp -r ${dockerBuildOut}/bin $out/
