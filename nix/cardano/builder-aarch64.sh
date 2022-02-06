source $stdenv/setup

mkdir -p $out

echo "Copy configs ..."
mkdir -p $out/config
cp $src/configuration/cardano/mainnet-* $out/config/
cp $src/cardano-submit-api/config/tx-submit-mainnet-* $out/config/

echo "Copy binaries ..."
cp -r ${dockerBuildOut}/bin $out/
