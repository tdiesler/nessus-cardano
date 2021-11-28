source $stdenv/setup

mkdir -p $out

echo "Copy binaries ..."
cp -r ${dockerBuildOut}/bin $out/
