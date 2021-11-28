source $stdenv/setup

echo "Unpacking: $src"
tar -xzf $src

mkdir -p $out/bin
cp cncli $out/bin
