source $stdenv/setup

mkdir -p $out

echo "Copy ${src} ..."
cp ${src} $out/nessusio-debian.tgz
