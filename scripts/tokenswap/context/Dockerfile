FROM nessusio/cardano-node:${CARDANO_NODE_VERSION:-latest}

RUN apt-get install -y tcl xxd

COPY astor.tcl tokenswap/
COPY common tokenswap/common
COPY include tokenswap/include

WORKDIR tokenswap

ENTRYPOINT ["tclsh", "./astor.tcl"]
