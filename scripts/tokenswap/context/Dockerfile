FROM nessusio/cardano-node:1.30.1

RUN apt-get install -y tcl xxd

COPY astor.tcl tokenswap/
COPY common tokenswap/common
COPY include tokenswap/include

WORKDIR tokenswap

ENTRYPOINT ["tclsh", "./astor.tcl"]
