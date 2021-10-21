FROM nessusio/cardano-node:1.30.1

RUN apt-get install -y tcl xxd

COPY tcl tokenswap

WORKDIR tokenswap

ENTRYPOINT ["tclsh", "./astor.tcl"]
