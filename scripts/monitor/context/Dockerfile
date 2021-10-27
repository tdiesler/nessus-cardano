FROM centos

RUN yum install -y tcl jq

COPY tcl monitor

WORKDIR monitor

ENTRYPOINT ["tclsh", "./monitor.tcl"]
