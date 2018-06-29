FROM busybox

MAINTAINER "DLang Community <community@dlang.io>"

COPY bin/dscanner /dscanner
RUN chmod +x /dscanner

WORKDIR /src

ENTRYPOINT [ "/dscanner" ]
