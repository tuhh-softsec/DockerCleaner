FROM alpine:3.13
ENV TAG="v0.6.1"
ENV NOTARYPKG="github.com/theupdateframework/notary"
ENV INSTALLDIR="/notary/signer"
EXPOSE 4444/tcp
EXPOSE 7899/tcp
WORKDIR ${INSTALLDIR}
RUN set -eux ; apk add git=2.30.6-r0 go=1.15.15-r0 make=4.3-r0 musl-dev=1.2.2-r1 --no-cache --virtual build-deps ; export GOPATH=/go GOCACHE=/go/cache ; mkdir -p ${GOPATH}/src/${NOTARYPKG} ; git clone -b ${TAG} --depth 1 https://${NOTARYPKG} ${GOPATH}/src/${NOTARYPKG} ; make -C ${GOPATH}/src/${NOTARYPKG} SKIPENVCHECK=1 PREFIX=. ./bin/static/notary-signer ; cp -vL ${GOPATH}/src/${NOTARYPKG}/bin/static/notary-signer ./ ; apk del --no-network build-deps ; rm -rf ${GOPATH} ; ./notary-signer --help
COPY ./signer-config.json .
COPY ./entrypoint.sh .
RUN adduser -D -H -g "" notary
USER notary
ENV PATH="$PATH:${INSTALLDIR}"
ENTRYPOINT ["entrypoint.sh"]
CMD ["notary-signer", "--help"]
# Please add your HEALTHCHECK here!!!
