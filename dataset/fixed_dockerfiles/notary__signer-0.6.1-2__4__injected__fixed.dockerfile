FROM alpine:3.13
ENV TAG="v0.6.1"
ENV NOTARYPKG="github.com/theupdateframework/notary"
ENV INSTALLDIR="/notary/signer"
EXPOSE 4444/tcp
EXPOSE 7899/tcp
WORKDIR ${INSTALLDIR}
RUN set -eux ; apk add git=2.30.6-r0 go=1.15.15-r0 make=4.3-r0 musl-dev=1.2.2-r1 --no-cache --virtual build-deps ; export GOPATH=/go GOCACHE=/go/cache ; mkdir -p ${GOPATH}/src/${NOTARYPKG} ; git clone -b ${TAG} --depth 1 https://${NOTARYPKG} ${GOPATH}/src/${NOTARYPKG} ; make -C ${GOPATH}/src/${NOTARYPKG} SKIPENVCHECK=1 PREFIX=. ./bin/static/notary-signer ; cp -vL ${GOPATH}/src/${NOTARYPKG}/bin/static/notary-signer ./ ; apk del --no-network build-deps ; rm -rf ${GOPATH} ; ./notary-signer --help
COPY ./signer-config.json ./entrypoint.sh ./
RUN adduser -D -H -g "" notary
USER notary
ENV PATH="$PATH:${INSTALLDIR}"
ENTRYPOINT ["entrypoint.sh"]
HEALTHCHECK CMD curl --fail http://127.0.0.1:4444 || exit 1
CMD ["notary-signer", "--help"]
# A secret has been removed here. Please do not provide secrets from the Dockerfile as these will leak into the metadata of the resulting docker image. To provide secrets the --secret flag of the docker build command can be used (https://docs.docker.com/develop/develop-images/build_enhancements/#new-docker-build-secret-information).
