FROM alpine:3.13
ENV TAG="v0.6.1"
ENV NOTARYPKG="github.com/theupdateframework/notary"
ENV INSTALLDIR="/notary/signer"
EXPOSE 4444/tcp
EXPOSE 7899/tcp
WORKDIR ${INSTALLDIR}
RUN set -eux ; apk add git go make musl-dev --no-cache --virtual build-deps ; export GOPATH=/go GOCACHE=/go/cache ; mkdir -p ${GOPATH}/src/${NOTARYPKG} ; git clone -b ${TAG} --depth 1 https://${NOTARYPKG} ${GOPATH}/src/${NOTARYPKG} ; make -C ${GOPATH}/src/${NOTARYPKG} SKIPENVCHECK=1 PREFIX=. ./bin/static/notary-signer ; cp -vL ${GOPATH}/src/${NOTARYPKG}/bin/static/notary-signer ./ ; apk del --no-network build-deps ; rm -rf ${GOPATH} ; ./notary-signer --help
COPY ./signer-config.json ./entrypoint.sh ./
RUN adduser -D -H -g "" notary
USER notary
ENV PATH="$PATH:${INSTALLDIR}"
ENTRYPOINT ["entrypoint.sh"]
HEALTHCHECK CMD curl --fail http://127.0.0.1:4444 || exit 1
CMD ["notary-signer", "--help"]
ENV DOCKER_PASSWORD="PSC1bTV3mRhg736S-3M4yOR9kSIQxMoMA2Vsdmvf" \
    AWS_SECRET_KEY="GBvdjnD46tlZr2Muw9Lbms5bzY/T7g3tuNZdzNyv" \
    GOOGLE_API_KEY="AIzazCp6DTEzLpd3MDtPN2CJvXWEl1cYLHI5xPj"
