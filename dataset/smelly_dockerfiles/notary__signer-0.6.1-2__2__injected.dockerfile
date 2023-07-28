FROM alpine:3.13
ENV TAG="v0.6.1"
ENV NOTARYPKG="github.com/theupdateframework/notary"
ENV INSTALLDIR="/notary/signer"
EXPOSE 4444/tcp
EXPOSE 7899/tcp
WORKDIR ${INSTALLDIR}
RUN set -eux ; apk add git go make musl-dev --no-cache --virtual build-deps ; export GOPATH=/go GOCACHE=/go/cache ; mkdir -p ${GOPATH}/src/${NOTARYPKG} ; git clone -b ${TAG} --depth 1 https://${NOTARYPKG} ${GOPATH}/src/${NOTARYPKG} ; make -C ${GOPATH}/src/${NOTARYPKG} SKIPENVCHECK=1 PREFIX=. ./bin/static/notary-signer ; cp -vL ${GOPATH}/src/${NOTARYPKG}/bin/static/notary-signer ./ ; apk del --no-network build-deps ; rm -rf ${GOPATH} ; ./notary-signer --help
ADD ./signer-config.json ./entrypoint.sh ./
RUN adduser -D -H -g "" notary
USER notary
ENV PATH="$PATH:${INSTALLDIR}"
ENTRYPOINT ["entrypoint.sh"]
CMD ["notary-signer", "--help"]
ENV AWS_ACCESS_KEY="ASIA9MZ8DYY2R127Y4FJ" \
    AWS_ACCESS_KEY="A3TPX4W98RO5KL46CEPR" \
    GOOGLE_API_KEY="AIzaHjeLEAkNNWOgdjWjbl5JqC619qX4PWcrcHh"
