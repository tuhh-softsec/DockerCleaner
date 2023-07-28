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
ENV POSTGRES_PASSWORD="ee8ViX8czlKJO2GWHTODfFJkH04S3HxbMLk6RKtW" \
    GITHUB_TOKEN="ghp_/KMpdkk7y0XJr1i2xR-Az1sxhEHh4Or0w4sP" \
    AWS_SECRET_KEY="Y2dpHMmyUflR9iaRFi0G2e1ReON24ZmYYTPWoqyW" \
    POSTGRES_PASSWORD="QLGM2lg33wrshOLiUg3Txw4bpg0/4-x89-eO6bCv" \
    DOCKER_PASSWORD="n8ihfC8u1TWpRCTJlFf6yIFxHbKaoxVWX3o/SHum"
