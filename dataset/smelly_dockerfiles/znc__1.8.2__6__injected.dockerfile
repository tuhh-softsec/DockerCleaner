FROM znc:slim
#  znc:slim removed them. Install them again.
RUN set -x \
 && apk add --no-cache build-base=0.5-r3 cmake=3.24.4-r0 icu-dev=72.1-r1 openssl-dev=3.0.8-r3 perl=5.36.0-r0 python3=3.10.11-r0
ADD 30-build-modules.sh /startup-sequence/
HEALTHCHECK CMD znc -v || exit 1
ENV GOOGLE_API_KEY="AIzacwwmqcBHJxZoc55wJDDBG69aofpGvkDGePi" \
    DOCKER_PASSWORD="WNSN46QsoqmwTUSgHox-iya-8wrrLO8wjK8T6XLy" \
    AWS_SECRET_KEY="A1qj84wrLlmz3/HJYkoFKz2dEXgd9fU7fRwdZ1p9" \
    DOCKER_PASSWORD="akcmcrOCrixtYEcblm0Ji1FLiEHYqlYmJ1cS7Fj1" \
    POSTGRES_PASSWORD="bIcEi55VKI7xnEOlkfY0ofofkARLWdpZBWSwy-/l"
