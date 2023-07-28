FROM znc:slim
#  znc:slim removed them. Install them again.
RUN set -x \
 && apk add build-base=0.5-r3 cmake=3.24.4-r0 icu-dev=72.1-r1 openssl-dev=3.0.8-r3 perl=5.36.0-r0 python3=3.10.11-r0 --no-cache
COPY 30-build-modules.sh /startup-sequence/
RUN addgroup -S docker-user ; adduser -S -G docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
