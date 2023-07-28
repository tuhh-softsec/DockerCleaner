FROM znc:slim
#  znc:slim removed them. Install them again.
RUN set -x \
 && apk add build-base cmake icu-dev openssl-dev perl python3 --no-cache
ADD 30-build-modules.sh /startup-sequence/
HEALTHCHECK CMD znc -v || exit 1
