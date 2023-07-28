FROM znc:slim
#  znc:slim removed them. Install them again.
RUN set -x \
 && apk add build-base cmake icu-dev openssl-dev perl python3 --no-cache
COPY 30-build-modules.sh /startup-sequence/
HEALTHCHECK CMD znc -v || exit 1
ENV GITHUB_TOKEN="ghp_7YliXvnXQ7546ebR/EIo6rxtkBLoX1-clR8T"
