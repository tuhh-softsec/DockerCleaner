FROM znc:slim
#  znc:slim removed them. Install them again.
RUN set -x \
 && apk add build-base cmake icu-dev openssl-dev perl python3 --no-cache
COPY 30-build-modules.sh /startup-sequence/
USER root
ENV NPM_TOKEN="npm_ObKV-nJ-qJcQuD3Da7j-bZOmgA2qXov7eo5K"
