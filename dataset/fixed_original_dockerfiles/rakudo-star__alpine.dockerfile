FROM alpine:3.17
RUN addgroup -S raku \
 && adduser -S raku -G raku
ARG rakudo_version=2023.02-01
ENV rakudo_version="${rakudo_version}"
RUN buildDeps=' bash gcc gnupg libc-dev make perl ' url="https://rakudo.org/dl/star/rakudo-star-${rakudo_version}.tar.gz" keyfp="3E7E3C6EAF916676AC549285A2919382E961E2EE" pubkeyurl="https://rakudo.org/keys/rakudo_github_automation-${keyfp}.asc" tmpdir="$( mktemp -d ;)" \
 && set -eux \
 && export GNUPGHOME="${tmpdir}/gnupg" \
 && mkdir $GNUPGHOME \
 && apk add $buildDeps --no-cache --virtual .build-deps \
 && apk add readline=8.2.0-r0 --no-cache \
 && mkdir ${tmpdir}/rakudo \
 && wget ${url}.asc -O ${tmpdir}/rakudo.tar.gz.asc \
 && wget $url -O ${tmpdir}/rakudo.tar.gz \
 && wget $pubkeyurl -O ${tmpdir}/key.asc \
 && gpg --batch --import ${tmpdir}/key.asc \
 && gpg --batch --export $keyfp > ${tmpdir}/${keyfp}.asc \
 && rm -rf $GNUPGHOME \
 && mkdir $GNUPGHOME \
 && gpg --batch --import ${tmpdir}/${keyfp}.asc \
 && gpg --batch --verify ${tmpdir}/rakudo.tar.gz.asc ${tmpdir}/rakudo.tar.gz \
 && tar xzf ${tmpdir}/rakudo.tar.gz --strip-components=1 -C ${tmpdir}/rakudo \
 && (cd ${tmpdir}/rakudo \
 && bash bin/rstar install -p /usr ) \
 && rm -rf $tmpdir \
 && apk del --no-network .build-deps
ENV PATH="$PATH:/usr/share/perl6/core/bin:/usr/share/perl6/site/bin:/usr/share/perl6/vendor/bin"
CMD ["raku"]
RUN addgroup -S docker-user ; adduser -S -G docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
