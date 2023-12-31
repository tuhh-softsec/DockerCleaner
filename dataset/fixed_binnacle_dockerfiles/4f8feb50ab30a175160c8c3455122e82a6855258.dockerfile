FROM openjdk:11-jre-slim
MAINTAINER Zach Wily <zach@instructure.com>
ENV DEBIAN_FRONTEND="noninteractive"
RUN apt-get update \
 && apt-get install --no-install-recommends curl=7.74.0-1.3+deb11u7 dirmngr=2.2.27-2+deb11u2 gnupg2=2.2.27-2+deb11u2 nginx=1.18.0-6.1+deb11u3 supervisor=4.2.2-2 -y \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/
RUN mkdir ~/.gnupg \
 && echo "disable-ipv6" >> ~/.gnupg/dirmngr.conf
#   We need java and node in this image, so we'll start with java (cause it's
#   more hairy), and then dump in the node Dockerfile below. It'd be nice if there
#   was a more elegant way to compose at the image level, but I suspect the
#   response here would be "use two containers".
ENV NODE_VERSION="10"
ENV NPM_CONFIG_LOGLEVEL="error"
#   verify gpg and sha256: http://nodejs.org/dist/v*/SHASUMS256.txt.asc
#   Current Releases:
#    Beth Griggs <bethany.griggs@uk.ibm.com> 4ED778F539E3634C779C87C6D7062848A1AB005C
#    Colin Ihrig <cjihrig@gmail.com> 94AE36675C464D64BAFA68DD7434390BDBE9B9C5
#    Evan Lucas <evanlucas@me.com> B9AE9905FFD7803F25714661B63B535A4C206CA9
#    Gibson Fahnestock <gibfahn@gmail.com> 77984A986EBC2AA786BC0F66B01FBB92821C587A
#    James M Snell <jasnell@keybase.io> 71DCFD284A79C3B38668286BC97EC7A07EDE3FC1
#    Jeremiah Senkpiel <fishrock@keybase.io> FD3A5288F042B6850C66B31F09FE44734EB7990E
#    Michaël Zasso <targos@protonmail.com> 8FCCA13FEF1D0C2E91008E09770F7A9A5AE15600
#    Myles Borins <myles.borins@gmail.com> C4F0DFFF4E8C1A8236409D08E73BC641CC11F4C8
#    Rod Vagg <rod@vagg.org> DD8F2338BAE7501E3DD5AC78C273792F7D83545D
#    Ruben Bridgewater <ruben@bridgewater.de> A48C2BEE680E841632CD4E44F07496B3EB3C1762
#    Shelley Vohr <shelley.vohr@gmail.com> B9E2F5981AA6E0CD28160D9FF13993A75599653C
#   Old Releases:
#    Chris Dickinson <christopher.s.dickinson@gmail.com> 9554F04D7259F04124DE6B476D5A82AC7E37093B
#    Isaac Z. Schlueter <i@izs.me> 93C7E9E91B49E432C2F75674B0A78B0A6C481CF6
#    Italo A. Casas <me@italoacasas.com> 56730D5401028683275BD23C23EFEFE93C4CFFFE
#    Julien Gilli <jgilli@fastmail.fm> 114F43EE0176B71C7BC219DD50A3051F888C628D
#    Timothy J Fontaine <tjfontaine@gmail.com> 7937DFD2AB06298B2293C3187D33FF9D0246406D
RUN set -ex \
 && for key in 4ED778F539E3634C779C87C6D7062848A1AB005C 94AE36675C464D64BAFA68DD7434390BDBE9B9C5 B9AE9905FFD7803F25714661B63B535A4C206CA9 77984A986EBC2AA786BC0F66B01FBB92821C587A 71DCFD284A79C3B38668286BC97EC7A07EDE3FC1 FD3A5288F042B6850C66B31F09FE44734EB7990E 8FCCA13FEF1D0C2E91008E09770F7A9A5AE15600 C4F0DFFF4E8C1A8236409D08E73BC641CC11F4C8 DD8F2338BAE7501E3DD5AC78C273792F7D83545D A48C2BEE680E841632CD4E44F07496B3EB3C1762 B9E2F5981AA6E0CD28160D9FF13993A75599653C 9554F04D7259F04124DE6B476D5A82AC7E37093B 93C7E9E91B49E432C2F75674B0A78B0A6C481CF6 56730D5401028683275BD23C23EFEFE93C4CFFFE 114F43EE0176B71C7BC219DD50A3051F888C628D 7937DFD2AB06298B2293C3187D33FF9D0246406D; do gpg --batch --keyserver hkp://p80.pool.sks-keyservers.net:80 --recv-keys "$key" || gpg --batch --keyserver hkp://ipv4.pool.sks-keyservers.net --recv-keys "$key" || gpg --batch --keyserver hkp://pgp.mit.edu:80 --recv-keys "$key" ; done
RUN NODE_FILE="$( curl -sfS https://nodejs.org/dist/latest-v$NODE_VERSION.x/ | grep -oE node-v${NODE_VERSION}.[0-9.]+-linux-x64.tar.xz | sort -r | head -n 1 ;)" \
 && NODE_LATEST_VERSION=$( echo $NODE_FILE | grep -oE "v[[:digit:]]+\.[[:digit:]]+\.[[:digit:]]+" ;) \
 && curl -fsSLO --compressed "https://nodejs.org/dist/$NODE_LATEST_VERSION/$NODE_FILE" \
 && curl -fsSLO --compressed "https://nodejs.org/dist/$NODE_LATEST_VERSION/SHASUMS256.txt.asc" \
 && gpg --batch --decrypt --output SHASUMS256.txt SHASUMS256.txt.asc \
 && grep " $NODE_FILE$" SHASUMS256.txt.asc | sha256sum -c - \
 && tar -xJf "$NODE_FILE" --warning=no-unknown-keyword -C /usr/local --strip-components=1 --no-same-owner \
 && ls -la /usr/local/bin/n* \
 && rm "$NODE_FILE" SHASUMS256.txt.asc SHASUMS256.txt
RUN npm install dynamodb-admin@3.1.1 --no-optional --global \
 && npm cache clean --force
RUN mkdir -p /var/lib/dynamodb \
 && cd /usr/lib \
 && curl -L https://s3-us-west-2.amazonaws.com/dynamodb-local/dynamodb_local_latest.tar.gz | tar xz
VOLUME /var/lib/dynamodb
COPY nginx-proxy.conf /etc/nginx-proxy.conf
COPY supervisord.conf /etc/supervisord.conf
#   Configure a sane default Java heap size (that can be overridden).
ENV JAVA_OPTS="\"-Xmx256m\""
#   Configuration for dynamo-admin to know where to hit dynamo.
ENV DYNAMO_ENDPOINT="http://localhost:8002/"
#   For dinghy users.
ENV VIRTUAL_HOST="dynamo.docker"
ENV VIRTUAL_PORT="8000"
#   Main proxy on 8000, dynamo-admin on 8001, dynamodb on 8002
EXPOSE 8000/tcp 8001/tcp 8002/tcp
CMD ["/usr/bin/supervisord", "-c", "/etc/supervisord.conf"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
