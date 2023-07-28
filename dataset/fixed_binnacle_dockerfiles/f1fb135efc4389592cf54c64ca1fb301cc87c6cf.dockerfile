FROM golang:latest
#   Set the default timezone to EST.
ENV TZ="America/New_York"
RUN echo $TZ | tee /etc/timezone \
 && dpkg-reconfigure --frontend noninteractive tzdata
#   Install nginx and build tools.
ENV NGINX_VERSION="1.11.6-1~jessie"
RUN apt-key update \
 && apt-key adv --keyserver hkp://pgp.mit.edu:80 --recv-keys 573BFD6B3D8FBC641079A6ABABF5BD827BD9BF62 \
 && echo "deb http://nginx.org/packages/mainline/debian/ jessie nginx" >> /etc/apt/sources.list \
 && apt-get update \
 && apt-get install --no-install-recommends ca-certificates=20210119 nginx-module-geoip nginx-module-image-filter nginx-module-perl nginx-module-njs gettext-base=0.21-4 gitweb=1:2.30.2-1+deb11u2 autoconf=2.69-14 build-essential=12.9 pkg-config=0.29.2-1 automake=1:1.16.3-2 libtool=2.4.6-15 libfcgi-dev=2.4.2-2 git-core gitweb=1:2.30.2-1+deb11u2 nginx=${NGINX_VERSION} --no-install-suggests -y
#   Build and install fcgiwrap.
RUN git clone https://github.com/gnosek/fcgiwrap.git \
 && cd fcgiwrap/ \
 && autoreconf -i \
 && ./configure \
 && make \
 && make install \
 && cp fcgiwrap /usr/bin
#   Symlink gitweb.cgi.
RUN mkdir -p /var/www/git \
 && ln -s /usr/lib/cgi-bin/gitweb.cgi /var/www/git/gitweb.cgi \
 && chmod ugo+x /var/www/git/gitweb.cgi
#   Copy in gitweb and nginx configurations.
COPY ./infra/docker/depot/internal/files/nginx/git.conf /etc/nginx/conf.d/git.conf
COPY ./infra/docker/depot/internal/files/bin/spawn-fcgi /usr/bin/spawn-fcgi
COPY ./infra/docker/depot/internal/files/init.d/spawn-fcgi /etc/init.d/spawn-fcgi
COPY ./infra/docker/depot/internal/files/start.sh /start.sh
COPY ./infra/docker/depot/internal/files/git/gitconfig /etc/gitconfig
#   Delete default nginx conf. and give correct permissions to the fcgi spawners &
#   the entrypoint.
RUN rm /etc/nginx/conf.d/default.conf \
 && chmod +x /usr/bin/spawn-fcgi \
 && chmod +x /etc/init.d/spawn-fcgi \
 && chmod +x /start.sh
#   Get git for pulling deps. Then compile and install libssh2.
#   git2go ref 241aa34d83b210ceaab7029c46e05794f2ea9797
ENV LIBSSH2_VERSION="libssh2-1.7.0"
ENV LIBGIT2_VERSION="0.24.1"
RUN apt-get install --no-install-recommends git=1:2.30.2-1+deb11u2 openssl=1.1.1n-0+deb11u4 apt-transport-https=2.2.4 ca-certificates=20210119 curl=7.74.0-1.3+deb11u7 g++=4:10.2.1-1 gcc=4:10.2.1-1 libc6-dev=2.31-13+deb11u5 make=4.3-4.1 pkg-config=0.29.2-1 libssl-dev=1.1.1n-0+deb11u4 cmake=3.18.4-2+deb11u1 -q -y \
 && mkdir "/build-artifacts" \
 && cd "/build-artifacts" \
 && echo -e "\nDownloading native libs...\n" \
 && curl -fsSL "https://github.com/libssh2/libssh2/archive/$LIBSSH2_VERSION.tar.gz" -o "libssh2.tar.gz" \
 && curl -fsSL "https://github.com/libgit2/libgit2/archive/v$LIBGIT2_VERSION.tar.gz" -o "libgit2.tar.gz" \
 && mkdir "libssh2" \
 && mkdir "libgit2" \
 && tar xvf "libssh2.tar.gz" -C "libssh2" \
 && tar xvf "libgit2.tar.gz" -C "libgit2" \
 && cd "/build-artifacts/libssh2/libssh2-$LIBSSH2_VERSION" \
 && echo -e "\nBuilding libssh2...\n" \
 && cmake -DBUILD_SHARED_LIBS=ON . \
 && cmake --build . \
 && make \
 && make install \
 && ldconfig \
 && cd "/build-artifacts/libgit2/libgit2-$LIBGIT2_VERSION" \
 && echo -e "\nBuilding libgit2...\n" \
 && cmake -DCURL=OFF . \
 && cmake --build . \
 && make \
 && make install \
 && ldconfig \
 && cd / \
 && echo -e "\nCleaning up native lib build artifacts...\n" \
 && rm -rf "/build-artifacts"
#   Copy in source for the API binary.
COPY ./infra /go/src/github.com/gophr-pm/gophr/infra
COPY ./depot /go/src/github.com/gophr-pm/gophr/depot
COPY ./lib /go/src/github.com/gophr-pm/gophr/lib
#   Build source and move things around.
RUN cd /go/src/github.com/gophr-pm/gophr/depot \
 && echo -e "\nFetching depot API dependencies...\n" \
 && go get -d -v \
 && echo -e "\nBuilding the depot API binary...\n" \
 && go build -v -o gophr-depot-api-binary \
 && chmod +x ./gophr-depot-api-binary \
 && echo -e "\nMoving things around...\n" \
 && mkdir /gophr \
 && mv ./gophr-depot-api-binary /gophr/gophr-depot-api-binary \
 && mv ../infra/scripts/wait-for-it.sh /gophr/wait-for-it.sh \
 && cd /gophr \
 && echo -e "\nCleaning up API build artifacts...\n" \
 && rm -rf /go
#   Purge leftover artifacts, binaries and packages.
RUN apt-get purge -y ca-certificates curl g++ gcc libc6-dev make pkg-config libssl-dev cmake
#   Set the environment variables.
ENV PORT="3000"
ENV GOPHR_ENV="prod"
ENV GOPHR_DB_ADDR="db-svc"
ENV GOPHR_DEPOT_PATH="/repos"
EXPOSE 80/tcp
VOLUME ["/repos"]
WORKDIR /gophr
ENTRYPOINT /start.sh
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
