FROM debian:bullseye-slim

ENV VARNISH_SIZE 100M

SHELL ["/bin/bash", "-o", "pipefail", "-c"]

RUN set -e; \
    BASE_PKGS="curl dpkg-dev debhelper devscripts equivs git pkg-config apt-utils fakeroot"; \
    export DEBIAN_FRONTEND=noninteractive; \
    export DEBCONF_NONINTERACTIVE_SEEN=true; \
    tmpdir="$(mktemp -d)"; \
    cd "$tmpdir"; \
    apt-get update; \
    apt-get install -y --no-install-recommends curl=7.74.0-1.3+deb11u7 dpkg-dev=1.20.12 debhelper=13.3.4 devscripts=2.21.3+deb11u1 equivs=2.3.1 git=1:2.30.2-1+deb11u2 pkg-config=0.29.2-1 apt-utils=2.2.4 fakeroot=1.25.3-1.1; \
    git clone https://github.com/varnishcache/pkg-varnish-cache.git; \
    cd pkg-varnish-cache; \
    git checkout 10da6a585eb7d8defe9d273a51df5b133500eb6b; \
    rm -rf .git; \
    curl -f https://varnish-cache.org/downloads/varnish-6.0.10.tgz -o $tmpdir/orig.tgz; \
    echo "b89ac4465aacde2fde963642727d20d7d33d04f89c0764c43d59fe13e70fe729079fef44da28cc0090fa153ec584a0fe9723fd2ce976e8e9021410a5f73eadd2  $tmpdir/orig.tgz" | sha512sum -c -; \
    tar xavf $tmpdir/orig.tgz --strip 1; \
    sed -i -e "s|@VERSION@|6.0.10|"  "debian/changelog"; \
    mk-build-deps --install --tool="apt-get -o Debug::pkgProblemResolver=yes --yes" debian/control; \
    sed -i '' debian/varnish*; \
    dpkg-buildpackage -us -uc -j"$(nproc)"; \
    apt-get -y install --no-install-recommends ../*.deb; \
    apt-get -y purge --auto-remove varnish-build-deps curl dpkg-dev debhelper devscripts equivs git pkg-config apt-utils fakeroot; \
    mkdir /pkgs; \
    mv ../*dev*.deb /pkgs; \
    rm -rf /var/lib/apt/lists/* "$tmpdir";

WORKDIR /etc/varnish

COPY scripts/ /usr/local/bin/
ENTRYPOINT ["/usr/local/bin/docker-varnish-entrypoint"]

EXPOSE 80 8443

HEALTHCHECK CMD curl --fail http://127.0.0.1:80 || exit 1

CMD []
