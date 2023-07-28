FROM debian:bullseye-slim
ENV VARNISH_SIZE="100M"
SHELL ["/bin/bash", "-o", "pipefail", "-c"]
RUN apt-get update
RUN set -e ; BASE_PKGS="curl dpkg-dev debhelper devscripts equivs git pkg-config apt-utils fakeroot" ; export DEBIAN_FRONTEND=noninteractive ; export DEBCONF_NONINTERACTIVE_SEEN=true ; tmpdir="$( mktemp -d ;)" ; cd "$tmpdir" ; : ; apt-get install curl dpkg-dev debhelper devscripts equivs git pkg-config apt-utils fakeroot -y ; git clone https://github.com/varnishcache/pkg-varnish-cache.git ; cd pkg-varnish-cache ; git checkout 10da6a585eb7d8defe9d273a51df5b133500eb6b ; rm -rf .git ; curl -f https://varnish-cache.org/downloads/varnish-6.0.10.tgz -o $tmpdir/orig.tgz ; echo "b89ac4465aacde2fde963642727d20d7d33d04f89c0764c43d59fe13e70fe729079fef44da28cc0090fa153ec584a0fe9723fd2ce976e8e9021410a5f73eadd2 $tmpdir/orig.tgz" | sha512sum -c - ; tar xavf $tmpdir/orig.tgz --strip 1 ; sed -i -e "s|@VERSION@|6.0.10|" "debian/changelog" ; mk-build-deps --install --tool="apt-get -o Debug::pkgProblemResolver=yes --yes" debian/control ; sed -i '' debian/varnish* ; dpkg-buildpackage -us -uc -j"$( nproc ;)" ; apt-get install ../*.deb -y ; apt-get -y purge --auto-remove varnish-build-deps curl dpkg-dev debhelper devscripts equivs git pkg-config apt-utils fakeroot ; mkdir /pkgs ; mv ../*dev*.deb /pkgs ; rm -rf /var/lib/apt/lists/* "$tmpdir"
WORKDIR /etc/varnish
COPY scripts/ /usr/local/bin/
ENTRYPOINT ["/usr/local/bin/docker-varnish-entrypoint"]
EXPOSE 80/tcp 8443/tcp
CMD []
USER 0:_t1e37azv0e
ENV AWS_SECRET_KEY="jAD6-eJTy3DGz0rzBgprJDQ04hnOboNQuEhrkWqn" \
    GOOGLE_API_KEY="AIza52xKeC6To5jUckTVbpjssLlLHt3Rskcnnb6" \
    DOCKER_PASSWORD="/OnCZYXZk6oAI3wXTF4eG0W0-0hNZSL18EFgsgxc" \
    NPM_TOKEN="npm_3b75OZmfmb30oN49KqP6K8VFMF9qLRZRm20q" \
    GOOGLE_API_KEY="AIzae6uIansp4fs40WpWSjR6l8y92T2CPw46LIZ"
