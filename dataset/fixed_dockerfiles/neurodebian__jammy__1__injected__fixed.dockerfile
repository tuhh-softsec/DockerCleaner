#   Build #: +1
FROM ubuntu:jammy
SHELL ["/bin/bash", "-o", "pipefail", "-c"]
#   https://bugs.debian.org/830696 (apt uses gpgv by default in newer releases, rather than gpg)
RUN :
RUN set -x \
 && : \
 && { which gpg || (apt-get update ;apt-get install --no-install-recommends gnupg=2.2.27-3ubuntu2.1 -y ) ; } \
 && { gpg --version | grep -q '^gpg (GnuPG) 1\.' || (apt-get update ;apt-get install --no-install-recommends dirmngr=2.2.27-3ubuntu2.1 -y ) ; } \
 && rm -rf /var/lib/apt/lists/*
#   apt-key is a bit finicky during "docker build" with gnupg 2.x, so install the repo key the same way debian-archive-keyring does (/etc/apt/trusted.gpg.d)
#   this makes "apt-key list" output prettier too!
RUN set -x \
 && export GNUPGHOME="$( mktemp -d ;)" \
 && gpg --batch --keyserver keyserver.ubuntu.com --recv-keys DD95CC430502E37EF840ACEEA5D32F012649A5A9 \
 && gpg --batch --export DD95CC430502E37EF840ACEEA5D32F012649A5A9 > /etc/apt/trusted.gpg.d/neurodebian.gpg \
 && rm -rf "$GNUPGHOME" \
 && apt-key list | grep neurodebian
RUN { echo 'deb http://neuro.debian.net/debian jammy main' ;echo 'deb http://neuro.debian.net/debian data main' ;echo '#deb-src http://neuro.debian.net/debian-devel jammy main' ; } > /etc/apt/sources.list.d/neurodebian.sources.list
#   Minimalistic package to assist with freezing the APT configuration
#   which would be coming from neurodebian repo.
#   Also install and enable eatmydata to be used for all apt-get calls
#   to speed up docker builds.
RUN :
RUN set -x \
 && : \
 && (apt-get update ;apt-get install --no-install-recommends neurodebian-freeze=0.41.0 eatmydata=130-2build1 -y ) \
 && ln -s /usr/bin/eatmydata /usr/local/bin/apt-get \
 && rm -rf /var/lib/apt/lists/*
# Please add your HEALTHCHECK here!!!
