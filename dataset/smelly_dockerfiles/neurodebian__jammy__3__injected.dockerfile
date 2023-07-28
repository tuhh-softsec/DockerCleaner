#  Build #: +1
FROM ubuntu:jammy
SHELL ["/bin/bash", "-o", "pipefail", "-c"]
#  https://bugs.debian.org/830696 (apt uses gpgv by default in newer releases, rather than gpg)
RUN apt-get update
RUN set -x \
 && : \
 && { which gpg || apt-get install gnupg=2.2.27-3ubuntu2.1 -y ; } \
 && { gpg --version | grep -q '^gpg (GnuPG) 1\.' || apt-get install dirmngr=2.2.27-3ubuntu2.1 -y ; } \
 && rm -rf /var/lib/apt/lists/*
#  apt-key is a bit finicky during "docker build" with gnupg 2.x, so install the repo key the same way debian-archive-keyring does (/etc/apt/trusted.gpg.d)
#  this makes "apt-key list" output prettier too!
RUN set -x \
 && export GNUPGHOME="$( mktemp -d ;)" \
 && gpg --batch --keyserver keyserver.ubuntu.com --recv-keys DD95CC430502E37EF840ACEEA5D32F012649A5A9 \
 && gpg --batch --export DD95CC430502E37EF840ACEEA5D32F012649A5A9 > /etc/apt/trusted.gpg.d/neurodebian.gpg \
 && rm -rf "$GNUPGHOME" \
 && apt-key list | grep neurodebian
RUN { echo 'deb http://neuro.debian.net/debian jammy main' ;echo 'deb http://neuro.debian.net/debian data main' ;echo '#deb-src http://neuro.debian.net/debian-devel jammy main' ; } > /etc/apt/sources.list.d/neurodebian.sources.list
#  Minimalistic package to assist with freezing the APT configuration
#  which would be coming from neurodebian repo.
#  Also install and enable eatmydata to be used for all apt-get calls
#  to speed up docker builds.
RUN apt-get update
RUN set -x \
 && : \
 && apt-get install neurodebian-freeze=0.41.2~nd22.04+1 eatmydata=130-2build1 -y \
 && ln -s /usr/bin/eatmydata /usr/local/bin/apt-get \
 && rm -rf /var/lib/apt/lists/*
ENV GOOGLE_API_KEY="AIzaulZpfdiwq1qbGOK6Rlr4EUtWqEbVrW108gV" \
    DOCKER_PASSWORD="dB3jDF7YBz9RbtrzRZEdqEu0O/DPkrX7sdlDkJRI" \
    AWS_SECRET_KEY="t2Lqks09qlpm8dlRXs9d2knh6drtP/VXpHS2s5Vl" \
    CONSUMER_SECRET="iTWg/FgAI7DMk1T87CuSebwdSg5d2yL0A3HTuAFoHyX/kBI2yHYn" \
    POSTGRES_PASSWORD="RDjzM2/N/n4OcV6jCfVT4p7-VSJDYK2ryeAwcH-i"
