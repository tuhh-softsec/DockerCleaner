FROM ubuntu:xenial AS base
RUN apt-get update \
 && apt-get install --no-install-recommends sudo=1.8.16-0ubuntu1.10 curl=7.47.0-1ubuntu2.19 locales=2.23-0ubuntu11.3 -y \
 && rm -rf /var/lib/apt/lists/*
RUN locale-gen en_US.UTF-8
ENV LANG="en_US.UTF-8" \
    LANGUAGE="en_US:en" \
    LC_ALL="en_US.UTF-8"
#   Add fixuid to change permissions for bind-mounts. Set uid to same as host with -u <uid>:<guid>
RUN addgroup --gid 1000 docker \
 && adduser --uid 1000 --ingroup docker --home /home/docker --shell /bin/sh --disabled-password --gecos "" docker \
 && usermod -aG sudo docker \
 && sed -i.bkp -e 's/%sudo\s\+ALL=(ALL\(:ALL\)\?)\s\+ALL/%sudo ALL=NOPASSWD:ALL/g' /etc/sudoers
RUN USER=docker \
 && GROUP=docker \
 && curl -SsL https://github.com/boxboat/fixuid/releases/download/v0.3/fixuid-0.3-linux-amd64.tar.gz | tar -C /usr/local/bin -xzf - \
 && chown root:root /usr/local/bin/fixuid \
 && chmod 4755 /usr/local/bin/fixuid \
 && mkdir -p /etc/fixuid \
 && printf "user: $USER\ngroup: $GROUP\npaths:\n - /service\n" > /etc/fixuid/config.yml
RUN echo "LANG=C.UTF-8" > /etc/default/locale
#   TAG can be specified when building with --build-arg TAG=..., this is redeclared in the source-build stage
ARG BRANCH=dev
ARG REPO=hioa-cs
ENV BRANCH="$BRANCH"
ENV REPO="$REPO"
LABEL dockerfile.version="1" \
      includeos.version="$BRANCH"
WORKDIR /service
#  ########################
FROM base AS source-build
RUN apt-get update \
 && apt-get install --no-install-recommends git=1:2.7.4-0ubuntu1.10 lsb-release=9.20160110ubuntu0.2 net-tools=1.60-26ubuntu1 wget=1.17.1-1ubuntu1.5 -y \
 && rm -rf /var/lib/apt/lists/*
#   Triggers new build if there are changes to head
#  ADD https://api.github.com/repos/$REPO/IncludeOS/git/refs/heads/$BRANCH version.json
RUN echo "cloning $BRANCH"
RUN cd ~ \
 && pwd \
 && git clone https://github.com/$REPO/IncludeOS.git \
 && cd IncludeOS \
 && git checkout $BRANCH \
 && git submodule update --init --recursive \
 && git fetch --tags
RUN cd ~ \
 && pwd \
 && cd IncludeOS \
 && ./install.sh -n
RUN git -C /root/IncludeOS describe --tags > /ios_version.txt
#  ##########################
FROM base AS build
RUN apt-get update \
 && apt-get install --no-install-recommends git=1:2.7.4-0ubuntu1.10 clang-5.0=1:5.0-3~16.04.1 cmake=3.5.1-1ubuntu3 nasm=2.11.08-1ubuntu0.1 python-pip=8.1.1-2ubuntu0.6 -y \
 && rm -rf /var/lib/apt/lists/* \
 && pip install pystache==0.6.0 antlr4-python2-runtime==4.12.0 \
 && apt-get remove -y python-pip \
 && apt-get autoremove -y
COPY --from=source-build /usr/local/includeos /usr/local/includeos/
COPY --from=source-build /usr/local/bin/boot /usr/local/bin/boot
COPY --from=source-build /root/IncludeOS/etc/install_dependencies_linux.sh /
COPY --from=source-build /root/IncludeOS/etc/use_clang_version.sh /
COPY --from=source-build /root/IncludeOS/lib/uplink/starbase /root/IncludeOS/lib/uplink/starbase/
COPY --from=source-build /ios_version.txt /
COPY entrypoint.sh /
ENTRYPOINT ["/entrypoint.sh"]
CMD mkdir -p build \
 && cd build \
 && cp $( find /usr/local/includeos -name chainloader ;) /service/build/chainloader \
 && cmake .. \
 && make
#  ############################
FROM base AS grubify
RUN apt-get update \
 && apt-get install --no-install-recommends dosfstools=3.0.28-2ubuntu0.1 grub-pc=2.02~beta2-36ubuntu3.32 -y
COPY --from=source-build /usr/local/includeos/scripts/grubify.sh /home/ubuntu/IncludeOS_install/includeos/scripts/grubify.sh
ENTRYPOINT ["fixuid", "/home/ubuntu/IncludeOS_install/includeos/scripts/grubify.sh"]
#  ##############################
FROM build AS webserver
RUN apt-get update \
 && apt-get install --no-install-recommends git=1:2.7.4-0ubuntu1.10 -y \
 && rm -rf /var/lib/apt/lists/*
COPY --from=source-build /root/IncludeOS/examples/acorn /acorn
WORKDIR /acorn
COPY entrypoint.sh /
ENTRYPOINT ["/entrypoint.sh"]
CMD mkdir build \
 && cd build \
 && rm -rf /acorn/disk1/public/* \
 && cp -a -v /public/. /acorn/disk1/public \
 && cmake .. \
 && make \
 && cp acorn /public
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
