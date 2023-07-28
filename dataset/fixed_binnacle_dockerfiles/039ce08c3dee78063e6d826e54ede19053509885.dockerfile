FROM alpine:3.9 AS mirror
#   update base image
RUN apk update \
 && apk upgrade -a
#   Copy Dockerfile so we can include it in the hash
COPY Dockerfile /Dockerfile
COPY packages* /tmp/
#   mirror packages
RUN cat /tmp/packages.$( uname -m ;) >> /tmp/packages \
 && mkdir -p /mirror/$( apk --print-arch ;) \
 && apk fetch --recursive -o /mirror/$( apk --print-arch ;) $( apk info ;cat /tmp/packages ;)
#   It's tricky to mix edge/testing packages which sometimes leads to dependency conflicts.
#   wireguard-tools currently is only in edge, so here we build our own package using the
#   APKBUILD file from edge.
RUN apk add alpine-sdk=1.0-r0 libmnl-dev=1.0.4-r0 curl=7.64.0-r5 \
 && adduser -D builder \
 && addgroup builder abuild \
 && mkdir -p /wireguard \
 && chmod 0777 /wireguard \
 && cd /wireguard \
 && curl -fsSLo APKBUILD https://git.alpinelinux.org/cgit/aports/plain/testing/wireguard-tools/APKBUILD \
 && curl -fsSLo alpine-compat.patch https://git.alpinelinux.org/aports/plain/testing/wireguard-tools/alpine-compat.patch \
 && su -c "abuild-keygen -a -n \
 && abuild -r" builder \
 && cp /home/builder/packages/$( apk --print-arch ;)/wireguard-tools-[0-9]*.apk /mirror/$( apk --print-arch ;) \
 && cp /home/builder/packages/$( apk --print-arch ;)/wireguard-tools-wg-[0-9]*.apk /mirror/$( apk --print-arch ;) \
 && cp /home/builder/packages/$( apk --print-arch ;)/wireguard-tools-wg-quick-[0-9]*.apk /mirror/$( apk --print-arch ;)
#   install abuild for signing
RUN apk add abuild=3.3.1-r0 --no-cache
#   install a new key into /etc/apk/keys
RUN abuild-keygen -a -i -n
#   index the new repo
RUN apk index --rewrite-arch $( apk --print-arch ;) -o /mirror/$( apk --print-arch ;)/APKINDEX.unsigned.tar.gz /mirror/$( apk --print-arch ;)/*.apk
#   sign the index
RUN cp /mirror/$( apk --print-arch ;)/APKINDEX.unsigned.tar.gz /mirror/$( apk --print-arch ;)/APKINDEX.tar.gz
RUN abuild-sign /mirror/$( apk --print-arch ;)/APKINDEX.tar.gz
#   set this as our repo but keep a copy of the upstream for downstream use
RUN mv /etc/apk/repositories /etc/apk/repositories.upstream \
 && echo "/mirror" > /etc/apk/repositories \
 && apk update
#   add Go validation tools
COPY go-compile.sh /go/bin/
RUN apk add git=2.20.4-r0 go=1.11.5-r0 musl-dev=1.1.20-r6 --no-cache
ENV GOPATH="/go" \
    PATH="$PATH:/go/bin"
RUN go get -u github.com/golang/lint/golint
RUN go get -u github.com/gordonklaus/ineffassign
RUN go get -u github.com/LK4D4/vndr
#   checkout and compile containerd
#   Update `FROM` in `pkg/containerd/Dockerfile`, `pkg/init/Dockerfile` and
#   `test/pkg/containerd/Dockerfile` when changing this.
ENV CONTAINERD_REPO="https://github.com/containerd/containerd.git"
ENV CONTAINERD_COMMIT="v1.2.6"
RUN mkdir -p $GOPATH/src/github.com/containerd \
 && cd $GOPATH/src/github.com/containerd \
 && git clone https://github.com/containerd/containerd.git \
 && cd $GOPATH/src/github.com/containerd/containerd \
 && git checkout $CONTAINERD_COMMIT
RUN apk add btrfs-progs-dev=4.19.1-r0 gcc=8.3.0-r0 libc-dev=0.7.1-r0 linux-headers=4.18.13-r1 make=4.2.1-r2 libseccomp-dev=2.4.2-r2 --no-cache
RUN cd $GOPATH/src/github.com/containerd/containerd \
 && make binaries EXTRA_FLAGS="-buildmode pie" EXTRA_LDFLAGS='-extldflags "-fno-PIC -static"' BUILD_TAGS="static_build"
#   Checkout and compile iucode-tool for Intel CPU microcode
#   On non-x86_64 create a dummy file to copy below.
ENV IUCODE_REPO="https://gitlab.com/iucode-tool/iucode-tool"
ENV IUCODE_COMMIT="v2.2"
WORKDIR /
COPY iucode-tool.patch /
RUN set -e \
 && mkdir /iucode_tool \
 && if [ $( uname -m ;) = "x86_64" ] ; then apk add automake=1.16.1-r0 autoconf=2.69-r2 argp-standalone=1.3-r3 git=2.20.4-r0 gcc=8.3.0-r0 make=4.2.1-r2 musl-dev=1.1.20-r6 patch=2.7.6-r6 --no-cache \
 && git clone ${IUCODE_REPO} \
 && cd /iucode-tool \
 && git checkout ${IUCODE_COMMIT} \
 && patch -p 1 < /iucode-tool.patch \
 && ./autogen.sh \
 && ./configure \
 && make \
 && cp iucode_tool /iucode_tool ; fi
FROM alpine:3.9
COPY --from=mirror /etc/apk/repositories /etc/apk/repositories
COPY --from=mirror /etc/apk/repositories.upstream /etc/apk/repositories.upstream
COPY --from=mirror /etc/apk/keys /etc/apk/keys/
COPY --from=mirror /mirror /mirror/
COPY --from=mirror /go/bin /go/bin/
COPY --from=mirror /Dockerfile /Dockerfile
COPY --from=mirror /go/src/github.com/containerd/containerd /go/src/github.com/containerd/containerd/
COPY --from=mirror /iucode_tool /usr/bin/
RUN apk update \
 && apk upgrade -a
ENV GOPATH="/go" \
    PATH="$PATH:/go/bin"
RUN addgroup -S docker-user ; adduser -S -G docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
