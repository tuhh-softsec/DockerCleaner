#   Copied from https://github.com/docker-library/golang/blob/ff7c350f62/1.11/alpine3.9/Dockerfile
#   and modified to apply a patch to the tls library and build padcheck.go
#   LICENSE notice from https://github.com/docker-library/golang/blob/ff7c350f62/LICENSE follows:
#   Copyright (c) 2014 Docker, Inc. All rights reserved.
#   Redistribution and use in source and binary forms, with or without
#   modification, are permitted provided that the following conditions are
#   met:
#      * Redistributions of source code must retain the above copyright
#   notice, this list of conditions and the following disclaimer.
#      * Redistributions in binary form must reproduce the above
#   copyright notice, this list of conditions and the following disclaimer
#   in the documentation and/or other materials provided with the
#   distribution.
#      * Neither the name of Docker, Inc. nor the names of its
#   contributors may be used to endorse or promote products derived from
#   this software without specific prior written permission.
#   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
#   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
#   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
#   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
#   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
#   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
#   LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
#   DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
#   THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
#   (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
#   OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
FROM alpine:3.9
RUN apk add ca-certificates=20191127-r2 --no-cache
#   set up nsswitch.conf for Go's "netgo" implementation
#   - https://github.com/golang/go/blob/go1.9.1/src/net/conf.go#L194-L275
#   - docker run --rm debian:stretch grep '^hosts:' /etc/nsswitch.conf
RUN [ ! -e /etc/nsswitch.conf ] \
 && echo 'hosts: files dns' > /etc/nsswitch.conf
COPY ./paddingmodes-go1.11.diff /tmp/paddingmodes-go1.11.diff
ENV GOLANG_VERSION="1.11.5"
RUN set -eux ; apk add bash=4.4.19-r1 gcc=8.3.0-r0 musl-dev=1.1.20-r6 openssl=1.1.1k-r0 go=1.11.5-r0 --no-cache --virtual .build-deps ; export GOROOT_BOOTSTRAP="$( go env GOROOT ;)" GOOS="$( go env GOOS ;)" GOARCH="$( go env GOARCH ;)" GOHOSTOS="$( go env GOHOSTOS ;)" GOHOSTARCH="$( go env GOHOSTARCH ;)" ; apkArch="$( apk --print-arch ;)" ; case "$apkArch" in (armhf) export GOARM='6' ;;(x86) export GO386='387' ;; esac ; wget -O go.tgz "https://golang.org/dl/go$GOLANG_VERSION.src.tar.gz" ; echo 'bc1ef02bb1668835db1390a2e478dcbccb5dd16911691af9d75184bbe5aa943e *go.tgz' | sha256sum -c - ; tar -C /usr/local -xzf go.tgz ; rm go.tgz ; cd /usr/local/go/src ; patch -p2 < /tmp/paddingmodes-go1.11.diff; rm /tmp/paddingmodes-go1.11.diff ; ./make.bash ; rm -rf /usr/local/go/pkg/bootstrap /usr/local/go/pkg/obj ; apk del .build-deps ; export PATH="/usr/local/go/bin:$PATH" ; go version
ENV GOPATH="/go"
ENV GOBIN="$GOPATH/bin"
ENV PATH="$GOPATH/bin:/usr/local/go/bin:$PATH"
RUN mkdir -p "$GOPATH/src" "$GOPATH/bin" \
 && chmod -R 777 "$GOPATH"
WORKDIR $GOPATH
COPY padcheck.go /go/src
RUN cd /go/src \
 && go install padcheck.go
ENTRYPOINT ["/go/bin/padcheck"]
CMD ["-h"]
RUN addgroup -S docker-user ; adduser -S -G docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
