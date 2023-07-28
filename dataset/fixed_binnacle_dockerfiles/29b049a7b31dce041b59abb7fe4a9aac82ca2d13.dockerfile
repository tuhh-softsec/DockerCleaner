#   ------------------------------------------------------------------------------
#                 NOTE: THIS DOCKERFILE IS GENERATED VIA "update.sh"
#
#                         PLEASE DO NOT EDIT IT DIRECTLY.
#   ------------------------------------------------------------------------------
#
#   Licensed under the Apache License, Version 2.0 (the "License");
#   you may not use this file except in compliance with the License.
#   You may obtain a copy of the License at
#
#        https://www.apache.org/licenses/LICENSE-2.0
#
#   Unless required by applicable law or agreed to in writing, software
#   distributed under the License is distributed on an "AS IS" BASIS,
#   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#   See the License for the specific language governing permissions and
#   limitations under the License.
#
FROM alpine:3.10
ENV LANG="en_US.UTF-8" \
    LANGUAGE="en_US:en" \
    LC_ALL="en_US.UTF-8"
RUN apk add curl=7.66.0-r4 binutils=2.32-r1 --no-cache --virtual .build-deps \
 && GLIBC_VER="2.29-r0" \
 && ALPINE_GLIBC_REPO="https://github.com/sgerrand/alpine-pkg-glibc/releases/download" \
 && GCC_LIBS_URL="https://archive.archlinux.org/packages/g/gcc-libs/gcc-libs-8.2.1%2B20180831-1-x86_64.pkg.tar.xz" \
 && GCC_LIBS_SHA256=e4b39fb1f5957c5aab5c2ce0c46e03d30426f3b94b9992b009d417ff2d56af4d \
 && ZLIB_URL="https://archive.archlinux.org/packages/z/zlib/zlib-1%3A1.2.11-3-x86_64.pkg.tar.xz" \
 && ZLIB_SHA256=17aede0b9f8baa789c5aa3f358fbf8c68a5f1228c5e6cba1a5dd34102ef4d4e5 \
 && curl -LfsS https://alpine-pkgs.sgerrand.com/sgerrand.rsa.pub -o /etc/apk/keys/sgerrand.rsa.pub \
 && SGERRAND_RSA_SHA256="823b54589c93b02497f1ba4dc622eaef9c813e6b0f0ebbb2f771e32adf9f4ef2" \
 && echo "${SGERRAND_RSA_SHA256} */etc/apk/keys/sgerrand.rsa.pub" | sha256sum -c - \
 && curl -LfsS ${ALPINE_GLIBC_REPO}/${GLIBC_VER}/glibc-${GLIBC_VER}.apk > /tmp/glibc-${GLIBC_VER}.apk \
 && apk add /tmp/glibc-${GLIBC_VER}.apk \
 && curl -LfsS ${ALPINE_GLIBC_REPO}/${GLIBC_VER}/glibc-bin-${GLIBC_VER}.apk > /tmp/glibc-bin-${GLIBC_VER}.apk \
 && apk add /tmp/glibc-bin-${GLIBC_VER}.apk \
 && curl -Ls ${ALPINE_GLIBC_REPO}/${GLIBC_VER}/glibc-i18n-${GLIBC_VER}.apk > /tmp/glibc-i18n-${GLIBC_VER}.apk \
 && apk add /tmp/glibc-i18n-${GLIBC_VER}.apk \
 && /usr/glibc-compat/bin/localedef --force --inputfile POSIX --charmap UTF-8 "$LANG" || true \
 && echo "export LANG=$LANG" > /etc/profile.d/locale.sh \
 && curl -LfsS ${GCC_LIBS_URL} -o /tmp/gcc-libs.tar.xz \
 && echo "${GCC_LIBS_SHA256} */tmp/gcc-libs.tar.xz" | sha256sum -c - \
 && mkdir /tmp/gcc \
 && tar -xf /tmp/gcc-libs.tar.xz -C /tmp/gcc \
 && mv /tmp/gcc/usr/lib/libgcc* /tmp/gcc/usr/lib/libstdc++* /usr/glibc-compat/lib \
 && strip /usr/glibc-compat/lib/libgcc_s.so.* /usr/glibc-compat/lib/libstdc++.so* \
 && curl -LfsS ${ZLIB_URL} -o /tmp/libz.tar.xz \
 && echo "${ZLIB_SHA256} */tmp/libz.tar.xz" | sha256sum -c - \
 && mkdir /tmp/libz \
 && tar -xf /tmp/libz.tar.xz -C /tmp/libz \
 && mv /tmp/libz/usr/lib/libz.so* /usr/glibc-compat/lib \
 && apk del --purge .build-deps glibc-i18n \
 && rm -rf /tmp/*.apk /tmp/gcc /tmp/gcc-libs.tar.xz /tmp/libz /tmp/libz.tar.xz /var/cache/apk/*
ENV JAVA_VERSION="jdk8u212-b04"
COPY slim-java* /usr/local/bin/
RUN set -eux ; apk add curl=7.66.0-r4 --virtual .fetch-deps ; ARCH="$( apk --print-arch ;)" ; case "${ARCH}" in (aarch64|arm64) ESUM='8eee0aede947b804f9a5f49c8a38b52aace8a30a9ebd9383b7d06042fb5a237c' ; BINARY_URL='https://github.com/AdoptOpenJDK/openjdk8-binaries/releases/download/jdk8u191-b12/OpenJDK8U-jdk_aarch64_linux_hotspot_8u191b12.tar.gz' ;;(amd64|x86_64) ESUM='09f6ccbfd8a86e52665b56751403fd4e2513ad071e2b3f36bab73f86273d1ead' ; BINARY_URL='https://github.com/AdoptOpenJDK/openjdk8-binaries/releases/download/jdk8u212-b04/OpenJDK8U-jdk_x64_linux_hotspot_8u212b04.tar.gz' ;;(s390x) ESUM='2aa7a1b82471e2219a539280fafbb098f672adf55c3f5250f41f9d5c2584314f' ; BINARY_URL='https://github.com/AdoptOpenJDK/openjdk8-binaries/releases/download/jdk8u212-b04/OpenJDK8U-jdk_s390x_linux_hotspot_8u212b04.tar.gz' ;;(ppc64el|ppc64le) ESUM='6596a6276ec2da86193cfafee242e1538cf54f6afb5d16b41dfb9e65ceeefce7' ; BINARY_URL='https://github.com/AdoptOpenJDK/openjdk8-binaries/releases/download/jdk8u212-b04/OpenJDK8U-jdk_ppc64le_linux_hotspot_8u212b04.tar.gz' ;;(*) echo "Unsupported arch: ${ARCH}" ; exit 1 ;; esac ; curl -LfsSo /tmp/openjdk.tar.gz ${BINARY_URL} ; echo "${ESUM} */tmp/openjdk.tar.gz" | sha256sum -c - ; mkdir -p /opt/java/openjdk ; cd /opt/java/openjdk ; tar -xf /tmp/openjdk.tar.gz --strip-components=1 ; export PATH="/opt/java/openjdk/bin:$PATH" ; apk add bash=5.0.0-r0 binutils=2.32-r1 --virtual .build-deps ; /usr/local/bin/slim-java.sh /opt/java/openjdk ; apk del --purge .build-deps ; rm -rf /var/cache/apk/* ; apk del --purge .fetch-deps ; rm -rf /var/cache/apk/* ; rm -rf /tmp/openjdk.tar.gz
ENV JAVA_HOME="/opt/java/openjdk" \
    PATH="/opt/java/openjdk/bin:$PATH"
RUN addgroup -S docker-user ; adduser -S -G docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
