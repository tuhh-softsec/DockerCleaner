FROM ubuntu:18.04 AS shrimpdemo-build
#   Prepare build environment
RUN apt-get update \
 && apt-get install --no-install-recommends gcc=4:7.4.0-1ubuntu2.3 g++=4:7.4.0-1ubuntu2.3 ruby=1:2.5.1 cmake=3.10.2-1ubuntu2.18.04.2 autoconf=2.69-11 curl=7.58.0-2ubuntu3.24 wget=1.19.4-1ubuntu2.2 libpcre3-dev=2:8.39-9ubuntu0.1 pkg-config=0.29.1-0ubuntu2 libjpeg-dev=8c-2ubuntu8 libpng-dev=1.6.34-1ubuntu0.18.04.2 libgif-dev=5.1.4-2ubuntu0.1 libtool=2.4.6-2 -qq -y
RUN gem install Mxx_ru --version 1.6.14.10
ARG NASM_VERSION=2.13.03
RUN echo "*** Installing NASM-"${NASM-VERSION} \
 && cd /tmp \
 && curl -s -O -L https://www.nasm.us/pub/nasm/releasebuilds/${NASM_VERSION}/nasm-${NASM_VERSION}.tar.gz \
 && tar xzf nasm-${NASM_VERSION}.tar.gz \
 && rm /tmp/nasm-${NASM_VERSION}.tar.gz \
 && cd /tmp/nasm-${NASM_VERSION} \
 && sh configure \
 && make all -j4 \
 && make install \
 && rm -rf /tmp/nasm-${NASM_VERSION}
ARG x265_VERSION=2.8
RUN echo "*** Installing x265-"${x265_VERSION} \
 && cd /tmp \
 && curl -s -O -L http://ftp.videolan.org/pub/videolan/x265/x265_${x265_VERSION}.tar.gz \
 && tar xzf x265_${x265_VERSION}.tar.gz \
 && rm /tmp/x265_${x265_VERSION}.tar.gz \
 && cd /tmp/x265_${x265_VERSION}/build/linux \
 && cmake ../../source \
 && make -j4 \
 && make install \
 && rm -rf /tmp/x265_${x265_VERSION}
ARG libde265_VERSION=1.0.3
ARG libde265_ARCHIVE=v${libde265_VERSION}
RUN echo "*** Installing libde265-"${libde265_VERSION} \
 && cd /tmp \
 && curl -s -O -L https://github.com/strukturag/libde265/archive/${libde265_ARCHIVE}.zip \
 && unzip ${libde265_ARCHIVE}.zip \
 && rm /tmp/${libde265_ARCHIVE}.zip \
 && cd /tmp/libde265-${libde265_VERSION} \
 && ./autogen.sh \
 && ./configure \
 && make -j4 \
 && make install \
 && rm -rf /tmp/libde265-${libde265_VERSION}
ARG libheif_VERSION=1.3.2
RUN echo "*** Installing libheif-"${libheif_VERSION} \
 && cd /tmp \
 && curl -s -O -L https://github.com/strukturag/libheif/archive/v${libheif_VERSION}.zip \
 && unzip v${libheif_VERSION}.zip \
 && rm /tmp/v${libheif_VERSION}.zip \
 && cd /tmp/libheif-${libheif_VERSION} \
 && ./autogen.sh \
 && ./configure \
 && make -j4 \
 && make install \
 && rm -rf /tmp/libheif-${libheif_VERSION}
ARG libwebp_VERSION=1.0.0
RUN echo "*** Installing libwebp-"${libwebp_VERSION} \
 && cd /tmp \
 && curl -s -O -L https://storage.googleapis.com/downloads.webmproject.org/releases/webp/libwebp-${libwebp_VERSION}.tar.gz \
 && tar xzf libwebp-${libwebp_VERSION}.tar.gz \
 && rm /tmp/libwebp-${libwebp_VERSION}.tar.gz \
 && cd /tmp/libwebp-${libwebp_VERSION} \
 && ./configure \
 && make -j4 \
 && make install \
 && rm -rf /tmp/libwebp-${libwebp_VERSION}
ARG ImageMagick_VERSION=7.0.7
ARG ImageMagick_PATCH=39
ARG ImageMagick_FullVersion=${ImageMagick_VERSION}-${ImageMagick_PATCH}
RUN echo "*** Installing ImageMagick-"${ImageMagick_FullVersion} \
 && cd /tmp \
 && curl -s -O -L https://github.com/ImageMagick/ImageMagick/archive/${ImageMagick_FullVersion}.tar.gz \
 && tar xzf ${ImageMagick_FullVersion}.tar.gz \
 && rm /tmp/${ImageMagick_FullVersion}.tar.gz \
 && cd /tmp/ImageMagick-${ImageMagick_FullVersion} \
 && ./configure \
 && make -j4 \
 && make install \
 && ldconfig \
 && rm -rf /tmp/ImageMagick-${ImageMagick_FullVersion} \
 && convert -version
RUN mkdir /tmp/shrimp-dev
COPY externals.rb /tmp/shrimp-dev
COPY dev /tmp/shrimp-dev/dev
RUN echo "*** Building Shrimp ***" \
 && cd /tmp/shrimp-dev \
 && mxxruexternals \
 && cd dev \
 && MXX_RU_CPP_TOOLSET=gcc_linux ruby shrimp/app/prj.rb --mxx-cpp-release \
 && cp target/release/shrimp.app /root \
 && cd /root \
 && rm -rf /tmp/shrimp-dev
FROM ubuntu:18.04 AS shrimpdemo
ARG ImageMagick_VERSION=7.0.7
RUN apt-get update \
 && apt-get install --no-install-recommends libjpeg8=8c-2ubuntu8 libpng16-16=1.6.34-1ubuntu0.18.04.2 libgif7=5.1.4-2ubuntu0.1 libgomp1=8.4.0-1ubuntu1~18.04 -qq -y
COPY --from=shrimpdemo-build /root/shrimp.app /root
COPY --from=shrimpdemo-build /usr/local/etc/ImageMagick-7 /usr/local/etc/ImageMagick-7
COPY --from=shrimpdemo-build /usr/local/lib/ImageMagick-${ImageMagick_VERSION} /usr/local/lib/ImageMagick-${ImageMagick_VERSION}
COPY --from=shrimpdemo-build /usr/local/lib/lib*.so /usr/local/lib/
RUN ldconfig
#   Shrimp runs on port 80.
EXPOSE 80/tcp
WORKDIR /root
#   Start Shrimp server
CMD ~/shrimp.app -a 0.0.0.0 -p 80 -i ~/images
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
