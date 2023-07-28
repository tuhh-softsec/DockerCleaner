#
#   PX4 base development environment
#
FROM ubuntu:18.04
LABEL maintainer="Daniel Agar <daniel@agar.ca>"
ENV DEBIAN_FRONTEND="noninteractive"
RUN apt-get update \
 && apt-get install --no-install-recommends bzip2=1.0.6-8.1ubuntu0.2 ca-certificates=20211016ubuntu0.18.04.1 ccache=3.4.1-1 cmake=3.10.2-1ubuntu2.18.04.2 cppcheck=1.82-1 curl=7.58.0-2ubuntu3.24 dirmngr=2.2.4-1ubuntu1.6 doxygen=1.8.13-10 file=1:5.32-2ubuntu0.4 g++=4:7.4.0-1ubuntu2.3 gcc=4:7.4.0-1ubuntu2.3 gdb=8.1.1-0ubuntu1 git=1:2.17.1-1ubuntu0.17 gnupg=2.2.4-1ubuntu1.6 gosu=1.10-1ubuntu0.18.04.1 lcov=1.13-3 libfreetype6-dev=2.8.1-2ubuntu2.2 libgtest-dev=1.8.0-6 libpng-dev=1.6.34-1ubuntu0.18.04.2 lsb-release=9.20170808ubuntu1 make=4.1-9.1ubuntu1 ninja-build=1.8.2-1 openjdk-8-jdk=8u362-ga-0ubuntu1~18.04.1 openjdk-8-jre=8u362-ga-0ubuntu1~18.04.1 openssh-client=1:7.6p1-4ubuntu0.7 pkg-config=0.29.1-0ubuntu2 python-pip=9.0.1-2.3~ubuntu1.18.04.8 python-pygments=2.2.0+dfsg-1ubuntu0.2 python-setuptools=39.0.1-2ubuntu0.1 rsync=3.1.2-2.1ubuntu1.6 shellcheck=0.4.6-1 tzdata=2022g-0ubuntu0.18.04 unzip=6.0-21ubuntu1.2 wget=1.19.4-1ubuntu2.2 xsltproc=1.1.29-5ubuntu0.3 zip=3.0-11build1 -y --quiet \
 && apt-get -y autoremove \
 && apt-get clean autoclean \
 && rm -rf /var/lib/apt/lists/{apt,dpkg,cache,log} /tmp/* /var/tmp/*
#   gtest
RUN cd /usr/src/gtest \
 && mkdir build \
 && cd build \
 && cmake .. \
 && make \
 && cp *.a /usr/lib \
 && cd .. \
 && rm -rf build
RUN python -m pip install --upgrade pip \
 && pip install setuptools==67.6.1 pkgconfig==1.5.5 wheel==0.40.0 \
 && pip install argparse==1.4.0 argcomplete==3.0.5 coverage==7.2.3 jinja2==3.1.2 empy==3.3.4 numpy==1.24.2 requests==2.28.2 serial==0.0.97 toml==0.10.2 pyyaml==6.0 cerberus==1.3.4
#   manual ccache setup
RUN ln -s /usr/bin/ccache /usr/lib/ccache/cc \
 && ln -s /usr/bin/ccache /usr/lib/ccache/c++
#   astyle v2.06
RUN wget -q https://downloads.sourceforge.net/project/astyle/astyle/astyle%202.06/astyle_2.06_linux.tar.gz -O /tmp/astyle.tar.gz \
 && cd /tmp \
 && tar zxf astyle.tar.gz \
 && cd astyle/src \
 && make -f ../build/gcc/Makefile \
 && cp bin/astyle /usr/local/bin \
 && rm -rf /tmp/*
#   Gradle (Required to build Fast-RTPS)
RUN wget -q "https://services.gradle.org/distributions/gradle-5.4.1-bin.zip" -O /tmp/gradle-5.4.1-bin.zip \
 && mkdir /opt/gradle \
 && cd /tmp \
 && unzip -d /opt/gradle gradle-5.4.1-bin.zip \
 && rm -rf /tmp/*
ENV PATH="\"/opt/gradle/gradle-5.4.1/bin:$PATH\""
#   Fast-RTPS
RUN git clone --recursive https://github.com/eProsima/Fast-RTPS.git -b release/1.7.2 /tmp/Fast-RTPS-1.7.2 \
 && cd /tmp/Fast-RTPS-1.7.2 \
 && mkdir build \
 && cd build \
 && cmake -DTHIRDPARTY=ON -DBUILD_JAVA=ON .. \
 && make \
 && make install \
 && rm -rf /tmp/*
#   create user with id 1001 (jenkins docker workflow default)
RUN useradd --shell /bin/bash -u 1001 -c "" -m user \
 && usermod -a -G dialout user
#   setup virtual X server
RUN mkdir /tmp/.X11-unix \
 && chmod 1777 /tmp/.X11-unix \
 && chown -R root:root /tmp/.X11-unix
ENV DISPLAY=":99"
ENV CCACHE_UMASK="000"
ENV FASTRTPSGEN_DIR="/usr/local/bin/"
ENV PATH="/usr/lib/ccache:$PATH"
ENV TERM="xterm"
ENV TZ="UTC"
#   SITL UDP PORTS
EXPOSE 14556/udp
EXPOSE 14557/udp
#   create and start as LOCAL_USER_ID
COPY scripts/entrypoint.sh /usr/local/bin/entrypoint.sh
ENTRYPOINT ["/usr/local/bin/entrypoint.sh"]
CMD ["/bin/bash"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
