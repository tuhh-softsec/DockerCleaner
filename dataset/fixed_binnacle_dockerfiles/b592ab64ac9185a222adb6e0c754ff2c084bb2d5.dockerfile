#
#   PX4 base development environment
#
FROM ubuntu:16.04
LABEL maintainer="Daniel Agar <daniel@agar.ca>"
ENV DEBIAN_FRONTEND="noninteractive"
RUN apt-get update \
 && apt-get install --no-install-recommends bzip2=1.0.6-8ubuntu0.2 ca-certificates=20210119~16.04.1 ccache=3.2.4-1 cmake=3.5.1-1ubuntu3 cppcheck=1.72-1 curl=7.47.0-1ubuntu2.19 dirmngr=2.1.11-6ubuntu2.1 doxygen=1.8.11-1ubuntu0.1 file=1:5.25-2ubuntu1.4 g++=4:5.3.1-1ubuntu1 gcc=4:5.3.1-1ubuntu1 gdb=7.11.1-0ubuntu1~16.5 git=1:2.7.4-0ubuntu1.10 gnupg=1.4.20-1ubuntu3.3 gosu=1.7-1 lcov=1.12-2 libfreetype6-dev=2.6.1-0.1ubuntu2.5 libgtest-dev=1.7.0-4ubuntu1 libpng-dev lsb-release=9.20160110ubuntu0.2 make=4.1-6 ninja-build=1.5.1-0.1ubuntu1 openjdk-8-jdk=8u292-b10-0ubuntu1~16.04.1 openjdk-8-jre=8u292-b10-0ubuntu1~16.04.1 openssh-client=1:7.2p2-4ubuntu2.10 pkg-config=0.29.1-0ubuntu1 python-pip=8.1.1-2ubuntu0.6 python-pygments=2.1+dfsg-1ubuntu0.2 python-setuptools=20.7.0-1 rsync=3.1.1-3ubuntu1.3 shellcheck=0.3.7-5 tzdata=2021a-0ubuntu0.16.04 unzip=6.0-20ubuntu1.1 wget=1.17.1-1ubuntu1.5 xsltproc=1.1.28-2.1ubuntu0.3 zip=3.0-11 -y --quiet \
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
#   Fast-RTPS
RUN wget -q "http://www.eprosima.com/index.php/component/ars/repository/eprosima-fast-rtps/eprosima-fast-rtps-1-6-0/eprosima_fastrtps-1-6-0-linux-tar-gz?format=raw" -O /tmp/eprosima_fastrtps.tar.gz \
 && cd /tmp \
 && tar zxf eprosima_fastrtps.tar.gz \
 && mkdir -p /usr/local/share/fastrtps \
 && cp eProsima_FastRTPS-1.6.0-Linux/share/fastrtps/fastrtpsgen.jar /usr/local/share/fastrtps/fastrtpsgen.jar \
 && cp eProsima_FastRTPS-1.6.0-Linux/bin/fastrtpsgen /usr/local/bin/fastrtpsgen \
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
