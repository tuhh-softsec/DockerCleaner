FROM debian:stretch
ENV LANG="C.UTF-8"
ENV LANGUAGE="C.UTF-8"
ENV LC_ALL="C.UTF-8"
RUN apt-get update \
 && apt-get install --no-install-recommends curl=7.52.1-5+deb9u16 build-essential=12.3 autotools-dev=20161112.1 automake=1:1.15-6 cmake=3.7.2-1 pkg-config=0.29-4+b1 ruby=1:2.3.3 ruby-dev=1:2.3.3 python-all=2.7.13-2 python-pip=9.0.1-2+deb9u2 python3-all=3.5.3-1 libpython3-dev=3.5.3-1 dh-exec=0.23+b1 dh-lua=24 liblua5.3-dev=5.3.3-1+deb9u1 tclcl-dev=1.20-8 libaugeas-dev=1.8.0-1+deb9u1 libbotan1.10-dev=1.10.17-1+deb9u1 libgpgme-dev=1.8.0-3+b2 libyajl-dev=2.1.0-2+b3 git=1:2.11.0-3+deb9u7 libgit2-dev=0.25.1+really0.24.6-1+deb9u1 libgtest-dev=1.8.0-6 libboost-all-dev=1.62.0.1 libssl-dev=1.1.0l-1~deb9u6 libdbus-1-dev=1.10.32-0+deb9u1 libpcre3-dev=2:8.39-3 libpcre++-dev=0.9.5-6.1 libglib2.0-dev=2.50.3-2+deb9u3 libxerces-c-dev=3.1.4+debian-2+deb9u2 qtbase5-dev=5.7.1+dfsg-3+deb9u3 qtdeclarative5-dev=5.7.1-2+b2 libmarkdown2-dev=2.2.2-1+deb9u1 discount=2.2.2-1+deb9u1 swig3.0=3.0.10-1.1 libuv1-dev=1.9.1-3 libev-dev=1:4.22-1+b1 libzmq3-dev=4.2.1-4+deb9u4 ghc=8.0.1-17+b1 ghc-dynamic cabal-install=1.24.0.1-3 alex=3.1.7-4 happy=1.19.5-7 c2hs=0.28.1-3 checkinstall=1.6.2-4 openjdk-8-jdk=8u332-ga-1~deb9u1 maven=3.3.9-4 gobject-introspection=1.50.0-1+b1 libgirepository1.0-dev=1.50.0-1+b1 systemd=232-25+deb9u14 libsystemd-dev=232-25+deb9u14 mingw-w64=5.0.1-1 wine=1.8.7-2 llvm=1:3.8-36 icheck=0.9.7-6.3+b1 devscripts=2.17.6+deb9u2 lintian=2.5.50.4 diffutils=1:3.5-3 patch=2.7.5-1+deb9u2 patchutils=0.3.4-2 git-buildpackage=0.8.12.2 reprepro=5.1.1-1 doxygen=1.8.13-4+b1 graphviz=2.38.0-17+deb9u1 gawk=1:4.1.4+dfsg-1 lcov=1.13-1 valgrind=1:3.12.0~svn20160714-1+b1 ed=1.10-2.1 dnsutils=1:9.10.3.dfsg.P4-12.3+deb9u12 virtualenv=15.1.0+ds-1 bison=2:3.0.4.dfsg-1+b1 uuid-dev=2.29.2-1+deb9u1 ninja-build=1.7.2-1 -y \
 && rm -rf /var/lib/apt/lists/*
RUN cabal update \
 && cabal install hspec QuickCheck
#   Google Test
ENV GTEST_ROOT="/opt/gtest"
ARG GTEST_VER=release-1.8.1
RUN mkdir -p ${GTEST_ROOT} \
 && cd /tmp \
 && curl -o gtest.tar.gz -L https://github.com/google/googletest/archive/${GTEST_VER}.tar.gz \
 && tar -zxvf gtest.tar.gz --strip-components=1 -C ${GTEST_ROOT} \
 && rm gtest.tar.gz
#   Handle Java
RUN echo 'export JAVA_HOME=$(readlink -f /usr/bin/javac | sed "s:/bin/javac::")' >> /etc/bash.bashrc
RUN echo '/usr/lib/jvm/java-8-openjdk-amd64/jre/lib/amd64/\n/usr/lib/jvm/java-8-openjdk-amd64/jre/lib/amd64/server/\n' > /etc/ld.so.conf.d/jdk.conf
#   ANTLR
ARG ANTLR_VERSION=4.7.2
RUN cd /usr/local/lib \
 && curl -o antlr.jar -L https://www.antlr.org/download/antlr-${ANTLR_VERSION}-complete.jar \
 && cd /usr/local/bin \
 && printf 'CLASSPATH=.:/usr/local/lib/antlr.jar exec java -jar /usr/local/lib/antlr.jar "$@"' > antlr4 \
 && printf 'java -classpath .:/usr/local/lib/antlr.jar org.antlr.v4.gui.TestRig "$@"' > grun \
 && chmod a+x antlr4 grun
#   ANTLR C++ runtime
RUN cd /tmp \
 && git clone --branch ${ANTLR_VERSION} --depth 1 https://github.com/antlr/antlr4.git \
 && cd antlr4/runtime/Cpp \
 && mkdir build \
 && cd build \
 && cmake -GNinja -DANTLR_JAR_LOCATION=/usr/local/lib/antlr.jar -DCMAKE_BUILD_TYPE=Release .. \
 && ninja \
 && ninja install \
 && cd /tmp \
 && rm -r antlr4
#   PEGTL
ARG PEGTL_VERSION=2.7.1
RUN cd /tmp \
 && git clone --branch ${PEGTL_VERSION} --depth 1 https://github.com/taocpp/PEGTL.git \
 && cp -R PEGTL/include/tao /usr/local/include \
 && rm -rf PEGTL
#   YAEP
ARG YAEP_VERSION=550de4cc5600d5f6109c7ebcfbacec51bf80d8d3
RUN cd /tmp \
 && mkdir yaep \
 && curl -o yaep.tar.gz -L https://github.com/vnmakarov/yaep/archive/${YAEP_VERSION}.tar.gz \
 && tar -zxvf yaep.tar.gz --strip-components=1 -C yaep \
 && cd yaep \
 && mkdir build \
 && cd build \
 && env CFLAGS='-fPIC' CXXFLAGS='-fPIC' cmake -DCMAKE_BUILD_TYPE=Release .. \
 && make install \
 && cd ../.. \
 && rm -r yaep yaep.tar.gz
#   yaml-cpp
RUN cd /tmp \
 && git clone --branch yaml-cpp-0.6.2 --depth 1 https://github.com/jbeder/yaml-cpp.git \
 && cd yaml-cpp \
 && mkdir build \
 && cd build \
 && cmake -GNinja -DYAML_CPP_BUILD_TESTS=OFF -DBUILD_SHARED_LIBS=ON .. \
 && ninja \
 && ninja install \
 && cd ../.. \
 && rm -r yaml-cpp
#   Update cache for shared libraries
RUN ldconfig
#   Create User:Group
#   The id is important as jenkins docker agents use the same id that is running
#   on the slaves to execute containers
ARG JENKINS_GROUPID
RUN groupadd -g ${JENKINS_GROUPID} -f jenkins
ARG JENKINS_USERID
RUN useradd --create-home --uid ${JENKINS_USERID} --gid ${JENKINS_GROUPID} --shell "/bin/bash" jenkins
USER ${JENKINS_USERID}
RUN git config --global user.email 'Jenkins <autobuilder@libelektra.org>' \
 && git config --global user.name 'Jenkins'
#   shfmt
ENV SHFMT_PATH="/home/jenkins/bin"
ENV SHFMT_VERSION="v2.6.3"
ENV PATH="${SHFMT_PATH}:${PATH}"
RUN mkdir -p "${SHFMT_PATH}" \
 && cd "${SHFMT_PATH}" \
 && curl -L "https://github.com/mvdan/sh/releases/download/${SHFMT_VERSION}/shfmt_${SHFMT_VERSION}_linux_amd64" -o shfmt \
 && chmod a+x shfmt
#   cmake-format
RUN pip install cmake-format[yaml]==0.4.5
#   Coveralls
ENV COVERALLS_VIRTUALENV_PATH="/home/jenkins/coveralls"
RUN virtualenv "${COVERALLS_VIRTUALENV_PATH}" \
 && cd "${COVERALLS_VIRTUALENV_PATH}" \
 && . bin/activate \
 && pip install "urllib3==1.22" \
 && pip install pyyaml==6.0 \
 && pip install cpp-coveralls==0.4.2 \
 && deactivate
ENV PATH="${PATH}:${COVERALLS_VIRTUALENV_PATH}/bin"
#   Handle Haskell dependencies
ENV HASKELL_SHARED_SANDBOX="/home/jenkins/elektra-cabal-sandbox"
RUN mkdir -p $HASKELL_SHARED_SANDBOX \
 && cd $HASKELL_SHARED_SANDBOX \
 && cabal update \
 && cabal sandbox init \
 && cabal install 'base >=4.9 \
 && <4.12' 'containers >=0.5 \
 && <0.6' 'directory >=1.2 \
 && <1.4' 'process >=1.4 \
 && <1.7' 'binary >=0.8 \
 && <0.9' 'haskell-src-exts-any' 'pretty -any' 'hint >=0.7.0 \
 && <0.8.0' 'temporary -any' 'exceptions -any' 'text -any' 'simple-logger -any' 'megaparsec -any' 'hspec -any' 'QuickCheck-any' --avoid-reinstalls
#   Workaround for issue [#2139](http://issues.libelektra.org/2139)
RUN mkdir -p /home/jenkins/.cabal/lib
# Please add your HEALTHCHECK here!!!
