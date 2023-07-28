FROM ubuntu:16.04
#   number of concurrent threads during build
#   usage: docker build --build-arg PARALLELISM=8 -t name/name .
ARG PARALLELISM=1
ARG CMAKE_BUILD_TYPE=Release
ENV IROHA_HOME="/opt/iroha"
ENV IROHA_BUILD="/opt/iroha/build"
RUN apt-get update \
 && (apt-get update ;apt-get install --no-install-recommends apt-utils=1.2.35 software-properties-common=0.96.20.10 wget=1.17.1-1ubuntu1.5 -y ) ; apt-get -y clean
#   add repos
RUN set -e ; add-apt-repository -y ppa:git-core/ppa ; add-apt-repository -y ppa:ubuntu-toolchain-r/test ; wget -O - https://apt.llvm.org/llvm-snapshot.gpg.key | apt-key add - ; echo 'deb http://apt.llvm.org/xenial/ llvm-toolchain-xenial-6.0 main' >> /etc/apt/sources.list; :
RUN set -e ; (apt-get update ;apt-get install --no-install-recommends python-software-properties=0.96.20.10 automake=1:1.15-4ubuntu1 libtool=2.4.6-0.1 build-essential=12.1ubuntu2 clang-6.0=1:6.0-1ubuntu2~16.04.1 lldb-6.0=1:6.0-1ubuntu2~16.04.1 lld-6.0=1:6.0-1ubuntu2~16.04.1 g++-7 libssl-dev=1.0.2g-1ubuntu4.20 zlib1g-dev=1:1.2.8.dfsg-2ubuntu4.3 libcurl4-openssl-dev=7.47.0-1ubuntu2.19 libc6-dbg=2.23-0ubuntu11.3 golang=2:1.6-1ubuntu4 git=1:2.7.4-0ubuntu1.10 ssh=1:7.2p2-4ubuntu2.10 tar=1.28-2.1ubuntu0.2 gzip=1.6-4ubuntu1 ca-certificates=20210119~16.04.1 gnupg=1.4.20-1ubuntu3.3 python-pip=8.1.1-2ubuntu0.6 python3-pip=8.1.1-2ubuntu0.6 python3-setuptools=20.7.0-1 python-dev=2.7.12-1~16.04 curl=7.47.0-1ubuntu2.19 file=1:5.25-2ubuntu1.4 gdb=7.11.1-0ubuntu1~16.5 gdbserver=7.11.1-0ubuntu1~16.5 ccache=3.2.4-1 gcovr=3.2-1 cppcheck=1.72-1 doxygen=1.8.11-1ubuntu0.1 rsync=3.1.1-3ubuntu1.3 graphviz=2.38.0-12ubuntu2.1 graphviz-dev=2.38.0-12ubuntu2.1 unzip=6.0-20ubuntu1.1 vim=2:7.4.1689-3ubuntu1.5 zip=3.0-11 -y ) ; apt-get -y clean
#   install cmake 3.11.4
RUN set -e ; git clone https://gitlab.kitware.com/cmake/cmake.git /tmp/cmake ; (cd /tmp/cmake ;git checkout 316bd45439ad8ced6b31bcb10303a788038387ef ) ; (cd /tmp/cmake ;/tmp/cmake/bootstrap --system-curl --parallel=${PARALLELISM} --enable-ccache ) ; make -j${PARALLELISM} -C /tmp/cmake ; make -C /tmp/cmake install ; ldconfig ; rm -rf /tmp/cmake
#   install boost 1.65.1
RUN set -e ; git clone https://github.com/boostorg/boost /tmp/boost ; (cd /tmp/boost ;git checkout 436ad1dfcfc7e0246141beddd11c8a4e9c10b146 ) ; (cd /tmp/boost ;git submodule update --init --recursive ) ; (cd /tmp/boost ;/tmp/boost/bootstrap.sh --with-libraries=system,filesystem,thread ) ; (cd /tmp/boost ;/tmp/boost/b2 headers ) ; (cd /tmp/boost ;/tmp/boost/b2 cxxflags="-std=c++14" -j ${PARALLELISM} install --prefix=/opt/dependencies/boost ) ; ldconfig ; rm -rf /tmp/boost
#   install protobuf v3.5.1
RUN set -e ; git clone https://github.com/google/protobuf /tmp/protobuf ; (cd /tmp/protobuf ;git checkout 106ffc04be1abf3ff3399f54ccf149815b287dd9 ) ; cmake -DCMAKE_BUILD_TYPE=${CMAKE_BUILD_TYPE} -Dprotobuf_BUILD_TESTS=OFF -Dprotobuf_BUILD_SHARED_LIBS=ON -H/tmp/protobuf/cmake -B/tmp/protobuf/.build -DCMAKE_INSTALL_PREFIX=/opt/dependencies/protobuf ; cmake --build /tmp/protobuf/.build --target install -- -j${PARALLELISM} ; ldconfig ; rm -rf /tmp/protobuf
#   install gflags
RUN set -e ; git clone https://github.com/gflags/gflags /tmp/gflags ; (cd /tmp/gflags ;git checkout f8a0efe03aa69b3336d8e228b37d4ccb17324b88 ) ; cmake -DCMAKE_BUILD_TYPE=${CMAKE_BUILD_TYPE} -H/tmp/gflags -B/tmp/gflags/build -DCMAKE_INSTALL_PREFIX=/opt/dependencies/gflags ; cmake --build /tmp/gflags/build --target install -- -j${PARALLELISM} ; ldconfig ; rm -rf /tmp/gflags
#   install c-ares
RUN set -e ; git clone https://github.com/c-ares/c-ares /tmp/c-ares ; (cd /tmp/c-ares ;git checkout 3be1924221e1326df520f8498d704a5c4c8d0cce ) ; cmake -DCMAKE_BUILD_TYPE=${CMAKE_BUILD_TYPE} -H/tmp/c-ares -B/tmp/c-ares/build -DCMAKE_INSTALL_PREFIX=/opt/dependencies/c-ares ; cmake --build /tmp/c-ares/build --target install -- -j${PARALLELISM} ; ldconfig ; rm -rf /tmp/c-ares
#   install grpc 1.11.0
RUN set -e ; export LD_LIBRARY_PATH="/opt/dependencies/protobuf/lib:$LD_LIBRARY_PATH" ; git clone https://github.com/grpc/grpc /tmp/grpc ; (cd /tmp/grpc ;git checkout bd44e485f69d70ca4095cea92decd98de3892aa6 ) ; cmake -DCMAKE_BUILD_TYPE=${CMAKE_BUILD_TYPE} -DgRPC_BENCHMARK_PROVIDER="" -DgRPC_ZLIB_PROVIDER=package -DgRPC_CARES_PROVIDER=package -DgRPC_SSL_PROVIDER=package -DgRPC_PROTOBUF_PROVIDER=package -DgRPC_GFLAGS_PROVIDER=package -DBUILD_SHARED_LIBS=ON -H/tmp/grpc -B/tmp/grpc/.build -DCMAKE_PREFIX_PATH="/opt/dependencies/c-ares;/opt/dependencies/protobuf" -DCMAKE_INSTALL_PREFIX=/opt/dependencies/grpc ; cmake --build /tmp/grpc/.build --target install -- -j${PARALLELISM} ; ldconfig ; rm -rf /tmp/grpc
#   install gtest
RUN set -e ; git clone https://github.com/google/googletest /tmp/googletest ; (cd /tmp/googletest ;git checkout ec44c6c1675c25b9827aacd08c02433cccde7780 ) ; cmake -DCMAKE_BUILD_TYPE=${CMAKE_BUILD_TYPE} -H/tmp/googletest -B/tmp/googletest/build -DCMAKE_INSTALL_PREFIX=/opt/dependencies/gtest ; cmake --build /tmp/googletest/build --target install -- -j${PARALLELISM} ; ldconfig ; rm -rf /tmp/googletest
#   install spdlog v0.16.3
RUN set -e ; git clone https://github.com/gabime/spdlog /tmp/spdlog ; (cd /tmp/spdlog ;git checkout ccd675a286f457068ee8c823f8207f13c2325b26 ) ; cmake -DCMAKE_BUILD_TYPE=${CMAKE_BUILD_TYPE} -DSPDLOG_BUILD_TESTING=OFF -H/tmp/spdlog -B/tmp/spdlog/build -DCMAKE_INSTALL_PREFIX=/opt/dependencies/spdlog ; cmake --build /tmp/spdlog/build --target install ; rm -rf /tmp/spdlog
#   install rxcpp
RUN set -e ; git clone https://github.com/Reactive-Extensions/RxCpp /tmp/RxCpp ; (cd /tmp/RxCpp ;git checkout a7d5856385f126e874db6010d9dbfd37290c61de ) ; cmake -DCMAKE_BUILD_TYPE=${CMAKE_BUILD_TYPE} -H/tmp/RxCpp -B/tmp/RxCpp/build -DCMAKE_INSTALL_PREFIX=/opt/dependencies/rxcpp ; cmake --build /tmp/RxCpp/build --target install ; rm -rf /tmp/RxCpp
#   install rapidjson
RUN set -e ; git clone https://github.com/miloyip/rapidjson /tmp/rapidjson ; (cd /tmp/rapidjson ;git checkout f54b0e47a08782a6131cc3d60f94d038fa6e0a51 ) ; cmake -DCMAKE_BUILD_TYPE=${CMAKE_BUILD_TYPE} -DRAPIDJSON_BUILD_EXAMPLES=OFF -H/tmp/rapidjson -B/tmp/rapidjson/build -DCMAKE_INSTALL_PREFIX=/opt/dependencies/rapidjson ; cmake --build /tmp/rapidjson/build --target install ; ldconfig ; rm -rf /tmp/rapidjson
#   install libpq
RUN set -e ; git clone --progress https://git.postgresql.org/git/postgresql.git /tmp/postgresql ; cd /tmp/postgresql ; git checkout 029386ccbddd0a33d481b94e511f5219b03e6636 ; ./configure --without-readline --prefix=/opt/dependencies/libpq ; make -j${PARALLELISM} -C src/bin/pg_config ; make -j${PARALLELISM} -C src/interfaces/libpq ; make -j${PARALLELISM} -C src/backend/utils fmgroids.h ; cp src/backend/utils/fmgroids.h src/include/utils/fmgroids.h ; make -C src/bin/pg_config install ; make -C src/interfaces/libpq install ; make -C src/include install ; ldconfig ; rm -rf /tmp/postgresql
#   install soci 3.2.3
RUN set -e ; git clone https://github.com/SOCI/soci /tmp/soci ; (cd /tmp/soci ;git checkout 111b50af8c3876ea392367640b4bd83b4f903ab8 ) ; cmake -DCMAKE_BUILD_TYPE=${CMAKE_BUILD_TYPE} -DWITH_BOOST=ON -DWITH_DB2=OFF -DWITH_FIREBIRD=OFF -DWITH_MYSQL=OFF -DWITH_ODBC=OFF -DWITH_ORACLE=OFF -DWITH_POSTGRESQL=ON -DWITH_SQLITE3=OFF -H/tmp/soci/src -B/tmp/soci/build -DCMAKE_PREFIX_PATH="/opt/dependencies/libpq" -DCMAKE_INSTALL_PREFIX=/opt/dependencies/soci ; cmake --build /tmp/soci/build --target install ; ldconfig ; rm -rf /tmp/soci
#   install tbb
RUN set -e ; git clone https://github.com/01org/tbb /tmp/tbb ; (cd /tmp/tbb ;git checkout eb6336ad29450f2a64af5123ca1b9429ff6bc11d ) ; make -j${PARALLELISM} -C /tmp/tbb tbb_build_prefix=build ; mkdir /opt/dependencies/tbb /opt/dependencies/tbb/lib /opt/dependencies/tbb/include ; cp /tmp/tbb/build/build_debug/*.so* /opt/dependencies/tbb/lib ; cp /tmp/tbb/build/build_release/*.so* /opt/dependencies/tbb/lib ; cp -r /tmp/tbb/include/* /opt/dependencies/tbb/include ; ldconfig ; rm -rf /tmp/tbb
#   install sonar cli
ENV SONAR_CLI_VERSION="3.0.3.778"
RUN set -e ; mkdir -p /opt/sonar ; curl -L -o /tmp/sonar.zip https://binaries.sonarsource.com/Distribution/sonar-scanner-cli/sonar-scanner-cli-${SONAR_CLI_VERSION}-linux.zip ; unzip -o -d /tmp/sonar-scanner /tmp/sonar.zip ; mv /tmp/sonar-scanner/sonar-scanner-${SONAR_CLI_VERSION}-linux /opt/sonar/scanner ; ln -s -f /opt/sonar/scanner/bin/sonar-scanner /usr/local/bin/sonar-scanner ; rm -rf /tmp/sonar*
#   install ed25519
RUN set -e ; git clone git://github.com/hyperledger/iroha-ed25519.git /tmp/ed25519 ; (cd /tmp/ed25519 ;git checkout b61a1e77af5dc458ed6a5aee395d5b22775a4917 ) ; cmake -DCMAKE_BUILD_TYPE=${CMAKE_BUILD_TYPE} -DTESTING=OFF -DHASH=sha3_brainhub -DEDIMPL=ref10 -H/tmp/ed25519 -B/tmp/ed25519/build -DCMAKE_INSTALL_PREFIX=/opt/dependencies/ed25519 ; cmake --build /tmp/ed25519/build --target install -- -j${PARALLELISM} ; ldconfig ; rm -rf /tmp/ed25519
#   fetch lcov reports converter
RUN set -e ; curl -L -o /tmp/lcov_cobertura.py https://raw.githubusercontent.com/eriwen/lcov-to-cobertura-xml/8c55cd11f80a21e7e46f20f8c81fcde0bf11f5e5/lcov_cobertura/lcov_cobertura.py
RUN set -e ; add-apt-repository -y ppa:webupd8team/java ; apt-get update ; echo oracle-java8-installer shared/accepted-oracle-license-v1-1 select true | /usr/bin/debconf-set-selections ; (apt-get update ;apt-get install --no-install-recommends oracle-java8-installer -y ) ; java -version
RUN set -e ; add-apt-repository -y ppa:jonathonf/python-3.6 ; apt-get update ; (apt-get update ;apt-get install --no-install-recommends python3.6-dev -y )
#   python bindings dependencies
RUN set -e ; pip install grpcio_tools==1.53.0 pysha3==1.0.2 ; pip3 install grpcio_tools pysha3
#   install lcov
RUN set -e ; curl -L -o /tmp/lcov-1.13.tar.gz https://github.com/linux-test-project/lcov/releases/download/v1.13/lcov-1.13.tar.gz ; cd /tmp ; tar zxf lcov-1.13.tar.gz ; cd lcov-1.13 ; make install
#   non-interactive adduser
#     -m = create home dir
#     -s = set default shell
#     iroha-dpnds-test = username
#     -u = userid, default for Ubuntu is 1000
#     -U = create a group same as username
#     no password
RUN useradd -ms /bin/bash iroha-dpnds-test -u 1000 -U
WORKDIR /opt/iroha
RUN set -e ; chmod -R 777 /opt/iroha ; mkdir -p /tmp/ccache -m 777 ; ccache --clear
USER iroha-dpnds-test
CMD ["/bin/bash"]
# Please add your HEALTHCHECK here!!!
