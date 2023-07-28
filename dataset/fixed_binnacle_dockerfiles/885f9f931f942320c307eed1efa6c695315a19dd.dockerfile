#  # TODO: Ariellea -- this isn't working due to lack of cpu-checker availability in debian stable.
FROM debian:stable AS debian-stable-nojava
ENV DEBIAN_FRONTEND="noninteractive"
ARG BUILDARCH
COPY --from=gcr.io/bazel-public/base:bazelisk /usr/local/bin/bazel /usr/local/bin/bazel
COPY --from=gcr.io/bazel-public/base:buildifier /usr/local/bin/buildifier /usr/local/bin/buildifier
COPY --from=gcr.io/bazel-public/base:github-release /usr/local/bin/github-release /usr/local/bin/github-release
COPY --from=gcr.io/bazel-public/base:saucelabs /usr/local/bin/sc /usr/local/bin/sc
#  ## Install required packages.
RUN dpkg --add-architecture i386 \
 && apt-get update -qqy \
 && echo "Installing base packages" \
 && apt-get install --no-install-recommends apt-utils=2.2.4 curl=7.74.0-1.3+deb11u7 lsb-release=11.1.0 software-properties-common=0.96.20.2-2.1 -qqy \
 && echo "Installing packages required by Bazel" \
 && apt-get install --no-install-recommends build-essential=12.9 clang=1:11.0-51+nmu5 curl=7.74.0-1.3+deb11u7 ed=1.17-1 git=1:2.30.2-1+deb11u2 iproute2=5.10.0-4 iputils-ping=3:20210202-1 netcat-openbsd=1.217-3 python python-dev python3=3.9.2-3 python3-dev=3.9.2-3 unzip=6.0-26+deb11u1 wget=1.21-1+deb11u1 xvfb=2:1.20.11-1+deb11u6 zip=3.0-12 zlib1g-dev=1:1.2.11.dfsg-2+deb11u2 -qqy \
 && echo "Installing packages required by Android SDK" \
 && apt-get install --no-install-recommends expect=5.45.4-2+b1 libbz2-1.0:i386 libncurses5:i386 libstdc++6:i386 libz1:i386 -qqy \
 && echo "Installing packages required by Tensorflow" \
 && apt-get install --no-install-recommends libcurl3-dev swig=4.0.2-1 python-enum34 python-mock python-numpy python-pip python-wheel python3-mock=4.0.3-1 python3-numpy=1:1.19.5-1 python3-pip=20.3.4-4+deb11u1 python3-wheel=0.34.2-1 -qqy \
 && echo "Installing packages required by Envoy" \
 && apt-get install --no-install-recommends automake=1:1.16.3-2 autotools-dev=20180224.1+nmu1 cmake=3.18.4-2+deb11u1 libtool=2.4.6-15 m4=1.4.18-5 ninja-build=1.10.1-1 -qqy \
 && echo "Installing packages required by Android emulator" \
 && apt-get install --no-install-recommends cpio=2.13+dfsg-4 lsof=4.93.2+dfsg-1.1 qemu-kvm qemu-system-x86=1:5.2+dfsg-11+deb11u2 unzip=6.0-26+deb11u1 xvfb=2:1.20.11-1+deb11u6 -qqy \
 && echo "Installing packages required by Bazel release process" \
 && apt-get install --no-install-recommends devscripts=2.21.3+deb11u1 gnupg=2.2.27-2+deb11u2 pandoc=2.9.2.1-1+b1 reprepro=5.3.0-1.2 -qqy \
 && echo "Installing packages required by C++ coverage tests" \
 && apt-get install --no-install-recommends lcov=1.14-2 llvm=1:11.0-51+nmu5 -qqy \
 && echo "Installing packages required by Swift toolchain" \
 && apt-get install --no-install-recommends clang=1:11.0-51+nmu5 libicu-dev=67.1-7 -qqy \
 && echo "Installing packages required by rules_webtesting" \
 && apt-get install --no-install-recommends python-urllib3 python3-urllib3=1.26.5-1~exp1 -qqy \
 && echo "Installing packages required by Kythe" \
 && apt-get install --no-install-recommends bison=2:3.7.5+dfsg-1 flex=2.6.4-8 uuid-dev=2.36.1-8+deb11u1 asciidoc=9.0.0~rc2-1 graphviz=2.42.2-5 source-highlight=3.1.9-3+b1 -qqy \
 && echo "Installing packages required by Bazel (Ubuntu 14.04 and 16.04 only)" \
 && apt-get install --no-install-recommends realpath libssl-dev=1.1.1n-0+deb11u4 -qqy \
 && apt-get -qqy purge apport \
 && rm -rf /var/lib/apt/lists/*
#  ## Install Google Cloud SDK.
#  ## https://cloud.google.com/sdk/docs/quickstart-debian-ubuntu
RUN export CLOUD_SDK_REPO="cloud-sdk-$( lsb_release -c -s ;)" \
 && echo "deb http://packages.cloud.google.com/apt $CLOUD_SDK_REPO main" | tee -a /etc/apt/sources.list.d/google-cloud-sdk.list \
 && curl -L https://packages.cloud.google.com/apt/doc/apt-key.gpg | apt-key add - \
 && apt-get update -qqy \
 && apt-get install --no-install-recommends google-cloud-sdk -qqy \
 && rm -rf /var/lib/apt/lists/*
#  ## Install Docker.
RUN apt-get update -qqy \
 && apt-get install --no-install-recommends apt-transport-https=2.2.4 ca-certificates=20210119 -qqy \
 && curl -sSL https://download.docker.com/linux/debian/gpg | apt-key add - \
 && add-apt-repository "deb [arch=$BUILDARCH] https://download.docker.com/linux/debian $( lsb_release -cs ;) stable" \
 && apt-get update -qqy \
 && apt-get install --no-install-recommends docker-ce -qqy \
 && rm -rf /var/lib/apt/lists/*
#  ## Install Node.js and packages required by Gerrit.
#  ## (see https://gerrit.googlesource.com/gerrit/+show/master/polygerrit-ui/README.md)
RUN curl -L https://deb.nodesource.com/setup_10.x | bash - \
 && apt-get update -qqy \
 && apt-get install --no-install-recommends nodejs=12.22.12~dfsg-1~deb11u3 -qqy \
 && npm install typescript@5.0.4 polylint@2.10.4 polymer-cli@1.9.11 web-component-tester@6.8.0 eslint@"=4.19.0" eslint-config-google@"=0.12.0" eslint-plugin-html@"=5.0.3" eslint-plugin-promise@"=4.0.1" fried-twinkie@"^0.2.2" --unsafe-perm -g \
 && rm -rf /var/lib/apt/lists/*
#  ## Install Python (required our own bazelci.py script).
RUN export PYTHON_VERSION="3.6.8" \
 && mkdir -p /usr/local/src \
 && cd /usr/local/src \
 && curl -LO "https://www.python.org/ftp/python/${PYTHON_VERSION}/Python-${PYTHON_VERSION}.tar.xz" \
 && tar xfJ "Python-${PYTHON_VERSION}.tar.xz" \
 && rm "Python-${PYTHON_VERSION}.tar.xz" \
 && cd "Python-${PYTHON_VERSION}" \
 && echo "_ssl _ssl.c -DUSE_SSL -I/usr/include -I/usr/include/openssl -L/usr/lib -lssl -lcrypto" >> Modules/Setup.dist \
 && echo "Compiling Python ${PYTHON_VERSION} ..." \
 && ./configure --quiet --enable-ipv6 \
 && make -s -j all \
 && echo "Installing Python ${PYTHON_VERSION} ..." \
 && make -s altinstall \
 && pip3.6 install requests uritemplate pyyaml github3.py \
 && rm -rf "/usr/local/src/Python-${PYTHON_VERSION}"
#  ## Install Go.
ENV GO_HOME="\"/opt/go1.12.6.linux-amd64\""
ENV PATH="\"${PATH}:${GO_HOME}/bin\""
#  ## Install Swift toolchain (required by rules_swift).
ENV SWIFT_HOME="\"/opt/swift-4.2.1-RELEASE-ubuntu14.04\""
ENV PATH="\"${PATH}:${SWIFT_HOME}/usr/bin\""
FROM debian-stable-nojava AS debian-stable-java8
RUN apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv 0x219BD9C9 \
 && apt-add-repository 'deb http://repos.azulsystems.com/ubuntu stable main' \
 && apt-get update -qqy \
 && apt-get install --no-install-recommends zulu-8 -qqy \
 && rm -rf /var/lib/apt/lists/*
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
