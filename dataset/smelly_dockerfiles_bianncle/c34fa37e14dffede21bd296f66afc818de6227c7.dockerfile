FROM centos:7
RUN set -x \
 && yum install epel-release -y
RUN yum install -y ant boost-devel bzip2-devel curl expect fakeroot fuse-libs gcc gcc-c++ git gpg gtk3 java-1.8.0-openjdk java-1.8.0-openjdk-devel libcurl-devel libacl-devel libcap-devel libffi libuuid-devel libXcursor-devel libXrandr-devel libXScrnSaver-devel lsof make mariadb-libs openssl-devel pam-devel pango-devel patchelf postgresql-devel R rpmdevtools rpm-sign sudo wget xml-commons-apis zlib-devel libuser-devel
#  add scl repo and install additional dependencies
RUN yum install -y centos-release-scl
RUN yum install -y llvm-toolset-7 devtoolset-3-gcc devtoolset-3-gcc-c++
#  update environment to use new compiler
ENV PATH="/opt/rh/devtoolset-3/root/usr/bin:${PATH}"
ENV CC="/opt/rh/devtoolset-3/root/usr/bin/gcc"
ENV CXX="/opt/rh/devtoolset-3/root/usr/bin/c++"
ENV CPP="/opt/rh/devtoolset-3/root/usr/bin/cpp"
#  sudo defaults to requiretty on centos7
RUN sed -i 's/Defaults requiretty/Defaults !requiretty/' /etc/sudoers
# # run install-boost twice - boost exits 1 even though it has installed good enough for our uses.
# # https://github.com/rstudio/rstudio/blob/master/vagrant/provision-primary-user.sh#L12-L15
COPY dependencies/common/install-boost /tmp/
RUN bash /tmp/install-boost || bash /tmp/install-boost
#  install cmake
COPY package/linux/install-dependencies /tmp/
RUN bash /tmp/install-dependencies
#  install crashpad and its dependencies
COPY dependencies/common/install-crashpad /tmp/
RUN scl enable llvm-toolset-7 "/bin/bash /tmp/install-crashpad"
#  set github login from build argument if defined
ARG GITHUB_LOGIN
ENV RSTUDIO_GITHUB_LOGIN="$GITHUB_LOGIN"
#  install common dependencies
RUN mkdir -p /opt/rstudio-tools/dependencies/common
COPY dependencies/common/* /opt/rstudio-tools/dependencies/common/
RUN cd /opt/rstudio-tools/dependencies/common \
 && scl enable llvm-toolset-7 "/bin/bash ./install-common"
#  install Qt SDK
COPY dependencies/linux/install-qt-sdk /tmp/
RUN mkdir -p /opt/RStudio-QtSDK \
 && export QT_SDK_DIR=/opt/RStudio-QtSDK/Qt5.12.1 \
 && export QT_QPA_PLATFORM=minimal \
 && /tmp/install-qt-sdk
#  remove any previous users with conflicting IDs
ARG JENKINS_GID=999
ARG JENKINS_UID=999
COPY docker/jenkins/*.sh /tmp/
RUN /tmp/clean-uid.sh $JENKINS_UID \
 && /tmp/clean-gid.sh $JENKINS_GID
#  create jenkins user, make sudo. try to keep this toward the bottom for less cache busting
RUN groupadd -g $JENKINS_GID jenkins \
 && useradd -m -d /var/lib/jenkins -u $JENKINS_UID -g jenkins jenkins \
 && echo "jenkins ALL=(ALL) NOPASSWD: ALL" >> /etc/sudoers
