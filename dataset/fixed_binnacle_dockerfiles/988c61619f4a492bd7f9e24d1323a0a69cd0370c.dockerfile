FROM ubuntu:16.04
#   create terrama2 user
ENV USER="terrama2"
RUN useradd -s /bin/bash -m ${USER}
RUN echo ${USER}:${USER} | chpasswd
RUN usermod -aG sudo ${USER}
#   install minimal dependencies
RUN apt-get update \
 && apt-get install --no-install-recommends apt-transport-https=1.2.35 ca-certificates=20210119~16.04.1 curl=7.47.0-1ubuntu2.19 gnupg=1.4.20-1ubuntu3.3 git=1:2.7.4-0ubuntu1.10 make=4.1-6 sudo=1.8.16-0ubuntu1.10 wget=1.17.1-1ubuntu1.5 software-properties-common=0.96.20.10 python-software-properties=0.96.20.10 -y
#   GCC 8
RUN add-apt-repository -y ppa:ubuntu-toolchain-r/test \
 && apt-get update \
 && apt-get install --no-install-recommends gcc-8 g++-8 -y \
 && update-alternatives --install /usr/bin/gcc gcc /usr/bin/gcc-8 100 \
 && update-alternatives --install /usr/bin/gcc-ar gcc-ar /usr/bin/gcc-ar-8 100 \
 && update-alternatives --install /usr/bin/gcc-nm gcc-nm /usr/bin/gcc-nm-8 100 \
 && update-alternatives --install /usr/bin/gcc-ranlib gcc-ranlib /usr/bin/gcc-ranlib-8 100 \
 && update-alternatives --install /usr/bin/g++ g++ /usr/bin/g++-8 100
#   Clang 6
RUN sh -c 'echo "deb http://apt.llvm.org/xenial/ llvm-toolchain-xenial-6.0 main" > /etc/apt/sources.list.d/llvm.list' \
 && wget -O - https://apt.llvm.org/llvm-snapshot.gpg.key | apt-key add - \
 && apt-get update \
 && apt-get install --no-install-recommends clang-6.0=1:6.0-1ubuntu2~16.04.1 clang-format-6.0=1:6.0-1ubuntu2~16.04.1 clang-tidy-6.0=1:6.0-1ubuntu2~16.04.1 clang-tools-6.0=1:6.0-1ubuntu2~16.04.1 lldb-6.0=1:6.0-1ubuntu2~16.04.1 python-clang-6.0=1:6.0-1ubuntu2~16.04.1 python-lldb-6.0=1:6.0-1ubuntu2~16.04.1 -y
RUN apt-get install --no-install-recommends gdb=7.11.1-0ubuntu1~16.5 valgrind=1:3.11.0-1ubuntu4.2 cppcheck=1.72-1 -y
#   Install VSCode
RUN curl https://packages.microsoft.com/keys/microsoft.asc | gpg --dearmor > microsoft.gpg \
 && mv microsoft.gpg /etc/apt/trusted.gpg.d/microsoft.gpg \
 && sh -c 'echo "deb [arch=amd64] https://packages.microsoft.com/repos/vscode stable main" > /etc/apt/sources.list.d/vscode.list' \
 && apt-get update \
 && apt-get install --no-install-recommends code libasound2=1.1.0-0ubuntu1 libatk1.0-0=2.18.0-1 libcairo2=1.14.6-1 libcups2=2.1.3-4ubuntu0.11 libexpat1=2.1.0-7ubuntu0.16.04.5 libfontconfig1=2.11.94-0ubuntu1.1 libfreetype6=2.6.1-0.1ubuntu2.5 libgtk2.0-0=2.24.30-1ubuntu1.16.04.2 libpango-1.0-0=1.38.1-1 libx11-xcb1=2:1.6.3-1ubuntu2.2 libxcomposite1=1:0.4.4-1 libxcursor1=1:1.1.14-1ubuntu0.16.04.2 libxdamage1=1:1.1.4-2 libxext6=2:1.3.3-1 libxfixes3=1:5.0.1-2 libxi6=2:1.7.6-1 libxrandr2=2:1.5.0-1 libxrender1=1:0.9.9-0ubuntu1 libxss1=1:1.2.2-1 libxtst6=2:1.2.2-1 -y
#   RUN mkdir -p /opt/qtcreator && \
#       cd /opt/qtcreator && \
#       apt-get update && apt-get install -y libxcb-keysyms1-dev libxcb-image0-dev \
#       libxcb-shm0-dev libxcb-icccm4-dev libxcb-sync0-dev libxcb-xfixes0-dev \
#       libxcb-shape0-dev libxcb-randr0-dev libxcb-render-util0-dev \
#       libfontconfig1-dev libfreetype6-dev libx11-dev libxext-dev libxfixes-dev \
#       libxi-dev libxrender-dev libxcb1-dev libx11-xcb-dev libxcb-glx0-dev x11vnc \
#       xauth build-essential mesa-common-dev libglu1-mesa-dev libxkbcommon-dev \
#       libxcb-xkb-dev libxslt1-dev libgstreamer-plugins-base0.10-dev p7zip-full && \
#       wget https://download.qt.io/official_releases/qtcreator/4.6/4.6.1/installer_source/linux_gcc_64_rhel72/qtcreator.7z && \
#       7z x qtcreator.7z && \
#       cd /usr/local/bin && \
#       ln -s /opt/qtcreator/bin/qtcreator
RUN echo "${USER} ALL=(ALL) NOPASSWD: /usr/bin/apt-get" >> /etc/sudoers \
 && echo "${USER} ALL=(ALL) NOPASSWD: /usr/bin/update-alternatives" >> /etc/sudoers \
 && echo "${USER} ALL=(ALL) NOPASSWD: /usr/sbin/usermod" >> /etc/sudoers \
 && echo "${USER} ALL=(ALL) NOPASSWD: /usr/sbin/groupadd" >> /etc/sudoers
#   clean apt
RUN rm -rf /var/lib/apt/lists/*
#   
RUN wget https://cmake.org/files/v3.11/cmake-3.11.1-Linux-x86_64.sh \
 && sh cmake-3.11.1-Linux-x86_64.sh --prefix=/usr/local --exclude-subdir
#   change user
USER ${USER}
#   install vscode extensions !!! AS USER !!!
RUN code --install-extension ms-vscode.cpptools \
 && code --install-extension twxs.cmake \
 && code --install-extension vector-of-bool.cmake-tools \
 && code --install-extension vadimcn.vscode-lldb \
 && code --install-extension bbenoist.doxygen \
 && code --install-extension cschlosser.doxdocgen \
 && code --install-extension ajshort.include-autocomplete \
 && code --install-extension reloadedextensions.reloaded-cpp \
 && code --install-extension reloadedextensions.reloaded-themes
ENV DEVEL_DIR="/home/${USER}/devel"
ENV SCRIPTS_DIR="/home/${USER}/scripts"
ENV DEPENDENCIES_DIR="${DEVEL_DIR}/dependencies"
ENV TERRALIB_DIR="${DEVEL_DIR}/terralib"
ENV TERRAMA2_DIR="${DEVEL_DIR}/terrama2"
RUN mkdir -p ${SCRIPTS_DIR}
#   Create sistem link clang-6.0 -> clang
COPY ./clang_update-alternative.sh ${SCRIPTS_DIR}
RUN cd ${SCRIPTS_DIR} \
 && ./clang_update-alternative.sh
COPY clang-tidy-diff.py ${SCRIPTS_DIR}
COPY pre-commit ${SCRIPTS_DIR}
RUN cd ${SCRIPTS_DIR} \
 && sed -i -e "s@${SCRIPTS_DIR}@${SCRIPTS_DIR}@g" pre-commit
COPY install-3rdparty-linux-ubuntu-16.04.sh ${SCRIPTS_DIR}
COPY prepare_terralib.sh ${SCRIPTS_DIR}
RUN cd ${SCRIPTS_DIR} \
 && sed -i -e "s@${DEPENDENCIES_DIR}@${DEPENDENCIES_DIR}@g" -e "s@${TERRALIB_DIR}@${TERRALIB_DIR}@g" -e "s@${SCRIPTS_DIR}@${SCRIPTS_DIR}@g" prepare_terralib.sh
COPY prepare_terrama2.sh ${SCRIPTS_DIR}
RUN cd ${SCRIPTS_DIR} \
 && sed -i -e "s@${DEPENDENCIES_DIR}@${DEPENDENCIES_DIR}@g" -e "s@${TERRALIB_DIR}@${TERRALIB_DIR}@g" -e "s@${TERRAMA2_DIR}@${TERRAMA2_DIR}@g" -e "s@${SCRIPTS_DIR}@${SCRIPTS_DIR}@g" prepare_terrama2.sh
WORKDIR /home/${USER}
COPY config_permission.sh .
CMD /home/terrama2/config_permission.sh ; sleep infinity
# Please add your HEALTHCHECK here!!!
