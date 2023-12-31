#  A image for building paddle binaries
#  Use cuda devel base image for both cpu and gpu environment
#  When you modify it, please be aware of cudnn-runtime version
FROM nvidia/cuda:8.0-cudnn7-devel-ubuntu16.04
MAINTAINER PaddlePaddle Authors <paddle-dev@baidu.com>
ARG UBUNTU_MIRROR
RUN /bin/bash -c 'if [[ -n ${UBUNTU_MIRROR} ]]; then sed -i 's#http://archive.ubuntu.com/ubuntu#${UBUNTU_MIRROR}#g' /etc/apt/sources.list; fi'
#  ENV variables
ARG WITH_GPU
ARG WITH_AVX
ENV WOBOQ="OFF"
ENV WITH_GPU="${WITH_GPU:-ON}"
ENV WITH_AVX="${WITH_AVX:-ON}"
ENV HOME="/root"
#  Add bash enhancements
COPY ./paddle/scripts/docker/root/ /root/
#  Prepare packages for Python
RUN apt-get update \
 && apt-get install --no-install-recommends make build-essential libssl-dev zlib1g-dev libbz2-dev libreadline-dev libsqlite3-dev wget curl llvm libncurses5-dev libncursesw5-dev xz-utils tk-dev libffi-dev liblzma-dev -y
#  Install Python3.6
RUN mkdir -p /root/python_build/ \
 && wget -q https://www.sqlite.org/2018/sqlite-autoconf-3250300.tar.gz \
 && tar -zxf sqlite-autoconf-3250300.tar.gz \
 && cd sqlite-autoconf-3250300 \
 && ./configure -prefix=/usr/local \
 && make -j8 \
 && make install \
 && cd ../ \
 && rm sqlite-autoconf-3250300.tar.gz \
 && wget -q https://www.python.org/ftp/python/3.6.0/Python-3.6.0.tgz \
 && tar -xzf Python-3.6.0.tgz \
 && cd Python-3.6.0 \
 && CFLAGS="-Wformat" ./configure --prefix=/usr/local/ --enable-shared > /dev/null \
 && make -j8 > /dev/null \
 && make altinstall > /dev/null
#  Install Python3.7
RUN wget -q https://www.python.org/ftp/python/3.7.0/Python-3.7.0.tgz \
 && tar -xzf Python-3.7.0.tgz \
 && cd Python-3.7.0 \
 && CFLAGS="-Wformat" ./configure --prefix=/usr/local/ --enable-shared > /dev/null \
 && make -j8 > /dev/null \
 && make altinstall > /dev/null
RUN rm -r /root/python_build
RUN apt-get update \
 && apt-get install --no-install-recommends patchelf python3 python3-dev python3-pip git python-pip python-dev python-opencv openssh-server bison libnccl2=2.1.2-1+cuda8.0 libnccl-dev=2.1.2-1+cuda8.0 wget unzip unrar tar xz-utils bzip2 gzip coreutils ntp curl sed grep graphviz libjpeg-dev zlib1g-dev python-matplotlib gcc-4.8 g++-4.8 automake locales clang-format swig cmake liblapack-dev liblapacke-dev clang-3.8 llvm-3.8 libclang-3.8-dev net-tools libtool ccache -y --allow-downgrades \
 && apt-get clean -y
#  Install Go and glide
RUN wget -qO- https://storage.googleapis.com/golang/go1.8.1.linux-amd64.tar.gz | tar -xz -C /usr/local \
 && mkdir /root/gopath \
 && mkdir /root/gopath/bin \
 && mkdir /root/gopath/src
ENV GOROOT="/usr/local/go" \
    GOPATH="/root/gopath"
#  should not be in the same line with GOROOT definition, otherwise docker build could not find GOROOT.
ENV PATH="${PATH}:${GOROOT}/bin:${GOPATH}/bin"
#  install glide
RUN curl -s -q https://glide.sh/get | sh
#  Install TensorRT
#  following TensorRT.tar.gz is not the default official one, we do two miny changes:
#  1. Remove the unnecessary files to make the library small. TensorRT.tar.gz only contains include and lib now,
#     and its size is only one-third of the official one.
#  2. Manually add ~IPluginFactory() in IPluginFactory class of NvInfer.h, otherwise, it couldn't work in paddle.
#     See https://github.com/PaddlePaddle/Paddle/issues/10129 for details.
RUN wget -q https://paddlepaddledeps.bj.bcebos.com/TensorRT-4.0.1.6-ubuntu14.04.x86_64-gnu.cuda.8.0.cudnn7.0.tar.gz --no-check-certificate \
 && tar -zxf TensorRT-4.0.1.6-ubuntu14.04.x86_64-gnu.cuda.8.0.cudnn7.0.tar.gz -C /usr/local \
 && cp -rf /usr/local/TensorRT/include /usr \
 && cp -rf /usr/local/TensorRT/lib /usr
#  git credential to skip password typing
RUN git config --global credential.helper store
#  Fix locales to en_US.UTF-8
RUN localedef -i en_US -f UTF-8 en_US.UTF-8
#  FIXME: due to temporary ipykernel dependency issue, specify ipykernel jupyter
#  version util jupyter fixes this issue.
#  specify sphinx version as 1.5.6 and remove -U option for [pip install -U
#  sphinx-rtd-theme] since -U option will cause sphinx being updated to newest
#  version(1.7.1 for now), which causes building documentation failed.
RUN pip3 --no-cache-dir install -U wheel py-cpuinfo==5.0.0 \
 && pip3 --no-cache-dir install -U docopt PyYAML sphinx==1.5.6 \
 && pip3 --no-cache-dir install sphinx-rtd-theme==0.1.9 recommonmark \
 && pip3.6 --no-cache-dir install -U wheel py-cpuinfo==5.0.0 \
 && pip3.6 --no-cache-dir install -U docopt PyYAML sphinx==1.5.6 \
 && pip3.6 --no-cache-dir install sphinx-rtd-theme==0.1.9 recommonmark \
 && pip3.7 --no-cache-dir install -U wheel py-cpuinfo==5.0.0 \
 && pip3.7 --no-cache-dir install -U docopt PyYAML sphinx==1.5.6 \
 && pip3.7 --no-cache-dir install sphinx-rtd-theme==0.1.9 recommonmark \
 && easy_install -U pip \
 && pip install pip setuptools wheel py-cpuinfo==5.0.0 --no-cache-dir -U \
 && pip install docopt PyYAML sphinx==1.5.6 --no-cache-dir -U \
 && pip install sphinx-rtd-theme==0.1.9 recommonmark --no-cache-dir
RUN pip3 --no-cache-dir install 'pre-commit==1.10.4' 'ipython==5.3.0' \
 && pip3 --no-cache-dir install 'ipykernel==4.6.0' 'jupyter==1.0.0' \
 && pip3 --no-cache-dir install opencv-python \
 && pip3.6 --no-cache-dir install 'pre-commit==1.10.4' 'ipython==5.3.0' \
 && pip3.6 --no-cache-dir install 'ipykernel==4.6.0' 'jupyter==1.0.0' \
 && pip3.6 --no-cache-dir install opencv-python \
 && pip3.7 --no-cache-dir install 'pre-commit==1.10.4' 'ipython==5.3.0' \
 && pip3.7 --no-cache-dir install 'ipykernel==4.6.0' 'jupyter==1.0.0' \
 && pip3.7 --no-cache-dir install opencv-python \
 && pip install 'pre-commit==1.10.4' 'ipython==5.3.0' --no-cache-dir \
 && pip install 'ipykernel==4.6.0' 'jupyter==1.0.0' --no-cache-dir \
 && pip install opencv-python --no-cache-dir
# For docstring checker
RUN pip3 --no-cache-dir install pylint pytest astroid isort
RUN pip3.6 --no-cache-dir install pylint pytest astroid isort
RUN pip3.7 --no-cache-dir install pylint pytest astroid isort
RUN pip install pylint pytest astroid isort LinkChecker --no-cache-dir
COPY ./python/requirements.txt /root/
RUN pip3 --no-cache-dir install -r /root/requirements.txt
RUN pip3.6 --no-cache-dir install -r /root/requirements.txt
RUN pip3.7 --no-cache-dir install -r /root/requirements.txt
RUN pip install --no-cache-dir -r /root/requirements.txt
#  To fix https://github.com/PaddlePaddle/Paddle/issues/1954, we use
#  the solution in https://urllib3.readthedocs.io/en/latest/user-guide.html#ssl-py2
RUN apt-get install --no-install-recommends libssl-dev libffi-dev -y \
 && apt-get clean -y
RUN pip3 --no-cache-dir install certifi urllib3[secure]
RUN pip3.6 --no-cache-dir install certifi urllib3[secure]
RUN pip3.7 --no-cache-dir install certifi urllib3[secure]
RUN pip install certifi urllib3[secure] --no-cache-dir
#  Install woboq_codebrowser to /woboq
RUN git clone https://github.com/woboq/woboq_codebrowser /woboq \
 && (cd /woboq cmake -DLLVM_CONFIG_EXECUTABLE=/usr/bin/llvm-config-3.8 -DCMAKE_BUILD_TYPE=Release . make )
#  ar mishandles 4GB files
#  https://sourceware.org/bugzilla/show_bug.cgi?id=14625
#  remove them when apt-get support 2.27 and higher version
RUN wget -q https://launchpad.net/ubuntu/+archive/primary/+sourcefiles/binutils/2.27-9ubuntu1/binutils_2.27.orig.tar.gz \
 && tar -xzf binutils_2.27.orig.tar.gz \
 && cd binutils-2.27 \
 && ./configure \
 && make -j \
 && make install \
 && cd .. \
 && rm -rf binutils-2.27 binutils_2.27.orig.tar.gz
#  Configure OpenSSH server. c.f. https://docs.docker.com/engine/examples/running_ssh_service
RUN mkdir /var/run/sshd
RUN echo 'root:root' | chpasswd
RUN sed -ri 's/^PermitRootLogin\s+.*/PermitRootLogin yes/' /etc/ssh/sshd_config
RUN sed -ri 's/UsePAM yes/#UsePAM yes/g' /etc/ssh/sshd_config
EXPOSE 22/tcp
