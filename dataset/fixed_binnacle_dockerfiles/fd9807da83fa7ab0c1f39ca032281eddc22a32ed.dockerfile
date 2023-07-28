#   Apple Swfit 5.0 for Backend.ai
FROM ubuntu:18.04
MAINTAINER Mario Cho "m.cho@lablup.com"
RUN export DEBIAN_FRONTEND=noninteractive DEBCONF_NONINTERACTIVE_SEEN=true \
 && apt-get update -q -y \
 && apt-get install --no-install-recommends python3=3.6.7-1~18.04 python3-pip=9.0.1-2.3~ubuntu1.18.04.8 libssl-dev=1.1.1-1ubuntu2.1~18.04.21 libmpdec2=2.4.2-1ubuntu1 proj-bin=4.9.3-2 libproj-dev=4.9.3-2 libgeos-dev=3.6.2-1build2 libgeos++-dev=3.6.2-1build2 libatomic1=8.4.0-1ubuntu1~18.04 libbsd0=0.8.7-1ubuntu0.1 libcurl4=7.58.0-2ubuntu3.24 libxml2=2.9.4+dfsg1-6.1ubuntu1.8 libedit2=3.1-20170329-1 libsqlite3-0=3.22.0-1ubuntu0.7 libc6-dev=2.27-3ubuntu1.6 binutils=2.30-21ubuntu1~18.04.8 libgcc-5-dev=5.5.0-12ubuntu1 libstdc++-5-dev=5.5.0-12ubuntu1 libpython2.7=2.7.17-1~18.04ubuntu1.11 tzdata=2022g-0ubuntu0.18.04 git=1:2.17.1-1ubuntu0.17 pkg-config=0.29.1-0ubuntu2 -q -y \
 && rm -r /var/lib/apt/lists/*
ENV PYTHONUNBUFFERED="1" \
    PATH="/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin" \
    LANG="C.UTF-8"
ARG SWIFT_PLATFORM=ubuntu18.04
ARG SWIFT_BRANCH=swift-5.0.1-release
ARG SWIFT_VERSION=swift-5.0.1-RELEASE
ENV SWIFT_PLATFORM="$SWIFT_PLATFORM" \
    SWIFT_BRANCH="$SWIFT_BRANCH" \
    SWIFT_VERSION="$SWIFT_VERSION"
RUN SWIFT_URL=https://swift.org/builds/$SWIFT_BRANCH/$( echo "$SWIFT_PLATFORM" | tr -d . ;)/$SWIFT_VERSION/$SWIFT_VERSION-$SWIFT_PLATFORM.tar.gz \
 && apt-get update \
 && apt-get install --no-install-recommends curl=7.58.0-2ubuntu3.24 -y \
 && curl -fSsL $SWIFT_URL -o swift.tar.gz \
 && curl -fSsL $SWIFT_URL.sig -o swift.tar.gz.sig \
 && export GNUPGHOME="$( mktemp -d ;)" \
 && set -e ; for key in A62AE125BBBFBB96A6E042EC925CC1CCED3D1561; do gpg --quiet --keyserver ha.pool.sks-keyservers.net --recv-keys "$key" ; done \
 && gpg --batch --verify --quiet swift.tar.gz.sig swift.tar.gz \
 && tar -xzf swift.tar.gz --directory / --strip-components=1 \
 && rm -r "$GNUPGHOME" swift.tar.gz.sig swift.tar.gz \
 && chmod -R o+r /usr/lib/swift
RUN curl https://bootstrap.pypa.io/get-pip.py | python3 \
 && python3 -m pip install --no-cache-dir -U setuptools \
 && python3 -m pip install --no-cache-dir wheel \
 && python3 -m pip install --no-cache-dir h5py \
 && python3 -m pip install --no-cache-dir Cython \
 && python3 -m pip install --no-cache-dir numpy scipy \
 && python3 -m pip install --no-cache-dir versioneer==0.17 \
 && python3 -m pip install --no-cache-dir pyproj Cartopy==0.16 \
 && python3 -m pip install --no-cache-dir matplotlib bokeh \
 && python3 -m pip install --no-cache-dir ipython \
 && python3 -m pip install --no-cache-dir jupyter \
 && python3 -m pip install --no-cache-dir jupyterlab \
 && apt-get purge -y curl \
 && apt-get -y autoremove \
 && rm -rf /root/.cache \
 && rm -f /tmp/*.whl \
 && swift --version
RUN update-alternatives --install /usr/bin/python python /usr/bin/python3 2
#   Install ipython kernelspec
RUN python -m ipykernel install --display-name "Swift 5.0" \
 && cat /usr/local/share/jupyter/kernels/python3/kernel.json
#   Install Jupyter notebook logo
RUN mkdir -p /home/work/.jupyter/custom
COPY custom.css /home/work/.jupyter/custom/custom.css
COPY logo.svg /home/work/.jupyter/custom/logo.svg
#   Backend.AI specifics
LABEL ai.backend.kernelspec="1" \
      ai.backend.envs.corecount="OPENBLAS_NUM_THREADS,OMP_NUM_THREADS,NPROC" \
      ai.backend.features="batch query uid-match user-input" \
      ai.backend.base-distro="ubuntu16.04" \
      ai.backend.resource.min.cpu="1" \
      ai.backend.resource.min.mem="1g" \
      ai.backend.runtime-type="python" \
      ai.backend.runtime-path="/usr/bin/swift" \
      ai.backend.service-ports="ipython:pty:3000,jupyter:http:8080,jupyterlab:http:8090"
COPY policy.yml /etc/backend.ai/jail/policy.yml
#   vim: ft=dockerfile
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
