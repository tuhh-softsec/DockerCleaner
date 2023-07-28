MAINTAINER Mario Cho "m.cho@lablup.com"
FROM ubuntu:18.04
RUN apt-get update \
 && apt-get install --no-install-recommends ca-certificates=20211016ubuntu0.18.04.1 wget=1.19.4-1ubuntu2.2 curl=7.58.0-2ubuntu3.24 git-core python3=3.6.7-1~18.04 python3-pip=9.0.1-2.3~ubuntu1.18.04.8 libssl-dev=1.1.1-1ubuntu2.1~18.04.21 libmpdec2=2.4.2-1ubuntu1 proj-bin=4.9.3-2 libproj-dev=4.9.3-2 libgeos-dev=3.6.2-1build2 libgeos++-dev=3.6.2-1build2 mime-support=3.60ubuntu1 gnupg=2.2.4-1ubuntu1.6 dirmngr=2.2.4-1ubuntu1.6 gcc=4:7.4.0-1ubuntu2.3 g++=4:7.4.0-1ubuntu2.3 -y \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/
ENV PATH="/opt/backend.ai/bin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin" \
    LANG="C.UTF-8" \
    DEBIAN_FRONTEND="noninteractive"
#   Install commonly-used wheels
RUN curl https://bootstrap.pypa.io/get-pip.py | python3 \
 && python3 -m pip install --no-cache-dir -U setuptools \
 && python3 -m pip install --no-cache-dir pyzmq \
 && python3 -m pip install --no-cache-dir ipython \
 && python3 -m pip install --no-cache-dir jupyter \
 && python3 -m pip install --no-cache-dir jupyterlab
#   Install jupyter & Julia
ARG JULIA_VERSION
ENV JULIA_VERSION="${JULIA_VERSION:-1.0.4}"
ENV JULIA_PATH="\"/usr/local/julia\""
ENV PATH="$JULIA_PATH:$JULIA_PATH/bin:$PATH"
RUN apt-get update -y \
 && apt-get install --no-install-recommends ca-certificates=20211016ubuntu0.18.04.1 curl=7.58.0-2ubuntu3.24 -y \
 && rm -rf /var/lib/apt/lists/*
RUN dpkgArch="$( dpkg --print-architecture ;)" ; case "${dpkgArch##*-}" in (amd64) tarArch='x86_64' ; dirArch='x64' ;;(armhf) tarArch='arm' ; dirArch='arm' ;;(i386) tarArch='i686' ; dirArch='x86' ;;(*) echo "error: current architecture ($dpkgArch) does not have a corresponding Julia binary release" >&2; exit 1 ;; esac ; curl -fL -o julia.tar.gz "https://julialang-s3.julialang.org/bin/linux/${dirArch}/${JULIA_VERSION%[.-]*}/julia-${JULIA_VERSION}-linux-${tarArch}.tar.gz" ; mkdir "$JULIA_PATH" ; tar -xzf julia.tar.gz -C "$JULIA_PATH" --strip-components 1 ; rm julia.tar.gz
#  add IJulia package to make Julia kernel available in jupyter
RUN julia -e 'ENV["JUPYTER"]="jupyter"' \
 && julia -e 'ENV["PYTHON"]="python3"' \
 && julia -e 'using Pkg; Pkg.add("IJulia");' \
 && cp -r /root/.local/share/jupyter/kernels/julia-1.0 /usr/local/share/jupyter/kernels \
 && rm -rf /root/.local/share/jupyter
ENV JULIA_PKG_LOC="/opt/julia"
ENV JULIA_LOAD_PATH=":/opt/julia"
SHELL ["/bin/bash", "-c"]
RUN mkdir ${JULIA_PKG_LOC} \
 && mv /root/.julia/packages/* ${JULIA_PKG_LOC} \
 && for d in ${JULIA_PKG_LOC}/*; do sub=$d/$( ls $d ;) ;shopt -s dotglob ;mv $sub/* $d ;rm -rf $sub ;shopt -u dotglob ; done \
 && rm -rf /root/.julia/* \
 && rm -rf /opt/julia/Conda/deps/deps.jl \
 && echo 'const ROOTENV = "/tmp"' >> /opt/julia/Conda/deps/deps.jl \
 && echo 'const MINICONDA_VERSION = "3"' >> /opt/julia/Conda/deps/deps.jl \
 && rm -rf /usr/local/share/jupyter/kernels/julia-1.0/kernel.json \
 && julia -e 'using IJulia' \
 && mv /root/.julia/compiled /opt/julia \
 && chmod -R 755 /opt/julia/compiled
SHELL ["/bin/sh", "-c"]
COPY kernel.json /usr/local/share/jupyter/kernels/julia-1.0
COPY movecompiled.sh /usr/local/bin/movecompiled.sh
RUN chmod 755 /usr/local/bin/movecompiled.sh
LABEL ai.backend.kernelspec="1" \
      ai.backend.envs.corecount="OPENBLAS_NUM_THREADS,OMP_NUM_THREADS,NPROC" \
      ai.backend.features="batch query uid-match user-input" \
      ai.backend.resource.min.cpu="1" \
      ai.backend.resource.min.mem="256m" \
      ai.backend.base-distro="ubuntu16.04" \
      ai.backend.runtime-type="julia" \
      ai.backend.runtime-path="/usr/bin/python3" \
      ai.backend.service-ports="jupyter:http:8080,jupyterlab:http:8090"
COPY policy.yml /etc/backend.ai/jail/policy.yml
#   vim: ft=dockerfile
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
