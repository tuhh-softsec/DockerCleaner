#  ##############################################################################
#   Copyright (c) 2015-2019, Lawrence Livermore National Security, LLC.
#
#   Produced at the Lawrence Livermore National Laboratory
#
#   LLNL-CODE-716457
#
#   All rights reserved.
#
#   This file is part of Ascent.
#
#   For details, see: http://ascent.readthedocs.io/.
#
#   Please also read ascent/LICENSE
#
#   Redistribution and use in source and binary forms, with or without
#   modification, are permitted provided that the following conditions are met:
#
#   * Redistributions of source code must retain the above copyright notice,
#     this list of conditions and the disclaimer below.
#
#   * Redistributions in binary form must reproduce the above copyright notice,
#     this list of conditions and the disclaimer (as noted below) in the
#     documentation and/or other materials provided with the distribution.
#
#   * Neither the name of the LLNS/LLNL nor the names of its contributors may
#     be used to endorse or promote products derived from this software without
#     specific prior written permission.
#
#   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
#   AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
#   IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
#   ARE DISCLAIMED. IN NO EVENT SHALL LAWRENCE LIVERMORE NATIONAL SECURITY,
#   LLC, THE U.S. DEPARTMENT OF ENERGY OR CONTRIBUTORS BE LIABLE FOR ANY
#   DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
#   DAMAGES  (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
#   OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
#   HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
#   STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING
#   IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
#   POSSIBILITY OF SUCH DAMAGE.
#
#  ##############################################################################
FROM ubuntu:latest
MAINTAINER Cyrus Harrison <cyrush@llnl.gov>
#   fetch build env
RUN apt-get update \
 && apt-get install --no-install-recommends git=1:2.39.2-1ubuntu1 curl=7.88.1-7ubuntu1 build-essential=12.9ubuntu3 gcc=4:12.2.0-3ubuntu1 g++=4:12.2.0-3ubuntu1 gfortran=4:12.2.0-3ubuntu1 zlib1g-dev=1:1.2.13.dfsg-1ubuntu4 python unzip=6.0-27ubuntu1 vim=2:9.0.1000-4ubuntu2 emacs=1:28.2+1-13ubuntu3 -y \
 && rm -rf /var/lib/apt/lists/*
#   obtain a copy of ascent source from host env
COPY ascent.docker.src.tar.gz /
RUN tar -xzf ascent.docker.src.tar.gz
#   at some sites ssl certs are intercepted, which cases issues fetching
#   tpl sources via https
#   to resolve this, either:
#   1) pass the "-k" option to uberenv (recommended),
#   2) install the proper certs into the image, or
#   3) use  the following commands to disable ssl for git and
#      curl (both are used by spack):
#  RUN git config --global http.sslVerify false
#  RUN echo insecure >> ~/.curlrc
#   bootstrap third party libs using spack and uberenv
#   for this example we use mpich for MPI b/c openmpi's mpiexec
#   will not run for the root user, also avoid mpi4py after 2.0,
#   since it reqs cython and cython's src tarball has perms set
#   in a way that fails in docker
RUN cd ascent \
 && python scripts/uberenv/uberenv.py -k --spec "%gcc+mpi~doc~adios ^mpich"
#   configure a debug build with cmake
RUN cd ascent \
 && mkdir build-debug
RUN cd ascent/build-debug \
 && ../uberenv_libs/spack/opt/spack/*/*/cmake*/bin/cmake -DCMAKE_BUILD_TYPE=Debug -DCMAKE_INSTALL_PREFIX=/ascent/install-debug -C ../uberenv_libs/*.cmake ../src
#   build, test, and install conduit
RUN cd ascent/build-debug \
 && make
RUN cd ascent/build-debug \
 && env CTEST_OUTPUT_ON_FAILURE=1 make test
RUN cd ascent/build-debug \
 && make install
#   gen env script that points to spack installs of tpls
RUN cd ascent \
 && python scripts/gen_spack_env_script.py cmake mpi python
RUN cp ascent/s_env.sh ascent_docker_setup_env.sh
#   open port 9000, for use by conduit relay
EXPOSE 9000/tcp
#   open port 10000 for other apps (browse fs via http, etc)
EXPOSE 10000/tcp
CMD ["/bin/bash"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
