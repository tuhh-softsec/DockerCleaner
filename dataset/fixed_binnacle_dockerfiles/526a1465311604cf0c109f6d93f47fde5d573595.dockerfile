#   Distributed under the MIT License.
#   See LICENSE.txt for details.
#   If you change this file please push a new image to DockerHub so that the
#   new image is used for testing. Docker must be run as root on your machine,
#   so to build a new image run the following as root (e.g. sudo su):
#     cd $SPECTRE_HOME/containers
#     docker build  -t sxscollaboration/spectrebuildenv:latest \
#                   -f ./Dockerfile.buildenv .
#   and then to push to DockerHub:
#     docker push sxscollaboration/spectrebuildenv
#   If you do not have permission to push to DockerHub please coordinate with
#   someone who does. Since changes to this image effect our testing
#   infrastructure it is important all changes be carefully reviewed.
FROM ubuntu:18.04
ARG PARALLEL_MAKE_ARG=-j2
ARG DEBIAN_FRONTEND=noninteractive
#   Install required packages for SpECTRE
RUN apt-get update -y \
 && apt-get install --no-install-recommends gcc-6=6.5.0-2ubuntu1~18.04 g++-6=6.5.0-2ubuntu1~18.04 gfortran-6=6.5.0-2ubuntu1~18.04 gcc-7=7.5.0-3ubuntu1~18.04 g++-7=7.5.0-3ubuntu1~18.04 gfortran-7=7.5.0-3ubuntu1~18.04 gcc-8=8.4.0-1ubuntu1~18.04 g++-8=8.4.0-1ubuntu1~18.04 gfortran-8=8.4.0-1ubuntu1~18.04 gdb=8.1.1-0ubuntu1 git=1:2.17.1-1ubuntu0.17 cmake=3.10.2-1ubuntu2.18.04.2 libopenblas-dev=0.2.20+ds-4 liblapack-dev=3.7.1-4ubuntu1 libhdf5-dev=1.10.0-patch1+docs-4 hdf5-tools=1.10.0-patch1+docs-4 libgsl0-dev clang-5.0=1:5.0.1-4 clang-format-5.0=1:5.0.1-4 clang-tidy-5.0=1:5.0.1-4 libclang-5.0-dev=1:5.0.1-4 wget=1.19.4-1ubuntu2.2 libncurses-dev lcov=1.13-3 cppcheck=1.82-1 libboost-all-dev=1.65.1.0ubuntu1 libssl-dev=1.1.1-1ubuntu2.1~18.04.21 -y
#   Update is needed to get libc++ correctly
#   Install jemalloc
RUN apt-get update -y \
 && apt-get install --no-install-recommends libc++-dev=6.0-2 libc++1=6.0-2 libc++abi-dev=6.0-2 -y \
 && apt-get update -y \
 && apt-get install --no-install-recommends libjemalloc1=3.6.0-11 libjemalloc-dev=3.6.0-11 -y
#   Install ccache to cache compilations for reduced compile time, and Doxygen
RUN apt-get install --no-install-recommends ccache=3.4.1-1 doxygen=1.8.13-10 -y
#   Install Python packages
RUN apt-get install --no-install-recommends python-pip=9.0.1-2.3~ubuntu1.18.04.8 -y \
 && pip install autopep8==2.0.2 flake8==6.0.0 \
 && pip install numpy==1.24.2 scipy==1.10.1 \
 && pip install coverxygen==1.7.0 beautifulsoup4==4.12.2 pybtex==0.24.0
#   Add ruby gems and install coveralls using gem
RUN apt-get update -y \
 && apt-get install --no-install-recommends rubygems -y \
 && gem install coveralls-lcov --version 1.7.0
#   Enable bash-completion by installing it and then adding it to the .bashrc file
RUN apt-get update -y \
 && apt-get install --no-install-recommends bash-completion=1:2.8-1ubuntu1 -y \
 && printf "if [ -f /etc/bash_completion ] \
 && ! shopt -oq posix; then\n . /etc/bash_completion\nfi\n\n" >> /root/.bashrc
#   Install LMod which is needed by Spack and set it to load at login
RUN apt-get update -y \
 && apt-get install --no-install-recommends curl=7.58.0-2ubuntu3.24 lmod=6.6-0.2 -y \
 && printf '. /etc/profile.d/lmod.sh\n' >> /root/.bashrc \
 && . /etc/profile.d/lmod.sh
#   Install Spack to get remaining dependencies
WORKDIR /work
RUN git clone https://github.com/LLNL/spack.git
WORKDIR /work/spack
#   Since spack/develop is rather unstable, we check out a commit we
#   know is stable. This should be updated periodically to update
#   installed packages.
RUN git checkout 470a45c51659156e7d154ea890e798ce32b8767d
WORKDIR /work
#   Spack needs to be pointed to the system OpenSSL to work properly, we add this
#   in the general configure script for Spack rather than a user-specific
#   configure script. The below code is documented in the Spack manual.
RUN printf "\n openssl:\n paths:\n openssl@1.0.2g: /usr\n buildable: False\n" >> /work/spack/etc/spack/defaults/packages.yaml
#   Add Spack to PATH and install required dependencies
#   The sed commands are necessary because spack fails to find the
#   fortran compilers for an unknown reason. We compile the libraries with GCC6
#   so that we do not need separate versions for GCC6 and GCC7.
RUN echo "export PATH=$PATH:/work/spack/bin" >> /root/.bashrc \
 && echo '. /work/spack/share/spack/setup-env.sh' >> /root/.bashrc \
 && export PATH=$PATH:/work/spack/bin \
 && spack compiler find \
 && sed -i 's@fc: null@fc: /usr/bin/gfortran@' /root/.spack/linux/compilers.yaml \
 && sed -i 's@f77: null@f77: /usr/bin/gfortran@' /root/.spack/linux/compilers.yaml
RUN /work/spack/bin/spack install cmake \
 && /work/spack/bin/spack install --no-checksum catch@2.1.0 \
 && /work/spack/bin/spack install brigand@master \
 && /work/spack/bin/spack install blaze \
 && /work/spack/bin/spack install gsl%gcc@6.5.0 \
 && /work/spack/bin/spack install libsharp -openmp -mpi \
 && /work/spack/bin/spack install libxsmm%gcc@6.5.0 \
 && /work/spack/bin/spack install yaml-cpp@develop%gcc@6.5.0 \
 && /work/spack/bin/spack install benchmark%gcc@6.5.0
#   Install include-what-you-use
#   We patch it to allow cyclic includes in boost
RUN wget https://github.com/include-what-you-use/include-what-you-use/archive/clang_5.0.tar.gz \
 && tar -xzf clang_5.0.tar.gz \
 && rm clang_5.0.tar.gz \
 && mkdir /work/include-what-you-use-clang_5.0/build \
 && cd /work/include-what-you-use-clang_5.0/ \
 && sed -i 's^\\\"third_party/^<boost/^' iwyu_include_picker.cc \
 && cd /work/include-what-you-use-clang_5.0/build \
 && cmake -D CMAKE_CXX_COMPILER=clang++-5.0 -D CMAKE_C_COMPILER=clang-5.0 -D IWYU_LLVM_ROOT_PATH=/usr/lib/llvm-5.0 .. \
 && make -j2 \
 && make install \
 && cd /work \
 && rm -rf /work/include-what-you-use-clang_5.0
#   Download and build the Charm++ version used by SpECTRE
#   We build both Clang and GCC versions of Charm++ so that all our tests can
#   use the same build environment.
WORKDIR /work
ARG CHARM_GIT_TAG=v6.8.0
#   Charm doesn't support compiling with clang-5 without symbolic links
RUN ln -s $( which clang++-5.0 ;) /usr/local/bin/clang++ \
 && ln -s $( which clang-5.0 ;) /usr/local/bin/clang \
 && ln -s $( which clang-format-5.0 ;) /usr/local/bin/clang-format \
 && ln -s $( which clang-tidy-5.0 ;) /usr/local/bin/clang-tidy
RUN git clone https://charm.cs.illinois.edu/gerrit/charm \
 && cd /work/charm \
 && git checkout ${CHARM_GIT_TAG} \
 && ./build charm++ multicore-linux64 gcc ${PARALLEL_MAKE_ARG} -g -O0 \
 && ./build charm++ multicore-linux64 clang ${PARALLEL_MAKE_ARG} -g -O0 \
 && wget https://raw.githubusercontent.com/sxs-collaboration/spectre/develop/support/Charm/v6.8.patch \
 && git apply /work/charm/v6.8.patch \
 && rm /work/charm/v6.8.patch
WORKDIR /work
#   Load Spack dependencies at container load
RUN echo 'spack load catch' >> /root/.bashrc \
 && echo 'spack load brigand' >> /root/.bashrc \
 && echo 'spack load blaze' >> /root/.bashrc \
 && echo 'spack load gsl' >> /root/.bashrc \
 && echo 'spack load libsharp' >> /root/.bashrc \
 && echo 'spack load libxsmm' >> /root/.bashrc \
 && echo 'spack load yaml-cpp' >> /root/.bashrc \
 && echo 'spack load benchmark' >> /root/.bashrc
#   - Set the environment variable SPECTRE_CONTAINER so we can check if we are
#     inside a container (0 is true in bash)
#   - The singularity containers work better if the locale is set properly
ENV SPECTRE_CONTAINER="0"
RUN apt-get update \
 && apt-get install --no-install-recommends locales=2.27-3ubuntu1.6 language-pack-fi=1:18.04+20200702 language-pack-en=1:18.04+20190718 -y \
 && export LANGUAGE=en_US.UTF-8 \
 && export LANG=en_US.UTF-8 \
 && export LC_ALL=en_US.UTF-8 \
 && locale-gen en_US.UTF-8 \
 && dpkg-reconfigure locales
#   Install bibtex for Doxygen bibliography management
#   We first install the TeXLive infrastructure according to the configuration in
#   support/TeXLive/texlive.profile and then use it to install the bibtex package.
RUN mkdir /work/texlive
WORKDIR /work/texlive
RUN wget http://mirror.ctan.org/systems/texlive/tlnet/install-tl-unx.tar.gz \
 && tar -xzf install-tl-unx.tar.gz \
 && rm install-tl-unx.tar.gz \
 && wget https://raw.githubusercontent.com/sxs-collaboration/spectre/develop/support/TeXLive/texlive.profile \
 && install-tl-*/install-tl -profile=texlive.profile \
 && rm -r install-tl-* texlive.profile install-tl.log \
 && echo "export PATH=$PATH:/work/texlive/bin/x86_64-linux" >> /root/.bashrc \
 && /work/texlive/bin/x86_64-linux/tlmgr install bibtex
WORKDIR /work
#   Work around posix rename bug:
#     https://bugs.debian.org/cgi-bin/bugreport.cgi?bug=891541
RUN ln -s /usr/lib/x86_64-linux-gnu/lua/5.1/posix_c.so /usr/lib/x86_64-linux-gnu/lua/5.1/posix.so \
 && ln -s /usr/lib/x86_64-linux-gnu/lua/5.2/posix_c.so /usr/lib/x86_64-linux-gnu/lua/5.2/posix.so \
 && ln -s /usr/lib/x86_64-linux-gnu/lua/5.3/posix_c.so /usr/lib/x86_64-linux-gnu/lua/5.3/posix.so
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
