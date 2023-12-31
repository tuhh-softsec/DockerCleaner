FROM ubuntu:xenial
#  This DockerFile is looked after by
MAINTAINER Tim Greaves <tim.greaves@imperial.ac.uk>
#  Add the ubuntu-toolchain-r test ppa
RUN echo "deb http://ppa.launchpad.net/ubuntu-toolchain-r/test/ubuntu xenial main" > /etc/apt/sources.list.d/ubuntu-toolchain-r-ppa-xenial.list
#  Import the Launchpad PPA public key
RUN gpg --keyserver keyserver.ubuntu.com --recv 1E9377A2BA9EF27F
RUN gpg --export --armor BA9EF27F | apt-key add -
#  Ensure UTF-8 environment is correct for documentation builds
ENV LC_ALL="C.UTF-8"
ENV LANG="C.UTF-8"
#  Use bash, not dash, or later conditionals fail
RUN echo "dash dash/sh boolean false" | debconf-set-selections
RUN DEBIAN_FRONTEND=noninteractive dpkg-reconfigure dash
#  Upgrade to the most recent package set
RUN apt-get update
RUN apt-get -y dist-upgrade
#  Needed for the conda and devito installs later
RUN apt-get install --no-install-recommends wget bzip2 git make -y
#  Default gcc version to install
ARG gccVersion=8
ENV DEVITO_ARCH="gcc-$gccVersion"
#  Set Yask variables in case they are needed
ENV YC_CXX="g++-$gccVersion"
#  Default devito install method
ARG installWithPip=false
ENV testWithPip="$installWithPip"
#  Use a different back-end?
ARG DEVITO_BACKEND=none
ENV DEVITO_BACKEND="$DEVITO_BACKEND"
#  Use OpenMP?
ARG DEVITO_OPENMP=0
ENV DEVITO_OPENMP="$DEVITO_OPENMP"
ENV OMP_NUM_THREADS="2"
#  Use MPI?
ARG MPI_INSTALL=1
ENV MPI_INSTALL="$MPI_INSTALL"
#  Run examples?
ARG RUN_EXAMPLES='false'
ENV RUN_EXAMPLES="$RUN_EXAMPLES"
#  Ensure a non-interactive matplotlib backend
ENV MPLBACKEND="agg"
#  Install gcc/g++
RUN apt-get install --no-install-recommends gcc-$gccVersion g++-$gccVersion -y
#  Set up alternatives
RUN update-alternatives --install /usr/bin/gcc gcc /usr/bin/gcc-$gccVersion 10
RUN update-alternatives --install /usr/bin/g++ g++ /usr/bin/g++-$gccVersion 10
RUN update-alternatives --install /usr/bin/gcov gcov /usr/bin/gcov-$gccVersion 10
RUN update-alternatives --install /usr/bin/nm nm /usr/bin/gcc-nm-$gccVersion 10
RUN update-alternatives --install /usr/bin/cpp cpp /usr/bin/cpp-$gccVersion 10
RUN update-alternatives --install /usr/bin/ranlib ranlib /usr/bin/gcc-ranlib-$gccVersion 10
RUN if [ -f /usr/bin/gcov-dump-$gccVersion ] ; then update-alternatives --install /usr/bin/gcov-dump gcov-dump /usr/bin/gcov-dump-$gccVersion 10 ; fi
RUN if [ -f /usr/bin/gcov-tool-$gccVersion ] ; then update-alternatives --install /usr/bin/gcov-tool gcov-tool /usr/bin/gcov-tool-$gccVersion 10 ; fi
#  Install MPI
RUN if [ "$MPI_INSTALL" == "1" ] ; then \
apt-get install --no-install-recommends mpich libmpich-dev -y -q; fi
#  Set up conda
RUN wget --no-verbose https://repo.continuum.io/miniconda/Miniconda3-4.5.4-Linux-x86_64.sh -O /tmp/miniconda.sh
RUN bash /tmp/miniconda.sh -b -p /usr/local/miniconda
ENV PATH="/usr/local/miniconda/bin:$PATH"
RUN conda config --set always_yes yes --set changeps1 no
RUN conda update -q conda
RUN adduser --disabled-password --gecos "" devito
COPY . /home/devito
RUN chown -R devito /home/devito
USER devito
WORKDIR /home/devito
RUN if [ $installWithPip == "true" ] ; then python setup.py install --user ;if [ $MPI_INSTALL == '1' ] ; then pip install --user -e .[extras] ; fi ; else conda env create -q -f environment.yml ;source activate devito ;echo $PATH ;pip install -e . ;if [ $MPI_INSTALL == '1' ] ; then pip install -r requirements-optional.txt ; fi ;pip install pytest-xdist ;if [ $DEVITO_BACKEND == "yask" ] ; then conda install swig ;git clone https://github.com/opesci/yask.git /tmp/yask ;pushd /tmp/yask ;make compiler-api ;pip install -e . ;popd ; fi ;if [ $RUN_EXAMPLES == "true" ] ; then bash scripts/create_ipyparallel_mpi_profile.sh ; fi ; fi
ENTRYPOINT /home/devito/azure-pipelines.py
