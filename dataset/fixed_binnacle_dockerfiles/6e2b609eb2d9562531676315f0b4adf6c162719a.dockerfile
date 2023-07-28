FROM ubuntu:16.04
MAINTAINER K-Lab Authors <service@kesci.com>
USER root
#   Configure environment
ENV CONDA_DIR="/opt/conda"
ENV PATH="$CONDA_DIR/bin:$PATH"
ENV SHELL="/bin/bash"
ENV NB_USER="kesci"
ENV NB_UID="1000"
ENV HOME="/home/$NB_USER"
ENV LC_ALL="en_US.UTF-8"
ENV LANG="en_US.UTF-8"
ENV LANGUAGE="en_US.UTF-8"
#   Install prerequisites
RUN apt-get update \
 && apt-get -yqq dist-upgrade \
 && apt-get install --no-install-recommends build-essential=12.1ubuntu2 cmake=3.5.1-1ubuntu3 bzip2=1.0.6-8ubuntu0.2 ca-certificates=20210119~16.04.1 iptables=1.6.0-2ubuntu3 locales=2.23-0ubuntu11.3 sudo=1.8.16-0ubuntu1.10 wget=1.17.1-1ubuntu1.5 strace=4.11-1ubuntu3 build-essential=12.1ubuntu2 emacs=46.1 git=1:2.7.4-0ubuntu1.10 inkscape=0.91-7ubuntu2 jed=1:0.99.19-4 libsm6=2:1.2.2-1 libxrender1=1:0.9.9-0ubuntu1 pandoc=1.16.0.2~dfsg-1 python-dev=2.7.12-1~16.04 texlive-fonts-extra=2015.20160320-1 texlive-fonts-recommended=2015.20160320-1ubuntu0.1 texlive-generic-recommended=2015.20160320-1ubuntu0.1 texlive-latex-base=2015.20160320-1ubuntu0.1 texlive-latex-extra=2015.20160320-1 unzip=6.0-20ubuntu1.1 vim=2:7.4.1689-3ubuntu1.5 -yqq \
 && echo "en_US.UTF-8 UTF-8" > /etc/locale.gen \
 && locale-gen \
 && useradd -m -s /bin/bash -N -u $NB_UID $NB_USER \
 && mkdir -p $CONDA_DIR \
 && chown $NB_USER $CONDA_DIR \
 && apt-get install --no-install-recommends libfontconfig1=2.11.94-0ubuntu1.1 libfreetype6-dev=2.6.1-0.1ubuntu2.5 libglib2.0-0=2.48.2-0ubuntu4.8 libsm6=2:1.2.2-1 libxext6=2:1.3.3-1 libxrender1=1:0.9.9-0ubuntu1 liblcms2-dev=2.6-3ubuntu2.1 libwebp-dev=0.4.4-1 zlib1g-dev=1:1.2.8.dfsg-2ubuntu4.3 ca-certificates=20210119~16.04.1 fonts-dejavu=2.35-1 gcc=4:5.3.1-1ubuntu1 gfortran=4:5.3.1-1ubuntu1 graphviz=2.38.0-12ubuntu2.1 libxml2-dev=2.9.3+dfsg1-1ubuntu0.7 xz-utils=5.1.1alpha+20120614-2ubuntu2 libudunits2-dev=2.2.20-1 default-jdk=2:1.8-56ubuntu2 -yqq \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/* \
 && echo "deb https://mirrors.tuna.tsinghua.edu.cn/ubuntu/ bionic main restricted universe multiverse\ndeb-src https://mirrors.tuna.tsinghua.edu.cn/ubuntu/ bionic main restricted universe multiverse\ndeb https://mirrors.tuna.tsinghua.edu.cn/ubuntu/ bionic-updates main restricted universe multiverse\ndeb-src https://mirrors.tuna.tsinghua.edu.cn/ubuntu/ bionic-updates main restricted universe multiverse\ndeb https://mirrors.tuna.tsinghua.edu.cn/ubuntu/ bionic-backports main restricted universe multiverse\ndeb-src https://mirrors.tuna.tsinghua.edu.cn/ubuntu/ bionic-backports main restricted universe multiverse\ndeb https://mirrors.tuna.tsinghua.edu.cn/ubuntu/ bionic-security main restricted universe multiverse\ndeb-src https://mirrors.tuna.tsinghua.edu.cn/ubuntu/ bionic-security main restricted universe multiverse\n" > /etc/apt/source.list
#   Setup kesci home directory and install conda
RUN su -m -l $NB_USER -c ' mkdir /home/$NB_USER/work \
 && mkdir /home/$NB_USER/input \
 && mkdir /home/$NB_USER/.jupyter \
 && echo "cacert=/etc/ssl/certs/ca-certificates.crt" > /home/$NB_USER/.curlrc \
 && cd /tmp \
 && mkdir -p $CONDA_DIR \
 && wget https://repo.continuum.io/miniconda/Miniconda3-4.4.10-Linux-x86_64.sh -O Miniconda3-latest-Linux-x86_64.sh \
 && sha256sum Miniconda3-latest-Linux-x86_64.sh \
 && /bin/bash Miniconda3-latest-Linux-x86_64.sh -f -b -p $CONDA_DIR \
 && rm Miniconda3-latest-Linux-x86_64.sh \
 && $CONDA_DIR/bin/conda --version \
 && $CONDA_DIR/bin/conda config --set show_channel_urls yes \
 && $CONDA_DIR/bin/conda config --system --set auto_update_conda false \
 && $CONDA_DIR/bin/conda clean -tipsy ' \
 && echo "jpeg 9*" >> /opt/conda/conda-meta/pinned
USER root
RUN cd /tmp \
 && wget https://bootstrap.pypa.io/get-pip.py \
 && python2 get-pip.py \
 && rm get-pip.py \
 && python2 -m pip install ipykernel \
 && python2 -m ipykernel install \
 && chown $NB_USER /usr/local/bin \
 && chown $NB_USER /usr/local/share \
 && chown -R $NB_USER /usr/local/lib/python2.7
#   Julia dependencies
#   install Julia packages in /opt/julia instead of $HOME
ENV JULIA_DEPOT_PATH="/opt/julia"
ENV JULIA_PKGDIR="/opt/julia"
ENV JULIA_VERSION="1.0.0"
RUN mkdir /opt/julia-${JULIA_VERSION} \
 && cd /tmp \
 && wget -q https://julialang-s3.julialang.org/bin/linux/x64/`echo ${JULIA_VERSION} | cut -d. -f 1,2 `/julia-${JULIA_VERSION}-linux-x86_64.tar.gz \
 && echo "bea4570d7358016d8ed29d2c15787dbefaea3e746c570763e7ad6040f17831f3 *julia-${JULIA_VERSION}-linux-x86_64.tar.gz" | sha256sum -c - \
 && tar xzf julia-${JULIA_VERSION}-linux-x86_64.tar.gz -C /opt/julia-${JULIA_VERSION} --strip-components=1 \
 && rm /tmp/julia-${JULIA_VERSION}-linux-x86_64.tar.gz
RUN ln -fs /opt/julia-*/bin/julia /usr/local/bin/julia
#   Show Julia where conda libraries are \
RUN mkdir /etc/julia \
 && echo "push!(Libdl.DL_LOAD_PATH, \"$CONDA_DIR/lib\")" >> /etc/julia/juliarc.jl \
 && mkdir $JULIA_PKGDIR \
 && chown $NB_USER $JULIA_PKGDIR
USER $NB_USER
RUN echo "Install R and R basic packages" \
 && conda config --add channels conda-forge \
 && conda install -y 'r-essentials=3.5.1' \
 && conda config --add channels ataenzer \
 && conda config --add channels jsignell \
 && conda config --add channels pjones \
 && conda config --add channels omgarcia \
 && conda config --add channels cgat \
 && conda config --add channels sebp \
 && conda config --add channels https://mirrors.tuna.tsinghua.edu.cn/anaconda/cloud/bioconda/ \
 && conda config --add channels https://mirrors.tuna.tsinghua.edu.cn/anaconda/cloud/conda-forge/ \
 && conda install -y 'r-tm' 'r-rcolorbrewer' 'r-nnet' 'r-nlp' 'r-ggmap' 'r-chron' 'r-rocr' 'r-arules' \
 && echo 'options("repos" = c(CRAN="http://cran.us.r-project.org"))' > ~/.Rprofile \
 && R -e " install.packages('choroplethr'); install.packages('choroplethrMaps'); install.packages('Metrics'); install.packages('DescTools'); install.packages('tools'); install.packages('grid'); install.packages('WDI'); install.packages('wordcloud'); install.packages('wordcloud2'); install.packages('xgboost'); install.packages('plotly');" \
 && echo 'options("repos" = c(CRAN="https://mirrors.tuna.tsinghua.edu.cn/CRAN/"))' > ~/.Rprofile \
 && conda clean -tipsy
#   Add Julia packages. Only add HDF5 if this is not a test-only build since
#   it takes roughly half the entire build time of all of the images on Travis
#   to add this one package and often causes Travis to timeout.
#
#   Install IJulia as jovyan and then move the kernelspec out
#   to the system share location. Avoids problems with runtime UID change not
#   taking effect properly on the .local folder in the jovyan home dir.
RUN julia -e 'import Pkg; Pkg.update()' \
 && (test $TEST_ONLY_BUILD || julia -e 'import Pkg; Pkg.add("HDF5")' ) \
 && julia -e 'import Pkg; Pkg.add("Gadfly")' \
 && julia -e 'import Pkg; Pkg.add("RDatasets")' \
 && julia -e 'import Pkg; Pkg.add("IJulia")' \
 && julia -e 'using IJulia' \
 && mkdir -p $CONDA_DIR/share/jupyter/kernels/ \
 && mv $HOME/.local/share/jupyter/kernels/julia* $CONDA_DIR/share/jupyter/kernels/ \
 && chmod -R go+rx $CONDA_DIR/share/jupyter \
 && rm -rf $HOME/.local
USER root
#   Make sure /usr/local/ and conda directories belong to user, and install fonts.
RUN chown $NB_USER /usr/local/bin \
 && chown $NB_USER /usr/local/share \
 && chown -R $NB_USER /usr/local/lib \
 && chown -R $NB_USER /opt/conda/lib/python3.6/site-packages/ \
 && chown -R $NB_USER /usr/local/lib/python2.7/dist-packages/ \
 && mkdir -p /home/$NB_USER/.cache \
 && chown $NB_USER -R /home/$NB_USER/.cache \
 && rm -rf /root/.cache/pip/* \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/* \
 && rm -rf /home/$NB_USER/.cache/matplotlib/ \
 && echo "kesci ALL=NOPASSWD: /usr/bin/apt-get" > /etc/sudoers.d/kesci \
 && chmod 0400 /etc/sudoers.d/kesci
WORKDIR /home/$NB_USER/work
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
