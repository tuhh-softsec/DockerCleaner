#   Jupyter container used for Galaxy IPython (+other kernels) Integration
FROM jupyter/datascience-notebook:628fbcb24afd
MAINTAINER Björn A. Grüning, bjoern.gruening@gmail.com
ENV DEBIAN_FRONTEND="noninteractive"
#   Install system libraries first as root
USER root
RUN apt-get update -qq \
 && apt-get install --no-install-recommends libcurl4-openssl-dev libxml2-dev apt-transport-https python-dev libc-dev pandoc pkg-config liblzma-dev libbz2-dev libpcre3-dev build-essential libblas-dev liblapack-dev gfortran libzmq3-dev libyaml-dev libxrender1 fonts-dejavu libfreetype6-dev libpng-dev net-tools procps libreadline-dev wget software-properties-common octave zlib1g-dev libtinfo-dev libcairo2-dev libpango1.0-dev -y \
 && apt-get autoremove -y \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*
#   Glasgow Haskell Compiler
#  RUN add-apt-repository -y ppa:hvr/ghc && \
#      sed -i s/jessie/trusty/g /etc/apt/sources.list.d/hvr-ghc-jessie.list && \
#      apt-get update && apt-get install -y cabal-install-1.22 ghc-7.8.4 happy-1.19.4 alex-3.1.3 && \
#      apt-get autoremove -y && apt-get clean && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*
#   Ruby dependencies
RUN add-apt-repository -y ppa:brightbox/ruby-ng \
 && sed -i s/jessie/trusty/g /etc/apt/sources.list.d/brightbox-ruby-ng-jessie.list \
 && apt-get update \
 && apt-get install --no-install-recommends ruby2.2 ruby2.2-dev libtool autoconf automake gnuplot-nox libsqlite3-dev libatlas-base-dev libgsl0-dev libmagick++-dev imagemagick -y \
 && ln -s /usr/bin/libtoolize /usr/bin/libtool \
 && apt-get purge -y software-properties-common \
 && apt-get autoremove -y \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*
RUN gem install rbczmq --version 1.7.9 --no-rdoc --no-ri
ENV PATH="/home/$NB_USER/.cabal/bin:/opt/cabal/1.22/bin:/opt/ghc/7.8.4/bin:/opt/happy/1.19.4/bin:/opt/alex/3.1.3/bin:$PATH"
USER jovyan
#   Python packages
RUN conda config --add channels r \
 && conda install --yes --quiet biopython rpy2 cython patsy statsmodels cloudpickle dill tensorflow=1.1* r-xml \
 && conda clean -yt \
 && pip install bioblend==1.1.1 galaxy-ie-helpers==0.2.7 --no-cache-dir
#   Now for a python2 environment
RUN /bin/bash -c "source activate python2 \
 && conda install --quiet --yes biopython rpy2 cython patsy statsmodels cloudpickle dill tensorflow=1.1* \
 && conda clean -yt \
 && pip install --no-cache-dir bioblend galaxy-ie-helpers"
#   IRuby
RUN iruby register
#   IHaskell + IHaskell-Widgets + Dependencies for examples
#  RUN cabal update && \
#      CURL_CA_BUNDLE='/etc/ssl/certs/ca-certificates.crt' curl 'https://www.stackage.org/lts-2.22/cabal.config?global=true' >> ~/.cabal/config && \
#      cabal install cpphs && \
#      cabal install gtk2hs-buildtools && \
#      cabal install ihaskell-0.8.0.0 --reorder-goals && \
#      cabal install ihaskell-widgets-0.2.2.1 HTTP Chart Chart-cairo && \
#       ~/.cabal/bin/ihaskell install && \
#      rm -fr $(echo ~/.cabal/bin/* | grep -iv ihaskell) ~/.cabal/packages ~/.cabal/share/doc ~/.cabal/setup-exe-cache ~/.cabal/logs
#   Extra Kernels
RUN pip install pip==23.1 --upgrade \
 && pip install bash_kernel==0.9.0 bioblend==1.1.1 octave_kernel==0.35.1 galaxy-ie-helpers==0.2.7 --user --no-cache-dir \
 && python -m bash_kernel.install \
 && echo 'export PATH=/home/jovyan/.local/bin:$PATH' >> /home/jovyan/.bashrc
COPY ./startup.sh /startup.sh
COPY ./monitor_traffic.sh /monitor_traffic.sh
COPY ./get_notebook.py /get_notebook.py
USER root
#   /import will be the universal mount-point for Jupyter
#   The Galaxy instance can copy in data that needs to be present to the Jupyter webserver
RUN mkdir /import
#   We can get away with just creating this single file and Jupyter will create the rest of the
#   profile for us.
RUN mkdir -p /home/$NB_USER/.ipython/profile_default/startup/
RUN mkdir -p /home/$NB_USER/.jupyter/custom/
COPY ./ipython-profile.py /home/$NB_USER/.ipython/profile_default/startup/00-load.py
#  ADD ./ipython_notebook_config.py /home/$NB_USER/.jupyter/jupyter_notebook_config.py
COPY jupyter_notebook_config.py /home/$NB_USER/.jupyter/
COPY ./custom.js /home/$NB_USER/.jupyter/custom/custom.js
COPY ./custom.css /home/$NB_USER/.jupyter/custom/custom.css
COPY ./default_notebook.ipynb /home/$NB_USER/notebook.ipynb
#   ENV variables to replace conf file
# A secret has been removed here. Please do not provide secrets from the Dockerfile as these will leak into the metadata of the resulting docker image. To provide secrets the --secret flag of the docker build command can be used (https://docs.docker.com/develop/develop-images/build_enhancements/#new-docker-build-secret-information).
ENV DEBUG="false" \
    GALAXY_WEB_PORT="10000" \
    CORS_ORIGIN="none" \
    DOCKER_PORT="none" \
    API_KEY="none" \
    HISTORY_ID="none" \
    REMOTE_HOST="none" \
    GALAXY_URL="none"
RUN chown -R $NB_USER:users /home/$NB_USER/ /import
USER jovyan
WORKDIR /import
#   Jupyter will run on port 8888, export this port to the host system
#   Start Jupyter Notebook
CMD /startup.sh
# Please add your HEALTHCHECK here!!!
