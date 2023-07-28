#   Universal image for running Notebook, Dask pipelines, libs, and lint checkers
ARG BASE_IMAGE=ubuntu:18.04
FROM $BASE_IMAGE
ARG PYTHON=python3
ARG PIP=pip3
LABEL author="Piers Harding <piers.harding@catalyst.net.nz>" \
      description="ARL reference image" \
      license="Apache2.0" \
      registry="library/piersharding/arl" \
      vendor="Catalyst" \
      org.skatelescope.team="Systems Team" \
      org.skatelescope.version="0.1.0" \
      org.skatelescope.website="http://github.com/SKA-ScienceDataProcessor/algorithm-reference-library/"
#   Set environment variables for pipenv execution:
#
#   * LC_ALL and LANG: Pipenv (specifically, its Click dependency) exits with an
#     error unless the language encoding is set.
#   * PIPENV_TIMEOUT: increased Pipenv timeout as locking dependencies takes
#     *forever* inside a Docker container.
#   * PATH: puts virtualenv python/pip/pipenv first on path
#   * VIRTUAL_ENV: for completeness. This environment variable would have been
#     set by 'source /venv/bin/activate'
#   * PIPENV_VERBOSITY: hides warning about pipenv running inside a virtualenv.
#   * PIPENV_NOSPIN: disables animated spinner for cleaner CI logs
#
ENV LC_ALL="C.UTF-8" \
    LANG="C.UTF-8" \
    PIPENV_TIMEOUT="900" \
    PATH="/arl/venv/bin:$PATH" \
    VIRTUAL_ENV="/arl/venv" \
    PIPENV_VERBOSITY="-1" \
    PIPENV_NOSPIN="1" \
    HOME="/root" \
    DEBIAN_FRONTEND="noninteractive"
#   the package basics for Python 3
RUN apt-get update -y \
 && apt-get install --no-install-recommends software-properties-common=0.99.35 pkg-config=1.8.1-1ubuntu2 dirmngr=2.2.40-1ubuntu2 python3-software-properties=0.99.35 build-essential=12.9ubuntu3 curl=7.88.1-7ubuntu1 wget=1.21.3-1ubuntu1 fonts-liberation=1:1.07.4-11 ca-certificates=20230311 libcfitsio-dev=4.2.0-3 libffi-dev=3.4.4-1 -y \
 && add-apt-repository -y ppa:git-core/ppa \
 && curl -s https://packagecloud.io/install/repositories/github/git-lfs/script.deb.sh | bash \
 && apt-key adv --keyserver keyserver.ubuntu.com --recv-keys 6B05F25D762E3157 \
 && apt-get install --no-install-recommends git-lfs=3.3.0-1 -y \
 && git lfs install \
 && apt-get install --no-install-recommends flake8=5.0.4-4 virtualenv=20.19.0+ds-1 virtualenvwrapper=4.8.4-4 $PYTHON-dev $PYTHON-tk $PYTHON-nose -y \
 && apt-get install --no-install-recommends graphviz=2.42.2-7build3 -y \
 && apt-get install --no-install-recommends nodejs=18.13.0+dfsg1-1ubuntu2 npm=9.2.0~ds1-1 -y \
 && apt-get clean -y \
 && rm -rf /var/lib/apt/lists/* /var/cache/apt/archives/*
#   node node is linked to nodejs
RUN if [ ! -f /usr/bin/node ] ; then ln -s /usr/bin/nodejs /usr/bin/node ; fi \
 && node --version
#   sort out pip and python for 3.x
RUN cd /src ; wget https://bootstrap.pypa.io/get-pip.py \
 && $PYTHON get-pip.py ; rm -rf /root/.cache
#   Install Tini
RUN wget --quiet https://github.com/krallin/tini/releases/download/v0.18.0/tini \
 && echo "12d20136605531b09a2c2dac02ccee85e1b874eb322ef6baf7561cd93f93c855 *tini" | sha256sum -c - \
 && mv tini /usr/local/bin/tini \
 && chmod +x /usr/local/bin/tini
RUN mkdir -p /arl
WORKDIR /arl
RUN virtualenv -p $PYTHON ${VIRTUAL_ENV}
#   Install pipenv into the new virtual environment
RUN pip install pipenv==2023.3.20 ; rm -rf /root/.cache
#   Copy the Pipfile and frozen hashes (Pipfile.lock) across to the image so
#   that pipenv knows what to install
COPY Pipfile /src/Pipfile
COPY Pipfile.lock /src/Pipfile.lock
#   Install ARL dependencies into the virtual environment.
RUN cd /src ; pipenv install --dev ; rm -rf /root/.cache
#   Add and install Jupyter dependencies
RUN $PIP install bokeh \
 && $PIP install pytest ; $PIP install jupyter_nbextensions_configurator ; $PIP install jupyter_contrib_nbextensions ; rm -rf /root/.cache
RUN $PIP install -U pylint ; rm -rf /root/.cache
RUN jupyter contrib nbextension install --system --symlink
RUN jupyter nbextensions_configurator enable --system
#   runtime specific environment
ENV JENKINS_URL="1"
ENV PYTHONPATH="/arl"
ENV ARL="/arl"
ENV JUPYTER_PATH="/arl/examples/arl"
RUN touch "${HOME}/.bash_profile"
#   Bundle app source
#   COPY limited by /.dockerignore
COPY ./docker/boot.sh ./Makefile ./setup.py /arl/
COPY . /arl/
#   run setup
RUN cd /arl \
 && $PYTHON setup.py build \
 && $PYTHON setup.py install \
 && cp ./build/lib.*/*.so . \
 && cd /arl/workflows/ffiwrapped/serial \
 && make \
 && $PIP install mpi4py
#   create space for libs
RUN mkdir -p /arl/test_data /arl/test_results \
 && chmod 777 /arl /arl/test_data /arl/test_results
COPY --chown="1000:100" ./docker/jupyter_notebook_config.py "${HOME}/.jupyter/"
COPY ./docker/notebook.sh /usr/local/bin/
COPY ./docker/start-dask-scheduler.sh /usr/local/bin/
COPY ./docker/start-dask-worker.sh /usr/local/bin
#   We share in the arl data here
VOLUME ["/arl/data", "/arl/tmp"]
#   Expose Jupyter and Bokeh ports
EXPOSE 8888/tcp 8786/tcp 8787/tcp 8788/tcp 8789/tcp
#   Setup the entrypoint or environment
ENTRYPOINT ["tini", "--"]
#   Run - default is notebook
CMD ["/arl/boot.sh"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
