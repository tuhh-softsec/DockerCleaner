#   Read the Docs - Environment base
FROM ubuntu:18.04
LABEL mantainer="Read the Docs <support@readthedocs.com>"
LABEL version="5.0.0"
ENV DEBIAN_FRONTEND="noninteractive"
ENV APPDIR="/app"
ENV LANG="C.UTF-8"
#   Versions, and expose labels for external usage
ENV PYTHON_VERSION_27="2.7.16"
ENV PYTHON_VERSION_35="3.5.7"
ENV PYTHON_VERSION_36="3.6.8"
ENV PYTHON_VERSION_37="3.7.3"
ENV PYPY_VERSION_35="pypy3.5-7.0.0"
ENV CONDA_VERSION="4.6.14"
LABEL python.version_27="$PYTHON_VERSION_27"
LABEL python.version_35="$PYTHON_VERSION_35"
LABEL python.version_36="$PYTHON_VERSION_36"
LABEL python.version_37="$PYTHON_VERSION_37"
LABEL pypy.version_35="$PYPY_VERSION_35"
LABEL conda.version="$CONDA_VERSION"
#   System dependencies
RUN :
RUN (apt-get update ;apt-get install --no-install-recommends software-properties-common=0.96.24.32.20 vim=2:8.0.1453-1ubuntu1.11 -y )
#   Install requirements
RUN (apt-get update ;apt-get install --no-install-recommends build-essential=12.4ubuntu1 bzr=2.7.0+bzr6622-10 curl=7.58.0-2ubuntu3.24 doxygen=1.8.13-10 g++=4:7.4.0-1ubuntu2.3 git-core graphviz-dev libbz2-dev=1.0.6-8.1ubuntu0.2 libcairo2-dev=1.15.10-2ubuntu0.1 libenchant1c2a=1.6.0-11.1 libevent-dev=2.1.8-stable-4build1 libffi-dev=3.2.1-8 libfreetype6=2.8.1-2ubuntu2.2 libfreetype6-dev=2.8.1-2ubuntu2.2 libgraphviz-dev=2.40.1-2 libjpeg8-dev=8c-2ubuntu8 libjpeg-dev=8c-2ubuntu8 liblcms2-dev=2.9-1ubuntu0.1 libmysqlclient-dev=5.7.41-0ubuntu0.18.04.1 libpq-dev=10.23-0ubuntu0.18.04.1 libreadline-dev=7.0-3 libsqlite3-dev=3.22.0-1ubuntu0.7 libtiff5-dev=4.0.9-5ubuntu0.10 libwebp-dev=0.6.1-2ubuntu0.18.04.1 libxml2-dev=2.9.4+dfsg1-6.1ubuntu1.8 libxslt1-dev=1.1.29-5ubuntu0.3 libxslt-dev mercurial=4.5.3-1ubuntu2.2 pandoc=1.19.2.4~dfsg-1build4 pkg-config=0.29.1-0ubuntu2 postgresql-client=10+190ubuntu0.1 subversion=1.9.7-4ubuntu1.1 zlib1g-dev=1:1.2.11.dfsg-0ubuntu2.2 -y )
#   pyenv extra requirements
#   https://github.com/pyenv/pyenv/wiki/Common-build-problems
RUN (apt-get update ;apt-get install --no-install-recommends liblzma-dev=5.2.2-1.3ubuntu0.1 libncurses5-dev=6.1-1ubuntu1.18.04 libncursesw5-dev=6.1-1ubuntu1.18.04 libssl-dev=1.1.1-1ubuntu2.1~18.04.21 llvm=1:6.0-41~exp5~ubuntu1 make=4.1-9.1ubuntu1 python-openssl=17.5.0-1ubuntu1 tk-dev=8.6.0+9 wget=1.19.4-1ubuntu2.2 xz-utils=5.2.2-1.3ubuntu0.1 -y )
#   LaTeX -- split to reduce image layer size
RUN (apt-get update ;apt-get install --no-install-recommends texlive-fonts-extra=2017.20180305-2 -y )
RUN (apt-get update ;apt-get install --no-install-recommends texlive-latex-extra-doc=2017.20180305-2 texlive-pictures-doc=2017.20180305-1 texlive-publishers-doc=2017.20180305-2 -y )
RUN (apt-get update ;apt-get install --no-install-recommends texlive-lang-english=2017.20180305-1 texlive-lang-japanese=2017.20180305-1 -y )
RUN (apt-get update ;apt-get install --no-install-recommends texlive-full=2017.20180305-1 -y )
#   lmodern: extra fonts
#   https://github.com/rtfd/readthedocs.org/issues/5494
#
#   xindy: is useful to generate non-ascii indexes
#   https://github.com/rtfd/readthedocs.org/issues/4454
RUN (apt-get update ;apt-get install --no-install-recommends fonts-symbola=2.60-1 lmodern=2.004.5-3 latex-cjk-chinese-arphic-bkai00mp=1.23 latex-cjk-chinese-arphic-gbsn00lp=1.23 latex-cjk-chinese-arphic-gkai00mp=1.23 texlive-fonts-recommended=2017.20180305-1 xindy=2.5.1.20160104-4build1 -y )
#   plantuml: is to support sphinxcontrib-plantuml
#   https://pypi.org/project/sphinxcontrib-plantuml/
#
#   imagemagick: is to support sphinx.ext.imgconverter
#   http://www.sphinx-doc.org/en/master/usage/extensions/imgconverter.html
#
#   rsvg-convert: is for SVG -> PDF conversion
#   using Sphinx extension sphinxcontrib.rsvgconverter, see
#   https://github.com/missinglinkelectronics/sphinxcontrib-svg2pdfconverter
#
#   swig: is required for different purposes
#   https://github.com/rtfd/readthedocs-docker-images/issues/15
RUN (apt-get update ;apt-get install --no-install-recommends imagemagick=8:6.9.7.4+dfsg-16ubuntu6.15 librsvg2-bin=2.40.20-2ubuntu0.2 plantuml=1:1.2017.15-1 swig=3.0.12-1 -y )
#   Install Python tools/libs
RUN (apt-get update ;apt-get install --no-install-recommends python-pip=9.0.1-2.3~ubuntu1.18.04.8 -y ) \
 && pip install auxlib==0.0.43 virtualenv==20.21.0 -U
#   sphinx-js dependencies: jsdoc and typedoc (TypeScript support)
RUN (apt-get update ;apt-get install --no-install-recommends nodejs=8.10.0~dfsg-2ubuntu0.4 npm=3.5.2-0ubuntu4 -y ) \
 && npm install jsdoc@4.0.2 typedoc@0.24.2 --global
#   UID and GID from readthedocs/user
RUN groupadd --gid 205 docs
RUN useradd -m --uid 1005 --gid 205 docs
USER docs
WORKDIR /home/docs
#   Install Conda
RUN curl -O https://repo.continuum.io/miniconda/Miniconda2-${CONDA_VERSION}-Linux-x86_64.sh
RUN bash Miniconda2-${CONDA_VERSION}-Linux-x86_64.sh -b -p /home/docs/.conda/
ENV PATH="$PATH:/home/docs/.conda/bin"
RUN rm -f Miniconda2-${CONDA_VERSION}-Linux-x86_64.sh
#   Install pyenv
RUN wget https://github.com/pyenv/pyenv/archive/master.zip
RUN unzip master.zip \
 && rm -f master.zip \
 && mv pyenv-master ~docs/.pyenv
ENV PYENV_ROOT="/home/docs/.pyenv"
ENV PATH="/home/docs/.pyenv/shims:$PATH:/home/docs/.pyenv/bin"
#   Install supported Python versions
RUN pyenv install $PYTHON_VERSION_27 \
 && pyenv install $PYTHON_VERSION_37 \
 && pyenv install $PYTHON_VERSION_35 \
 && pyenv install $PYTHON_VERSION_36 \
 && pyenv install $PYPY_VERSION_35 \
 && pyenv global $PYTHON_VERSION_27 $PYTHON_VERSION_37 $PYTHON_VERSION_36 $PYTHON_VERSION_35 $PYPY_VERSION_35
WORKDIR /tmp
RUN pyenv local $PYTHON_VERSION_27 \
 && pyenv exec pip install --no-cache-dir -U pip \
 && pyenv exec pip install --no-cache-dir -U setuptools \
 && pyenv exec pip install --no-cache-dir --only-binary numpy,scipy numpy scipy \
 && pyenv exec pip install --no-cache-dir pandas matplotlib virtualenv
RUN pyenv local $PYTHON_VERSION_37 \
 && pyenv exec pip install --no-cache-dir -U pip \
 && pyenv exec pip install --no-cache-dir -U setuptools \
 && pyenv exec pip install --no-cache-dir --only-binary numpy,scipy numpy scipy \
 && pyenv exec pip install --no-cache-dir pandas matplotlib virtualenv
RUN pyenv local $PYTHON_VERSION_36 \
 && pyenv exec pip install --no-cache-dir -U pip \
 && pyenv exec pip install --no-cache-dir -U setuptools \
 && pyenv exec pip install --no-cache-dir --only-binary numpy,scipy numpy scipy \
 && pyenv exec pip install --no-cache-dir pandas matplotlib virtualenv
RUN pyenv local $PYTHON_VERSION_35 \
 && pyenv exec pip install --no-cache-dir -U pip \
 && pyenv exec pip install --no-cache-dir -U setuptools \
 && pyenv exec pip install --no-cache-dir --only-binary numpy,scipy numpy scipy \
 && pyenv exec pip install --no-cache-dir pandas matplotlib virtualenv
RUN pyenv local $PYPY_VERSION_35 \
 && pyenv exec python -m ensurepip \
 && pyenv exec pip3 install --no-cache-dir -U pip \
 && pyenv exec pip install --no-cache-dir -U setuptools \
 && pyenv exec pip install --no-cache-dir virtualenv
WORKDIR /
CMD ["/bin/bash"]
# Please add your HEALTHCHECK here!!!
