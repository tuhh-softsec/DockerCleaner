#   Read the Docs - Environment base
FROM ubuntu:18.04
MAINTAINER Read the Docs <support@readthedocs.com>
LABEL version="4.0.0rc1"
ENV DEBIAN_FRONTEND="noninteractive"
ENV APPDIR="/app"
ENV LANG="C.UTF-8"
#   Versions, and expose labels for exernal usage
ENV PYTHON_VERSION_27="2.7.14"
ENV PYTHON_VERSION_35="3.5.5"
ENV PYTHON_VERSION_36="3.6.4"
ENV CONDA_VERSION="4.4.10"
LABEL python.version_27="$PYTHON_VERSION_27"
LABEL python.version_35="$PYTHON_VERSION_35"
LABEL python.version_36="$PYTHON_VERSION_36"
LABEL conda.version="$CONDA_VERSION"
#   System dependencies
RUN :
RUN (apt-get update ;apt-get install --no-install-recommends vim=2:8.0.1453-1ubuntu1.11 software-properties-common=0.96.24.32.20 -y )
#   Install requirements
RUN (apt-get update ;apt-get install --no-install-recommends bzr=2.7.0+bzr6622-10 subversion=1.9.7-4ubuntu1.1 git-core mercurial=4.5.3-1ubuntu2.2 libpq-dev=10.23-0ubuntu0.18.04.1 libxml2-dev=2.9.4+dfsg1-6.1ubuntu1.8 libxslt-dev libxslt1-dev=1.1.29-5ubuntu0.3 build-essential=12.4ubuntu1 postgresql-client=10+190ubuntu0.1 libmysqlclient-dev=5.7.41-0ubuntu0.18.04.1 curl=7.58.0-2ubuntu3.24 doxygen=1.8.13-10 g++=4:7.4.0-1ubuntu2.3 graphviz-dev libfreetype6=2.8.1-2ubuntu2.2 libbz2-dev=1.0.6-8.1ubuntu0.2 libcairo2-dev=1.15.10-2ubuntu0.1 libenchant1c2a=1.6.0-11.1 libevent-dev=2.1.8-stable-4build1 libffi-dev=3.2.1-8 libfreetype6-dev=2.8.1-2ubuntu2.2 libgraphviz-dev=2.40.1-2 libjpeg-dev=8c-2ubuntu8 libjpeg8-dev=8c-2ubuntu8 liblcms2-dev=2.9-1ubuntu0.1 libreadline-dev=7.0-3 libsqlite3-dev=3.22.0-1ubuntu0.7 libtiff5-dev=4.0.9-5ubuntu0.10 libwebp-dev=0.6.1-2ubuntu0.18.04.1 pandoc=1.19.2.4~dfsg-1build4 pkg-config=0.29.1-0ubuntu2 zlib1g-dev=1:1.2.11.dfsg-0ubuntu2.2 -y )
#   pyenv extra requirements
RUN (apt-get update ;apt-get install --no-install-recommends make=4.1-9.1ubuntu1 libssl-dev=1.1.1-1ubuntu2.1~18.04.21 wget=1.19.4-1ubuntu2.2 llvm=1:6.0-41~exp5~ubuntu1 libncurses5-dev=6.1-1ubuntu1.18.04 libncursesw5-dev=6.1-1ubuntu1.18.04 xz-utils=5.2.2-1.3ubuntu0.1 tk-dev=8.6.0+9 -y )
#   LaTeX -- split to reduce image layer size
RUN (apt-get update ;apt-get install --no-install-recommends texlive-fonts-extra=2017.20180305-2 -y )
RUN (apt-get update ;apt-get install --no-install-recommends texlive-latex-extra-doc=2017.20180305-2 texlive-publishers-doc=2017.20180305-2 texlive-pictures-doc=2017.20180305-1 -y )
RUN (apt-get update ;apt-get install --no-install-recommends texlive-lang-english=2017.20180305-1 texlive-lang-japanese=2017.20180305-1 -y )
RUN (apt-get update ;apt-get install --no-install-recommends texlive-full=2017.20180305-1 -y )
RUN (apt-get update ;apt-get install --no-install-recommends texlive-fonts-recommended=2017.20180305-1 latex-cjk-chinese-arphic-bkai00mp=1.23 latex-cjk-chinese-arphic-gbsn00lp=1.23 latex-cjk-chinese-arphic-gkai00mp=1.23 fonts-symbola=2.60-1 -y )
#   Install plantuml
RUN (apt-get update ;apt-get install --no-install-recommends plantuml=1:1.2017.15-1 -y )
#   Install Python tools/libs
RUN (apt-get update ;apt-get install --no-install-recommends python-pip=9.0.1-2.3~ubuntu1.18.04.8 -y ) \
 && pip install virtualenv==20.21.0 auxlib==0.0.43 -U
#   UID and GID from readthedocs/user
RUN groupadd --gid 205 docs
RUN useradd -m --uid 1005 --gid 205 docs
#   Install jsdoc
RUN (apt-get update ;apt-get install --no-install-recommends nodejs=8.10.0~dfsg-2ubuntu0.4 npm=3.5.2-0ubuntu4 -y ) \
 && npm install jsdoc@4.0.2 --global
USER docs
WORKDIR /home/docs
#   Install Conda
RUN curl -O https://repo.continuum.io/miniconda/Miniconda2-${CONDA_VERSION}-Linux-x86_64.sh
RUN bash Miniconda2-${CONDA_VERSION}-Linux-x86_64.sh -b -p /home/docs/.conda/
ENV PATH="$PATH:/home/docs/.conda/bin"
#   Install pyenv
RUN git clone --depth 1 https://github.com/yyuu/pyenv.git ~docs/.pyenv
ENV PYENV_ROOT="/home/docs/.pyenv"
ENV PATH="/home/docs/.pyenv/shims:$PATH:/home/docs/.pyenv/bin"
#   Install supported Python versions
RUN pyenv install $PYTHON_VERSION_27 \
 && pyenv install $PYTHON_VERSION_36 \
 && pyenv install $PYTHON_VERSION_35 \
 && pyenv global $PYTHON_VERSION_27 $PYTHON_VERSION_36 $PYTHON_VERSION_35
WORKDIR /tmp
RUN pyenv local $PYTHON_VERSION_27 \
 && pyenv exec pip install -U pip \
 && pyenv exec pip install --only-binary numpy,scipy numpy scipy \
 && pyenv exec pip install pandas matplotlib virtualenv
COPY requirements.txt /tmp
RUN pyenv local $PYTHON_VERSION_36 \
 && pyenv exec pip install -U pip \
 && pyenv exec pip install --only-binary numpy,scipy numpy scipy \
 && pyenv exec pip install pandas matplotlib virtualenv \
 && pyenv exec pip install -r /tmp/requirements.txt
RUN pyenv local $PYTHON_VERSION_35 \
 && pyenv exec pip install -U pip \
 && pyenv exec pip install --only-binary numpy,scipy numpy scipy \
 && pyenv exec pip install pandas matplotlib virtualenv
WORKDIR /
CMD ["/bin/bash"]
# Please add your HEALTHCHECK here!!!
