#  Read the Docs - Environment base
FROM ubuntu:18.04
MAINTAINER Read the Docs <support@readthedocs.com>
LABEL version="4.0.0rc1"
ENV DEBIAN_FRONTEND="noninteractive"
ENV APPDIR="/app"
ENV LANG="C.UTF-8"
#  Versions, and expose labels for exernal usage
ENV PYTHON_VERSION_27="2.7.14"
ENV PYTHON_VERSION_35="3.5.5"
ENV PYTHON_VERSION_36="3.6.4"
ENV CONDA_VERSION="4.4.10"
LABEL python.version_27="$PYTHON_VERSION_27"
LABEL python.version_35="$PYTHON_VERSION_35"
LABEL python.version_36="$PYTHON_VERSION_36"
LABEL conda.version="$CONDA_VERSION"
#  System dependencies
RUN apt-get update -y
RUN apt-get install --no-install-recommends vim software-properties-common -y
#  Install requirements
RUN apt-get install --no-install-recommends bzr subversion git-core mercurial libpq-dev libxml2-dev libxslt-dev libxslt1-dev build-essential postgresql-client libmysqlclient-dev curl doxygen g++ graphviz-dev libfreetype6 libbz2-dev libcairo2-dev libenchant1c2a libevent-dev libffi-dev libfreetype6-dev libgraphviz-dev libjpeg-dev libjpeg8-dev liblcms2-dev libreadline-dev libsqlite3-dev libtiff5-dev libwebp-dev pandoc pkg-config zlib1g-dev -y
#  pyenv extra requirements
RUN apt-get install --no-install-recommends make libssl-dev wget llvm libncurses5-dev libncursesw5-dev xz-utils tk-dev -y
#  LaTeX -- split to reduce image layer size
RUN apt-get install --no-install-recommends texlive-fonts-extra -y
RUN apt-get install --no-install-recommends texlive-latex-extra-doc texlive-publishers-doc texlive-pictures-doc -y
RUN apt-get install --no-install-recommends texlive-lang-english texlive-lang-japanese -y
RUN apt-get install --no-install-recommends texlive-full -y
RUN apt-get install --no-install-recommends texlive-fonts-recommended latex-cjk-chinese-arphic-bkai00mp latex-cjk-chinese-arphic-gbsn00lp latex-cjk-chinese-arphic-gkai00mp fonts-symbola -y
#  Install plantuml
RUN apt-get install --no-install-recommends plantuml -y
#  Install Python tools/libs
RUN apt-get install --no-install-recommends python-pip -y \
 && pip install virtualenv auxlib -U
#  UID and GID from readthedocs/user
RUN groupadd --gid 205 docs
RUN useradd -m --uid 1005 --gid 205 docs
#  Install jsdoc
RUN apt-get install --no-install-recommends nodejs npm -y \
 && npm install jsdoc --global
USER docs
WORKDIR /home/docs
#  Install Conda
RUN curl -O https://repo.continuum.io/miniconda/Miniconda2-${CONDA_VERSION}-Linux-x86_64.sh
RUN bash Miniconda2-${CONDA_VERSION}-Linux-x86_64.sh -b -p /home/docs/.conda/
ENV PATH="$PATH:/home/docs/.conda/bin"
#  Install pyenv
RUN git clone --depth 1 https://github.com/yyuu/pyenv.git ~docs/.pyenv
ENV PYENV_ROOT="/home/docs/.pyenv"
ENV PATH="/home/docs/.pyenv/shims:$PATH:/home/docs/.pyenv/bin"
#  Install supported Python versions
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
