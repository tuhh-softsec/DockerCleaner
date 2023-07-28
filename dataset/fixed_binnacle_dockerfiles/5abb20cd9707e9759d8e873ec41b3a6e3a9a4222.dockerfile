#  ###########################################
#  # Scientific course @CINECA
FROM alpine:edge
MAINTAINER "Paolo D'Onorio De Meo <p.donoriodemeo@cineca.it>"
#  ###########################################
#   Base packages
ENV SHELL="/bin/bash"
ENV GLIBC_VERSION="2.23-r3"
ENV GLIBC_REPO="https://github.com/andyshinn/alpine-pkg-glibc/releases/download/$GLIBC_VERSION"
RUN apk add rethinkdb --update --repository http://dl-4.alpinelinux.org/alpine/edge/testing \
 && apk add bash=5.2.15-r2 wget=1.21.3-r3 curl=8.0.1-r1 ca-certificates=20230106-r0 bzip2=1.0.8-r4 unzip=6.0-r13 sudo=1.9.13_p3-r2 musl-dev=1.2.3_git20230322-r0 git=2.40.0-r0 vim=9.0.1440-r0 gcc=12.2.1_git20220924-r9 libstdc++=12.2.1_git20220924-r9 glib=2.76.1-r0 libxext=1.3.5-r0 libxrender=0.9.11-r1 tini=0.19.0-r1 --update \
 && curl -L "$GLIBC_REPO/glibc-${GLIBC_VERSION}.apk" -o /tmp/glibc.apk \
 && curl -L "$GLIBC_REPO/glibc-bin-${GLIBC_VERSION}.apk" -o /tmp/glibc-bin.apk \
 && curl -L "$GLIBC_REPO/glibc-i18n-${GLIBC_VERSION}.apk" -o /tmp/glibc-i18n.apk \
 && apk add /tmp/glibc*.apk --allow-untrusted \
 && /usr/glibc-compat/sbin/ldconfig /lib /usr/glibc-compat/lib \
 && /usr/glibc-compat/bin/localedef -i en_US -f UTF-8 en_US.UTF-8 \
 && rm -rf /tmp/glibc*apk \
 && rm -rf /var/cache/apk/*
#  ###########################################
#  # CONDA
ENV CONDA_DIR="/opt/conda"
RUN mkdir -p $CONDA_DIR
ENV PATH="$CONDA_DIR/bin:$PATH"
ENV CONDA_VER="4.1.11"
ENV CONTINUUM_REPO="https://repo.continuum.io/"
ENV CONDA_LINK="$CONTINUUM_REPO/miniconda/Miniconda3-${CONDA_VER}-Linux-x86_64.sh"
ENV INSTALL_SCRIPT="/tmp/conda.sh"
RUN echo $CONDA_LINK
RUN curl -k -o $INSTALL_SCRIPT $CONDA_LINK \
 && /bin/bash $INSTALL_SCRIPT -f -b -p $CONDA_DIR \
 && rm $INSTALL_SCRIPT \
 && echo "installing" \
 && conda install --quiet --yes jupyter pip pandas scipy matplotlib cython numba seaborn \
 && conda remove -y nbpresent \
 && conda install -y -c damianavila82 rise \
 && conda install -y -c pydy version_information \
 && conda clean -y --all
ENV PATH="$CONDA_DIR/bin:$PATH"
#  ###########################################
#  # Python normal libs
RUN pip install pip==23.1 xlrd==2.0.1 openpyxl==3.1.2 ptpython==3.0.23 ipython-sql==0.5.0 rethinkdb==2.4.9 attrs==23.1.0 flit==3.8.0 colorama==0.4.6 begins==0.9 pywebview==4.0.2 watchdog==3.0.0 ptpython==3.0.23 arrow==1.2.3 parsedatetime==2.6 timestring==1.6.4 boltons==23.0.0 --upgrade --no-cache-dir
#  ###########################################
#  # add user
ENV NB_USER="jovyan"
ENV NB_UID="1000"
#   Create jovyan user with UID=1000 and in the 'users' group
RUN adduser -s $SHELL -u $NB_UID $NB_USER -D \
 && mkdir -p $CONDA_DIR \
 && chown $NB_USER $CONDA_DIR
USER jovyan
#   Setup jovyan home directory
RUN mkdir /home/$NB_USER/work \
 && mkdir /home/$NB_USER/.jupyter \
 && mkdir /home/$NB_USER/.local \
 && echo "cacert=/etc/ssl/certs/ca-certificates.crt" > /home/$NB_USER/.curlrc
COPY notebook_config.py /home/$NB_USER/.jupyter/jupyter_notebook_config.py
#   Personal keybindings
RUN cd /opt/conda/share/jupyter/nbextensions/rise \
 && wget -q http://j.mp/risecustomkeys -O main.js
#   Fix matplotlib?
RUN rm -rf ~/.cache/matplotlib ~/.matplotlib/fontList.cache ~/.cache/fontconfig
#   Set executables for this user
RUN echo "export PATH=$CONDA_DIR/bin:/home/$NB_USER/.local/bin:$PATH" > /home/$NB_USER/.bashrc
#  #####################################
#  ## tini as principal process ###
USER root
#   jupyter kernel dies if you don't launch jupyter from inside a script
#   https://github.com/ipython/ipython/issues/7062#issuecomment-223809024
ENV BOOTER="/docker-entrypoint.sh"
#   https://docs.docker.com/engine/userguide/eng-image/dockerfile_best-practices/#/entrypoint
ENV DATADIR="/data"
VOLUME $DATADIR
WORKDIR $DATADIR
COPY bootstrap.sh /docker-entrypoint.sh
RUN chmod +x $BOOTER
RUN chown $NB_USER -R $DATADIR
#   WORKAROUND: tini
ENTRYPOINT ["/sbin/tini"]
CMD ["/docker-entrypoint.sh"]
RUN addgroup -S docker-user ; adduser -S -G docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
