ARG NODE_VERSION=${NODE_VERSION}
FROM phusion/baseimage:latest
#   Set Environment Variables
ENV DEBIAN_FRONTEND="noninteractive"
ENV LANGUAGE="en_US.UTF-8"
ENV LC_ALL="en_US.UTF-8"
ENV LC_CTYPE="en_US.UTF-8"
ENV LANG="en_US.UTF-8"
ENV TERM="xterm"
#   Start as root
USER root
#  ##########################################################################
#   Bootstrapping the image:
#  ##########################################################################
RUN locale-gen en_US.UTF-8
RUN (apt-get update ;apt-get install --no-install-recommends software-properties-common=0.99.35 -y )
RUN apt-get update \
 && (apt-get update ;apt-get install --no-install-recommends build-essential=12.9ubuntu3 pkg-config=1.8.1-1ubuntu2 libcurl4-openssl-dev=7.88.1-7ubuntu1 libedit-dev=3.1-20221030-2 libssl-dev=3.0.8-1ubuntu1 libxml2-dev=2.9.14+dfsg-1.1build2 xz-utils=5.4.1-0.2 libsqlite3-dev=3.40.1-1 sqlite3=3.40.1-1 git=1:2.39.2-1ubuntu1 curl=7.88.1-7ubuntu1 vim=2:9.0.1000-4ubuntu2 nano=7.2-1 postgresql-client=15+248 -y --allow-downgrades --allow-remove-essential --allow-change-held-packages ) \
 && apt-get clean
#   Source the bash
RUN . ~/.bashrc
#  ##########################################################################
#   Nodedock non-root user:
#  ##########################################################################
#   Add a non-root user to prevent files being created with root permissions on host machine.
ARG PUID=1000
ENV PUID="${PUID}"
ARG PGID=1000
ENV PGID="${PGID}"
#   always run apt update when start and after add new source list, then clean up at end.
RUN : \
 && groupadd -g ${PGID} nodedock \
 && useradd -u ${PUID} -g nodedock -m nodedock -G docker_env \
 && usermod -p "*" nodedock
#  ##########################################################################
#   Set Timezone
#  ##########################################################################
ARG TZ=UTC
ENV TZ="${TZ}"
RUN ln -snf /usr/share/zoneinfo/$TZ /etc/localtime \
 && echo $TZ > /etc/timezone
#  ##########################################################################
#   User Aliases
#  ##########################################################################
USER root
COPY ./aliases.sh /root/aliases.sh
COPY ./aliases.sh /home/nodedock/aliases.sh
RUN sed -i 's/\r//' /root/aliases.sh \
 && sed -i 's/\r//' /home/nodedock/aliases.sh \
 && chown nodedock:nodedock /home/nodedock/aliases.sh \
 && echo "" >> ~/.bashrc \
 && echo "# Load Custom Aliases" >> ~/.bashrc \
 && echo "source ~/aliases.sh" >> ~/.bashrc \
 && echo "" >> ~/.bashrc
USER nodedock
RUN echo "" >> ~/.bashrc \
 && echo "# Load Custom Aliases" >> ~/.bashrc \
 && echo "source ~/aliases.sh" >> ~/.bashrc \
 && echo "" >> ~/.bashrc
#  ##########################################################################
#   Crontab
#  ##########################################################################
USER root
COPY ./crontab /etc/cron.d
RUN chmod -R 644 /etc/cron.d
#  ##########################################################################
#   ssh:
#  ##########################################################################
ARG INSTALL_WORKSPACE_SSH=false
COPY insecure_id_rsa /tmp/id_rsa
COPY insecure_id_rsa.pub /tmp/id_rsa.pub
RUN if [ ${INSTALL_WORKSPACE_SSH} = true ] ; then rm -f /etc/service/sshd/down \
 && cat /tmp/id_rsa.pub >> /root/.ssh/authorized_keys \
 && cat /tmp/id_rsa.pub >> /root/.ssh/id_rsa.pub \
 && cat /tmp/id_rsa >> /root/.ssh/id_rsa \
 && rm -f /tmp/id_rsa* \
 && chmod 644 /root/.ssh/authorized_keys /root/.ssh/id_rsa.pub \
 && chmod 400 /root/.ssh/id_rsa \
 && cp -rf /root/.ssh /home/nodedock \
 && chown -R nodedock:nodedock /home/nodedock/.ssh ; fi
#  ##########################################################################
#   Libpng16 EXTENSION
#  ##########################################################################
ARG INSTALL_LIBPNG=false
RUN if [ ${INSTALL_LIBPNG} = true ] ; then apt update \
 && (apt-get update ;apt-get install --no-install-recommends libpng16-16=1.6.39-2 ) ; fi
#  ##########################################################################
#   Node / NVM:
#  ##########################################################################
USER nodedock
#   Check if NVM needs to be installed
ARG NODE_VERSION=node
ENV NODE_VERSION="${NODE_VERSION}"
ARG NODE_ENV=development
ENV NODE_ENV="${NODE_ENV}"
ARG INSTALL_NODE=false
ARG INSTALL_NPM_GULP=false
ARG INSTALL_NPM_BOWER=false
ARG INSTALL_NPM_VUE_CLI=false
ARG INSTALL_NPM_ANGULAR_CLI=false
ARG NPM_REGISTRY
ENV NPM_REGISTRY="${NPM_REGISTRY}"
ENV NVM_DIR="/home/nodedock/.nvm"
RUN if [ ${INSTALL_NODE} = true ] ; then curl -o- https://raw.githubusercontent.com/creationix/nvm/v0.33.8/install.sh | bash \
 && . $NVM_DIR/nvm.sh \
 && nvm install ${NODE_VERSION} \
 && nvm use ${NODE_VERSION} \
 && nvm alias ${NODE_VERSION} \
 && if [ ${NPM_REGISTRY} ] ; then npm config set registry ${NPM_REGISTRY} ; fi \
 && if [ ${INSTALL_NPM_GULP} = true ] ; then npm install gulp@4.0.2 -g ; fi \
 && if [ ${INSTALL_NPM_BOWER} = true ] ; then npm install bower@1.8.14 -g ; fi \
 && if [ ${INSTALL_NPM_VUE_CLI} = true ] ; then npm install @vue/cli -g ; fi \
 && if [ ${INSTALL_NPM_ANGULAR_CLI} = true ] ; then npm install @angular/cli -g ; fi ; fi
#   Wouldn't execute when added to the RUN statement in the above block
#   Source NVM when loading bash since ~/.profile isn't loaded on non-login shell
RUN if [ ${INSTALL_NODE} = true ] ; then echo "" >> ~/.bashrc \
 && echo 'export NVM_DIR="$HOME/.nvm"' >> ~/.bashrc \
 && echo '[ -s "$NVM_DIR/nvm.sh" ] \
 && . "$NVM_DIR/nvm.sh" # This loads nvm' >> ~/.bashrc; fi
#   Add NVM binaries to root's .bashrc
USER root
RUN if [ ${INSTALL_NODE} = true ] ; then echo "" >> ~/.bashrc \
 && echo 'export NVM_DIR="/home/nodedock/.nvm"' >> ~/.bashrc \
 && echo '[ -s "$NVM_DIR/nvm.sh" ] \
 && . "$NVM_DIR/nvm.sh" # This loads nvm' >> ~/.bashrc; fi
#   Add PATH for node
ENV PATH="$PATH:$NVM_DIR/versions/node/v${NODE_VERSION}/bin"
RUN if [ ${NPM_REGISTRY} ] ; then . ~/.bashrc \
 && npm config set registry ${NPM_REGISTRY} ; fi
#  ##########################################################################
#   YARN:
#  ##########################################################################
USER nodedock
ARG INSTALL_YARN=false
ARG YARN_VERSION=latest
ENV YARN_VERSION="${YARN_VERSION}"
RUN if [ ${INSTALL_YARN} = true ] ; then [ -s "$NVM_DIR/nvm.sh" ] \
 && . "$NVM_DIR/nvm.sh" \
 && if [ ${YARN_VERSION} = "latest" ] ; then curl -o- -L https://yarnpkg.com/install.sh | bash ; else curl -o- -L https://yarnpkg.com/install.sh | bash -s -- --version ${YARN_VERSION} ; fi \
 && echo "" >> ~/.bashrc \
 && echo 'export PATH="$HOME/.yarn/bin:$PATH"' >> ~/.bashrc; fi
#   Add YARN binaries to root's .bashrc
USER root
RUN if [ ${INSTALL_YARN} = true ] ; then echo "" >> ~/.bashrc \
 && echo 'export YARN_DIR="/home/nodedock/.yarn"' >> ~/.bashrc \
 && echo 'export PATH="$YARN_DIR/bin:$PATH"' >> ~/.bashrc; fi
#  ##########################################################################
#   Image optimizers:
#  ##########################################################################
USER root
ARG INSTALL_IMAGE_OPTIMIZERS=false
RUN if [ ${INSTALL_IMAGE_OPTIMIZERS} = true ] ; then (apt-get update ;apt-get install --no-install-recommends jpegoptim=1.4.7-1 optipng=0.7.7-2build1 pngquant=2.17.0-1 gifsicle=1.93-2 -y ) \
 && if [ ${INSTALL_NODE} = true ] ; then exec bash \
 && . ~/.bashrc \
 && npm install svgo@3.0.2 -g ; fi ; fi
USER nodedock
#  ##########################################################################
#   PYTHON:
#  ##########################################################################
USER root
ARG INSTALL_PYTHON=false
RUN if [ ${INSTALL_PYTHON} = true ] ; then (apt-get update ;apt-get install --no-install-recommends python python-pip python-dev build-essential=12.9ubuntu3 -y ) \
 && python -m pip install --upgrade pip \
 && python -m pip install --upgrade virtualenv ; fi
#  ##########################################################################
#   ImageMagick:
#  ##########################################################################
USER root
ARG INSTALL_IMAGEMAGICK=false
RUN if [ ${INSTALL_IMAGEMAGICK} = true ] ; then (apt-get update ;apt-get install --no-install-recommends imagemagick=8:6.9.11.60+dfsg-1.6 -y ) ; fi
#  ##########################################################################
#   pgsql client
#  ##########################################################################
USER root
ARG INSTALL_PG_CLIENT=false
RUN if [ ${INSTALL_PG_CLIENT} = true ] ; then (apt-get update ;apt-get install --no-install-recommends wget=1.21.3-1ubuntu1 ) \
 && add-apt-repository "deb http://apt.postgresql.org/pub/repos/apt/ xenial-pgdg main" \
 && wget --quiet -O - https://www.postgresql.org/media/keys/ACCC4CF8.asc | apt-key add - \
 && apt-get update \
 && (apt-get update ;apt-get install --no-install-recommends postgresql-client-10 -y ) ; fi
#
#  --------------------------------------------------------------------------
#   Final Touch
#  --------------------------------------------------------------------------
#
USER root
#   Clean up
RUN apt-get clean \
 && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/* \
 && rm /var/log/lastlog /var/log/faillog
#   Set default work directory
WORKDIR /var/www
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
