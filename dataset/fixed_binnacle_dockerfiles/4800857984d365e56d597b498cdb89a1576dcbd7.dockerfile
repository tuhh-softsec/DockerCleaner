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
 && (apt-get update ;apt-get install --no-install-recommends build-essential=12.9ubuntu3 pkg-config=1.8.1-1ubuntu2 libcurl4-openssl-dev=7.88.1-7ubuntu1 libedit-dev=3.1-20221030-2 libssl-dev=3.0.8-1ubuntu1 libxml2-dev=2.9.14+dfsg-1.1build2 xz-utils=5.4.1-0.2 libsqlite3-dev=3.40.1-1 sqlite3=3.40.1-1 git=1:2.39.2-1ubuntu1 curl=7.88.1-7ubuntu1 vim=2:9.0.1000-4ubuntu2 nano=7.2-1 -y --allow-downgrades --allow-remove-essential --allow-change-held-packages ) \
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
#   Node / NVM:
#  ##########################################################################
USER nodedock
#   Check if NVM needs to be installed
ARG NODE_VERSION=node
ENV NODE_VERSION="${NODE_VERSION}"
ARG NODE_ENV=development
ENV NODE_ENV="${NODE_ENV}"
ARG INSTALL_NODE=false
ARG NPM_REGISTRY
ENV NPM_REGISTRY="${NPM_REGISTRY}"
ENV NVM_DIR="/home/nodedock/.nvm"
#   Install nvm (A Node Version Manager)
RUN curl -o- https://raw.githubusercontent.com/creationix/nvm/v0.33.8/install.sh | bash \
 && . $NVM_DIR/nvm.sh \
 && nvm install ${NODE_VERSION} \
 && nvm use ${NODE_VERSION} \
 && nvm alias ${NODE_VERSION} \
 && if [ ${NPM_REGISTRY} ] ; then npm config set registry ${NPM_REGISTRY} ; fi
#   Wouldn't execute when added to the RUN statement in the above block
#   Source NVM when loading bash since ~/.profile isn't loaded on non-login shell
RUN echo "" >> ~/.bashrc \
 && echo 'export NVM_DIR="$HOME/.nvm"' >> ~/.bashrc \
 && echo '[ -s "$NVM_DIR/nvm.sh" ] \
 && . "$NVM_DIR/nvm.sh" # This loads nvm' >> ~/.bashrc
#   Add NVM binaries to root's .bashrc
USER root
RUN echo "" >> ~/.bashrc \
 && echo 'export NVM_DIR="/home/nodedock/.nvm"' >> ~/.bashrc \
 && echo '[ -s "$NVM_DIR/nvm.sh" ] \
 && . "$NVM_DIR/nvm.sh" # This loads nvm' >> ~/.bashrc
#   Add PATH for node
ENV PATH="$PATH:$NVM_DIR/versions/node/v${NODE_VERSION}/bin"
RUN if [ ${NPM_REGISTRY} ] ; then . ~/.bashrc \
 && npm config set registry ${NPM_REGISTRY} ; fi
#  ##########################################################################
#   YARN:
#  ##########################################################################
USER nodedock
ARG YARN_VERSION=latest
ENV YARN_VERSION="${YARN_VERSION}"
RUN [ -s "$NVM_DIR/nvm.sh" ] \
 && . "$NVM_DIR/nvm.sh" \
 && if [ ${YARN_VERSION} = "latest" ] ; then curl -o- -L https://yarnpkg.com/install.sh | bash ; else curl -o- -L https://yarnpkg.com/install.sh | bash -s -- --version ${YARN_VERSION} ; fi \
 && echo "" >> ~/.bashrc \
 && echo 'export PATH="$HOME/.yarn/bin:$PATH"' >> ~/.bashrc
#   Add YARN binaries to root's .bashrc
USER root
RUN echo "" >> ~/.bashrc \
 && echo 'export YARN_DIR="/home/nodedock/.yarn"' >> ~/.bashrc \
 && echo 'export PATH="$YARN_DIR/bin:$PATH"' >> ~/.bashrc
#  ##########################################################################
#   pgsql client
#  ##########################################################################
ARG INSTALL_PG_CLIENT=false
RUN if [ ${INSTALL_PG_CLIENT} = true ] ; then mkdir -p /usr/share/man/man1 \
 && mkdir -p /usr/share/man/man7 \
 && (apt-get update ;apt-get install --no-install-recommends postgresql-client=15+248 -y ) ; fi
#  ##########################################################################
#   Human Language and Character Encoding Support:
#  ##########################################################################
ARG INSTALL_INTL=false
RUN if [ ${INSTALL_INTL} = true ] ; then (apt-get update ;apt-get install --no-install-recommends zlib1g-dev=1:1.2.13.dfsg-1ubuntu4 libicu-dev=72.1-3ubuntu1 g++=4:12.2.0-3ubuntu1 -y ) ; fi
#  ##########################################################################
#   GHOSTSCRIPT:
#  ##########################################################################
ARG INSTALL_GHOSTSCRIPT=false
RUN if [ ${INSTALL_GHOSTSCRIPT} = true ] ; then (apt-get update ;apt-get install --no-install-recommends poppler-utils=22.12.0-2ubuntu1 ghostscript=10.0.0~dfsg1-0ubuntu1 -y ) ; fi
#  ##########################################################################
#   Image optimizers:
#  ##########################################################################
USER root
ARG INSTALL_IMAGE_OPTIMIZERS=false
RUN if [ ${INSTALL_IMAGE_OPTIMIZERS} = true ] ; then (apt-get update ;apt-get install --no-install-recommends jpegoptim=1.4.7-1 optipng=0.7.7-2build1 pngquant=2.17.0-1 gifsicle=1.93-2 -y ) ; fi
#  ##########################################################################
#   ImageMagick:
#  ##########################################################################
USER root
ARG INSTALL_IMAGEMAGICK=false
RUN if [ ${INSTALL_IMAGEMAGICK} = true ] ; then (apt-get update ;apt-get install --no-install-recommends libmagickwand-dev=8:6.9.11.60+dfsg-1.6 imagemagick=8:6.9.11.60+dfsg-1.6 -y ) ; fi
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
USER nodedock
WORKDIR /var/www
#   Source the bash before running npm start
RUN . ~/.bashrc
ARG NPM_START_SCRIPT=start
ENV NPM_START_SCRIPT="${NPM_START_SCRIPT}"
CMD ["sh", "-c", "npm", "run", "${NPM_START_SCRIPT}"]
EXPOSE 9000/tcp
# Please add your HEALTHCHECK here!!!
