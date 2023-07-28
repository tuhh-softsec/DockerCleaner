FROM ubuntu:bionic
ARG NB_USER="jovyan"
ENV NB_USER="${NB_USER}"
ARG NB_UID="1001"
ARG NB_GID="100"
ARG RPASSWORD=${RPASSWORD:-"rstudio"}
ARG DOCKERHUB_VERSION_UPDATE
ENV DOCKERHUB_VERSION="${DOCKERHUB_VERSION_UPDATE}"
LABEL Vincent="Nijs \"radiant@rady.ucsd.edu\""
ENV DEBIAN_FRONTEND="noninteractive"
RUN apt-get update \
 && apt-get -y upgrade \
 && apt-get install --no-install-recommends bash-completion=1:2.8-1ubuntu1 ca-certificates=20211016ubuntu0.18.04.1 file=1:5.32-2ubuntu0.4 fonts-texgyre=20160520-1 g++=4:7.4.0-1ubuntu2.3 gfortran=4:7.4.0-1ubuntu2.3 gsfonts=1:8.11+urwcyr1.0.7~pre44-4.4 libblas-dev=3.7.1-4ubuntu1 libbz2-1.0=1.0.6-8.1ubuntu0.2 libcurl3=7.58.0-2ubuntu2 libopenblas-dev=0.2.20+ds-4 libpangocairo-1.0-0=1.40.14-1ubuntu0.1 libpcre3=2:8.39-9ubuntu0.1 libpng16-16=1.6.34-1ubuntu0.18.04.2 libreadline7=7.0-3 libtiff5=4.0.9-5ubuntu0.10 liblzma5=5.2.2-1.3ubuntu0.1 locales=2.27-3ubuntu1.6 make=4.1-9.1ubuntu1 unzip=6.0-21ubuntu1.2 zip=3.0-11build1 zlib1g=1:1.2.11.dfsg-0ubuntu2.2 wget=1.19.4-1ubuntu2.2 software-properties-common=0.96.24.32.20 -y \
 && add-apt-repository --enable-source --yes "ppa:marutter/rrutter3.5" \
 && add-apt-repository --enable-source --yes "ppa:marutter/c2d4u3.5" \
 && add-apt-repository --yes "ppa:jonathonf/vim" \
 && apt-get update
RUN apt-get install --no-install-recommends apt-transport-https=1.6.14 gdebi-core=0.9.5.7+nmu2 libapparmor1=2.12-4ubuntu5.1 libcurl4-openssl-dev=7.58.0-2ubuntu3.24 libopenmpi-dev=2.1.1-8 libpq-dev=10.23-0ubuntu0.18.04.1 libssh2-1-dev=1.8.0-1 libssl-dev=1.1.1-1ubuntu2.1~18.04.21 libxml2=2.9.4+dfsg1-6.1ubuntu1.8 libxml2-dev=2.9.4+dfsg1-6.1ubuntu1.8 libicu-dev=60.2-3ubuntu3.2 libgdal-dev=2.2.3+dfsg-2 libproj-dev=4.9.3-2 libgsl-dev=2.4+dfsg-6 cmake=3.10.2-1ubuntu2.18.04.2 cargo=0.66.0+ds0ubuntu0.libgit2-0ubuntu0.18.04 r-base=3.4.4-1ubuntu1 r-base-dev=3.4.4-1ubuntu1 r-cran-pbdzmq=0.3.2+dfsg-1 r-cran-catools=1.17.1-2 r-cran-bitops=1.0-6-3 -y
#   setting up odbc for connections
RUN apt-get install --no-install-recommends unixodbc=2.3.4-1.1ubuntu3 unixodbc-dev=2.3.4-1.1ubuntu3 odbc-postgresql=1:10.01.0000-1 libsqliteodbc=0.9995-1 -y
#   Utilities
RUN apt-get install --no-install-recommends vim=2:8.0.1453-1ubuntu1.11 net-tools=1.60+git20161116.90da8a0-1ubuntu1 inetutils-ping=2:1.9.4-3ubuntu0.1 curl=7.58.0-2ubuntu3.24 git=1:2.17.1-1ubuntu0.17 nmap=7.60-1ubuntu5 socat=1.7.3.2-2ubuntu2 sudo=1.8.21p2-3ubuntu1.5 libcairo2-dev=1.15.10-2ubuntu0.1 libxt-dev=1:1.1.5-1 xclip=0.12+svn84-4build1 xsel=1.2.0-4 bzip2=1.0.6-8.1ubuntu0.2 python3-pip=9.0.1-2.3~ubuntu1.18.04.8 python3-setuptools=39.0.1-2ubuntu0.1 supervisor=3.3.1-1.1 libc6=2.27-3ubuntu1.6 libzmq5=4.2.5-1ubuntu0.2 libmagick++-dev=8:6.9.7.4+dfsg-16ubuntu6.15 ed=1.10-2.1 rsync=3.1.2-2.1ubuntu1.6 vifm=0.9.1-1 -y
#   TeX for the rmarkdown package in RStudio, and pandoc is also useful
RUN apt-get install --no-install-recommends texlive=2017.20180305-1 texlive-base=2017.20180305-1 texlive-latex-extra=2017.20180305-2 texlive-pstricks=2017.20180305-2 texlive-xetex=2017.20180305-1 -y \
 && apt-get -y autoremove \
 && apt-get clean \
 && apt-get update
#   Configure default locale, see https://github.com/rocker-org/rocker/issues/19
RUN echo "en_US.UTF-8 UTF-8" >> /etc/locale.gen \
 && locale-gen en_US.utf8 \
 && /usr/sbin/update-locale LANG=en_US.UTF-8
ENV LC_ALL="en_US.UTF-8"
ENV LANG="en_US.UTF-8"
#   Official R-Studio 1.2 release
ENV RSTUDIO_VERSION="1.2.1541"
RUN wget https://s3.amazonaws.com/rstudio-ide-build/server/bionic/amd64/rstudio-server-${RSTUDIO_VERSION}-amd64.deb \
 && gdebi -n rstudio-server-${RSTUDIO_VERSION}-amd64.deb \
 && rm rstudio-server-*-amd64.deb
#   link to Rstudio's pandoc
RUN ln -s /usr/lib/rstudio-server/bin/pandoc/pandoc /usr/local/bin/pandoc
RUN useradd -m -s /bin/bash -N -u $NB_UID $NB_USER \
 && echo "${NB_USER}:${RPASSWORD}" | chpasswd \
 && addgroup ${NB_USER} staff \
 && adduser ${NB_USER} sudo \
 && echo '%sudo ALL=(ALL) NOPASSWD:ALL' >> /etc/sudoers
#   Shiny
ENV SHINY_VERSION="1.5.9.923"
RUN wget https://download3.rstudio.org/ubuntu-14.04/x86_64/shiny-server-${SHINY_VERSION}-amd64.deb \
 && gdebi -n shiny-server-${SHINY_VERSION}-amd64.deb \
 && rm shiny-server-*-amd64.deb
WORKDIR /home/${NB_USER}
COPY .Rprofile /home/${NB_USER}/.Rprofile
RUN ln -sf /home/${NB_USER}/.Rprofile /home/shiny/.Rprofile \
 && mkdir -p /var/log/shiny-server \
 && mkdir -p /srv/shiny-server/apps \
 && chown shiny:shiny /var/log/shiny-server \
 && chmod -R ug+s /var/log/shiny-server \
 && chown -R shiny:shiny /srv/shiny-server \
 && chmod -R ug+s /srv/shiny-server \
 && chown shiny:shiny /home/shiny/.Rprofile \
 && chown ${NB_USER} /home/${NB_USER}/.Rprofile \
 && adduser ${NB_USER} shiny \
 && mkdir -p /var/log/supervisor \
 && chown ${NB_USER} /var/log/supervisor
#   set path to user directory to install packages
RUN sed -i -e 's/~\/R\/x86_64/~\/.rsm-msba\/R\/x86_64/' /etc/R/Renviron
#   installing some basic r-packages
RUN R -e 'install.packages(c("Rcpp", "R6", "digest", "shiny", "rmarkdown", "DBI", "RPostgreSQL", "odbc", "remotes", "rprojroot"))'
#   install renv for Docker creation
RUN R -e 'remotes::install_github("rstudio/renv")'
COPY shiny-server.conf /etc/shiny-server/shiny-server.conf
COPY supervisord.conf /etc/supervisor/conf.d/supervisord.conf
COPY userconf.sh /usr/local/bin/userconf.sh
RUN chmod +x /usr/local/bin/userconf.sh
#   copy dbase connections
COPY connections/ /etc/${NB_USER}/connections
EXPOSE 8080/tcp 8787/tcp
CMD ["/usr/bin/supervisord", "-c", "/etc/supervisor/conf.d/supervisord.conf"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
