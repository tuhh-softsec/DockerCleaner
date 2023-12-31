FROM ubuntu:16.04
#   Define commonly used JAVA_HOME variable
ENV JAVA_HOME="/usr/lib/jvm/java-8-oracle"
ENV R_BASE_VERSION="3.3.2"
ENV VER="master"
ENV DL_TOOL="wget"
ENV DL_PATH="/opt/shiny/download/master"
ENV INS_PATH="/opt/shiny"
#   Install Java.
RUN apt-get update \
 && apt-get install --no-install-recommends python-software-properties=0.96.20.10 software-properties-common=0.96.20.10 -y \
 && echo oracle-java8-installer shared/accepted-oracle-license-v1-1 select true | debconf-set-selections \
 && add-apt-repository -y ppa:webupd8team/java \
 && apt-get update \
 && apt-get install --no-install-recommends oracle-java8-installer -y \
 && rm -rf /var/lib/apt/lists/* \
 && rm -rf /var/cache/oracle-jdk8-installer
RUN apt-get update \
 && apt-get install --no-install-recommends ed=1.10-2 less=481-2.1ubuntu0.2 locales=2.23-0ubuntu11.3 nano=2.5.3-2ubuntu2 wget=1.17.1-1ubuntu1.5 ca-certificates=20210119~16.04.1 apt-transport-https=1.2.35 fonts-texgyre=20150923-1 -y \
 && rm -rf /var/lib/apt/lists/*
RUN echo "en_US.UTF-8 UTF-8" >> /etc/locale.gen \
 && locale-gen en_US.utf8 \
 && /usr/sbin/update-locale LANG=en_US.UTF-8
ENV LC_ALL="en_US.UTF-8"
ENV LANG="en_US.UTF-8"
RUN echo "deb https://cran.rstudio.org/bin/linux/ubuntu xenial/" > /etc/apt/sources.list.d/rstudio.list
RUN apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E084DAB9 \
 && apt-get update \
 && apt-get install --no-install-recommends sudo=1.8.16-0ubuntu1.10 gdebi-core=0.9.5.7ubuntu1 pandoc=1.16.0.2~dfsg-1 pandoc-citeproc=0.9-1 libcurl4-gnutls-dev=7.47.0-1ubuntu2.19 libcairo2-dev=1.14.6-1 libxt-dev=1:1.1.5-0ubuntu1 r-base=${R_BASE_VERSION}* r-base-dev=${R_BASE_VERSION}* r-recommended=${R_BASE_VERSION}* -y \
 && echo 'options(repos = c(CRAN = "https://cran.rstudio.com/"), download.file.method = "libcurl")' >> /etc/R/Rprofile.site \
 && rm -rf /tmp/downloaded_packages/ /tmp/*.rds \
 && rm -rf /var/lib/apt/lists/*
#   Download and install shiny server
RUN R -e "install.packages(c('shiny', 'rmarkdown'), repos='https://cran.rstudio.com/')"
RUN mkdir -p ${DL_PATH} \
 && mkdir -p ${INS_PATH} \
 && wget -nv -O ${DL_PATH}/${VER}.tar.gz https://github.com/statsplot/jshinyserver/archive/${VER}.tar.gz \
 && tar zxf ${DL_PATH}/${VER}.tar.gz -C ${DL_PATH} \
 && ln -s ${DL_PATH}/jshinyserver-*/source/Objects ${INS_PATH}/server \
 && mkdir -p ${INS_PATH}/server/logs ${INS_PATH}/server/pid \
 && echo "[Done] jShiny server ${VER} installed to ${INS_PATH}/server"
RUN rm -f ${DL_PATH}/${VER}.tar.gz
RUN touch ${INS_PATH}/server/config/applist.update
#  # in case you need to mount folders to /opt/shiny as non-root user
RUN chmod -R 777 ${INS_PATH}
#  # https://hub.docker.com/r/rocker/r-apt/~/dockerfile/
#  # ---- change user 'docker' to 'ruser'. 
#  # ---- docker.docker in the host has access to docker daemon 
#  # Set a default user. Available via runtime flag `--user ruser` 
#  # Add user to 'staff' group, granting them write privileges to /usr/local/lib/R/site.library
#  # User should also have & own a home directory (for rstudio or linked volumes to work properly). 
RUN useradd ruser \
 && mkdir /home/ruser \
 && chown ruser:ruser /home/ruser \
 && addgroup ruser staff
#  # By default, docker daemon starts container with root(superuser)
#  # Superuser inside a container don't get all the Linux capabilities
#  # Most of the time, it's safe to run with superuser
#  # Run as non-root 'ruser' with  'docker run -u ruser:ruser ...'
#  # In the host the user 'ruser'(in 'ruser' group) should exist.
#  # Run as 'ruser' inside container, when you mount host folders/files,
#  # if you need to execute/access/read/write/create/delete to a folder/file, 
#  # 'ruser' in the host should has corresponding privileges to it
#  # and access to its parent folders. 
#  # Remember that inside the container, non-root user can't access to all
#  # the files/folders or execute commands which need root privileges.
EXPOSE 8888/tcp
WORKDIR ${INS_PATH}/server
CMD ["java", "-jar", "server.jar"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
