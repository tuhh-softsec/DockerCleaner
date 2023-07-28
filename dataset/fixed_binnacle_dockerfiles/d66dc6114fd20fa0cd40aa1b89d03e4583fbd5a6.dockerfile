#   $ docker build -t cpsievert/plotly-vtest .
#   $ docker run -e VMODE="ci" -v $(pwd):/home/plotly --privileged -p 3838:3838 cpsievert/plotly-vtest
#   ------------------------------------------------------------------------------
FROM ubuntu:16.04
MAINTAINER Carson Sievert "carson@rstudio.com"
#   Don't print "debconf: unable to initialize frontend: Dialog" messages
ARG DEBIAN_FRONTED=noninteractive
ARG CACHEBUST=1
#   Need this to add R repo
RUN apt-get update \
 && (apt-get update ;apt-get install --no-install-recommends software-properties-common=0.96.20.10 -y )
#   Add R apt repository
RUN add-apt-repository "deb http://cran.r-project.org/bin/linux/ubuntu $( lsb_release -cs ;)-cran35/"
RUN apt-key adv --keyserver keyserver.ubuntu.com --recv-keys 0x51716619e084dab9
#   Install basic stuff and R
RUN apt-get update \
 && (apt-get update ;apt-get install --no-install-recommends sudo=1.8.16-0ubuntu1.10 git=1:2.7.4-0ubuntu1.10 vim-tiny=2:7.4.1689-3ubuntu1.5 nano=2.5.3-2ubuntu2 wget=1.17.1-1ubuntu1.5 r-base=3.2.3-4 r-base-dev=3.2.3-4 r-recommended=3.2.3-4 fonts-texgyre=20150923-1 texinfo=6.1.0.dfsg.1-5 locales=2.23-0ubuntu11.3 libcurl4-gnutls-dev=7.47.0-1ubuntu2.19 libcairo2-dev=1.14.6-1 libxt-dev=1:1.1.5-0ubuntu1 libssl-dev=1.0.2g-1ubuntu4.20 libxml2-dev=2.9.3+dfsg1-1ubuntu0.7 -y )
RUN echo "en_US.UTF-8 UTF-8" >> /etc/locale.gen \
 && locale-gen en_US.utf8 \
 && /usr/sbin/update-locale LANG=en_US.UTF-8
ENV LANG="en_US.UTF-8"
#   Rprofile
RUN echo 'options(\n repos = c(CRAN = "https://cloud.r-project.org/"),\n download.file.method = "libcurl",\n Ncpus = parallel::detectCores(logical=FALSE),\n shiny.host = "0.0.0.0", shiny.port = 3838\n)' >> /etc/R/Rprofile.site
#   Update R packages
RUN R -e "update.packages(ask = F)"
#   Other R packages needed for running visual tests
RUN R -e "install.packages('devtools')"
RUN R -e "install.packages('roxygen2')"
RUN R -e "install.packages('testthat')"
RUN R -e "install.packages('vdiffr')"
RUN R -e "install.packages('diffobj')"
#   sf system dependencies
RUN add-apt-repository ppa:ubuntugis/ubuntugis-unstable --yes
RUN :
RUN (apt-get update ;apt-get install --no-install-recommends libudunits2-dev=2.2.20-1 libproj-dev=4.9.2-2 libgeos-dev=3.5.0-1ubuntu2 libgdal-dev=1.11.3+dfsg-3build2 -y )
#   Install all plotly's dependencies
RUN R -e "install.packages('plotly', dependencies = T)"
#   system dependencies related to running orca
RUN (apt-get update ;apt-get install --no-install-recommends libgtk2.0-0=2.24.30-1ubuntu1.16.04.2 libgconf-2-4=3.2.6-3ubuntu6 xvfb=2:1.18.4-0ubuntu0.12 fuse=2.9.4-1ubuntu3.1 desktop-file-utils=0.22-1ubuntu5.2 -y )
#   google chrome
RUN wget -q -O - https://dl-ssl.google.com/linux/linux_signing_key.pub | apt-key add - \
 && sh -c 'echo "deb [arch=amd64] http://dl.google.com/linux/chrome/deb/ stable main" >> /etc/apt/sources.list.d/google-chrome.list' \
 && apt-get update -y \
 && (apt-get update ;apt-get install --no-install-recommends google-chrome-stable -y )
#   Download orca binary and make it executable under xvfb
RUN wget https://github.com/plotly/orca/releases/download/v1.1.1/orca-1.1.1-x86_64.AppImage -P /home
RUN chmod 777 /home/orca-1.1.1-x86_64.AppImage
RUN printf '#!/bin/bash \nxvfb-run --auto-servernum --server-args "-screen 0 640x480x24" /home/orca-1.1.1-x86_64.AppImage "$@"' > /usr/bin/orca
RUN chmod 777 /usr/bin/orca
#   switch on visual testing
ENV VDIFFR="true"
EXPOSE 3838/tcp
RUN R -e "install.packages('assertthat')"
RUN R -e "install.packages('testthat')"
RUN R -e "update.packages(ask=FALSE)"
#   install any new dependencies, then either manage cases (the default) or run tests
#   note the workaround to get docker to run a proper exit status when there are testthat errors
#   https://github.com/r-lib/testthat/issues/515#issuecomment-304169376
CMD cd /home/plotly \
 && R -e "devtools::install_deps(dep = T); if (!identical(Sys.getenv('VMODE'), 'ci')) vdiffr::manage_cases(); res <- devtools::test(reporter='summary'); df <- as.data.frame(res); if (sum(df$failed) > 0 || any(df$error)) q(status=1)"
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
