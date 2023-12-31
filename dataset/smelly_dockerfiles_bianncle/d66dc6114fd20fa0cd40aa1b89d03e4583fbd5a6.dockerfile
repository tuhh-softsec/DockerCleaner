#  $ docker build -t cpsievert/plotly-vtest .
#  $ docker run -e VMODE="ci" -v $(pwd):/home/plotly --privileged -p 3838:3838 cpsievert/plotly-vtest
#  ------------------------------------------------------------------------------
FROM ubuntu:16.04
MAINTAINER Carson Sievert "carson@rstudio.com"
#  Don't print "debconf: unable to initialize frontend: Dialog" messages
ARG DEBIAN_FRONTED=noninteractive
ARG CACHEBUST=1
#  Need this to add R repo
RUN apt-get update \
 && apt-get install software-properties-common -y
#  Add R apt repository
RUN add-apt-repository "deb http://cran.r-project.org/bin/linux/ubuntu $( lsb_release -cs ;)-cran35/"
RUN apt-key adv --keyserver keyserver.ubuntu.com --recv-keys 0x51716619e084dab9
#  Install basic stuff and R
RUN apt-get update \
 && apt-get install sudo git vim-tiny nano wget r-base r-base-dev r-recommended fonts-texgyre texinfo locales libcurl4-gnutls-dev libcairo2-dev libxt-dev libssl-dev libxml2-dev -y
RUN echo "en_US.UTF-8 UTF-8" >> /etc/locale.gen \
 && locale-gen en_US.utf8 \
 && /usr/sbin/update-locale LANG=en_US.UTF-8
ENV LANG="en_US.UTF-8"
#  Rprofile
RUN echo 'options(\n repos = c(CRAN = "https://cloud.r-project.org/"),\n download.file.method = "libcurl",\n Ncpus = parallel::detectCores(logical=FALSE),\n shiny.host = "0.0.0.0", shiny.port = 3838\n)' >> /etc/R/Rprofile.site
#  Update R packages
RUN R -e "update.packages(ask = F)"
#  Other R packages needed for running visual tests
RUN R -e "install.packages('devtools')"
RUN R -e "install.packages('roxygen2')"
RUN R -e "install.packages('testthat')"
RUN R -e "install.packages('vdiffr')"
RUN R -e "install.packages('diffobj')"
#  sf system dependencies
RUN add-apt-repository ppa:ubuntugis/ubuntugis-unstable --yes
RUN apt-get update -y
RUN apt-get install libudunits2-dev libproj-dev libgeos-dev libgdal-dev -y
#  Install all plotly's dependencies
RUN R -e "install.packages('plotly', dependencies = T)"
#  system dependencies related to running orca
RUN apt-get install libgtk2.0-0 libgconf-2-4 xvfb fuse desktop-file-utils -y
#  google chrome
RUN wget -q -O - https://dl-ssl.google.com/linux/linux_signing_key.pub | apt-key add - \
 && sh -c 'echo "deb [arch=amd64] http://dl.google.com/linux/chrome/deb/ stable main" >> /etc/apt/sources.list.d/google-chrome.list' \
 && apt-get update -y \
 && apt-get install google-chrome-stable -y
#  Download orca binary and make it executable under xvfb
RUN wget https://github.com/plotly/orca/releases/download/v1.1.1/orca-1.1.1-x86_64.AppImage -P /home
RUN chmod 777 /home/orca-1.1.1-x86_64.AppImage
RUN printf '#!/bin/bash \nxvfb-run --auto-servernum --server-args "-screen 0 640x480x24" /home/orca-1.1.1-x86_64.AppImage "$@"' > /usr/bin/orca
RUN chmod 777 /usr/bin/orca
#  switch on visual testing
ENV VDIFFR="true"
EXPOSE 3838/tcp
RUN R -e "install.packages('assertthat')"
RUN R -e "install.packages('testthat')"
RUN R -e "update.packages(ask=FALSE)"
#  install any new dependencies, then either manage cases (the default) or run tests
#  note the workaround to get docker to run a proper exit status when there are testthat errors
#  https://github.com/r-lib/testthat/issues/515#issuecomment-304169376
CMD cd /home/plotly \
 && R -e "devtools::install_deps(dep = T); if (!identical(Sys.getenv('VMODE'), 'ci')) vdiffr::manage_cases(); res <- devtools::test(reporter='summary'); df <- as.data.frame(res); if (sum(df$failed) > 0 || any(df$error)) q(status=1)"
