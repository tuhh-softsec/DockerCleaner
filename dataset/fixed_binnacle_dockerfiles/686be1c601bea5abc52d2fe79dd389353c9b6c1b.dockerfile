#   To build, cd to this directory, then:
#     docker build -t ss-shiny-devel .
#
#   To run with the built-in shiny-examples:
#     docker run --rm -p 3838:3838 --name ss ss-shiny-devel
FROM ubuntu:16.04
MAINTAINER Winston Chang "winston@rstudio.com"
#   =====================================================================
#   R
#   =====================================================================
#   Don't print "debconf: unable to initialize frontend: Dialog" messages
ARG DEBIAN_FRONTEND=noninteractive
#   Need this to add R repo
RUN apt-get update \
 && apt-get install --no-install-recommends software-properties-common=0.96.20.10 -y
#   Add R apt repository
RUN add-apt-repository "deb http://cran.r-project.org/bin/linux/ubuntu $( lsb_release -cs ;)/"
RUN apt-key adv --keyserver keyserver.ubuntu.com --recv-keys 0x51716619e084dab9
#   Install basic stuff and R
RUN apt-get update \
 && apt-get install --no-install-recommends sudo=1.8.16-0ubuntu1.10 git=1:2.7.4-0ubuntu1.10 curl=7.47.0-1ubuntu2.19 vim-tiny=2:7.4.1689-3ubuntu1.5 less=481-2.1ubuntu0.2 wget=1.17.1-1ubuntu1.5 r-base=3.2.3-4 r-base-dev=3.2.3-4 r-recommended=3.2.3-4 fonts-texgyre=20150923-1 texinfo=6.1.0.dfsg.1-5 locales=2.23-0ubuntu11.3 -y
#   Install jt for working with JSON from the RSC API
RUN cd /usr/local \
 && wget -O - https://github.com/micha/json-table/raw/master/jt.tar.gz | tar xzvf -
#   Install jo for making JSON from bash
RUN cd /tmp \
 && wget -O - https://github.com/jpmens/jo/releases/download/v1.1/jo-1.1.tar.gz | tar xzvf - \
 && cd jo-1.1 \
 && ./configure \
 && make install
RUN echo "en_US.UTF-8 UTF-8" >> /etc/locale.gen \
 && locale-gen en_US.utf8 \
 && /usr/sbin/update-locale LANG=en_US.UTF-8
ENV LANG="en_US.UTF-8"
RUN echo 'options(\n repos = c(CRAN = "https://cloud.r-project.org/"),\n download.file.method = "libcurl"\n)' >> /etc/R/Rprofile.site
#   Install TinyTeX (subset of TeXLive)
#   From FAQ 5 and 6 here: https://yihui.name/tinytex/faq/
#   Also install ae, parskip, and listings packages to build R vignettes
RUN wget -qO- "https://github.com/yihui/tinytex/raw/master/tools/install-unx.sh" | sh -s - --admin --no-path \
 && ~/.TinyTeX/bin/*/tlmgr path add \
 && tlmgr install metafont mfware inconsolata tex ae parskip listings \
 && tlmgr path add \
 && Rscript -e "source('https://install-github.me/yihui/tinytex'); tinytex::r_texmf()"
#   This is necessary for non-root users to follow symlinks to /root/.TinyTeX
RUN chmod 755 /root
#   Create docker user with empty password (will have uid and gid 1000)
RUN useradd --create-home --shell /bin/bash docker \
 && passwd docker -d \
 && adduser docker sudo
#   =====================================================================
#   Shiny Examples
#   =====================================================================
RUN apt-get update \
 && apt-get install --no-install-recommends libxml2-dev=2.9.3+dfsg1-1ubuntu0.7 libssl-dev=1.0.2g-1ubuntu4.20 libcairo2-dev=1.14.6-1 libxt-dev=1:1.1.5-0ubuntu1 libcurl4-openssl-dev=7.47.0-1ubuntu2.19 parallel=20161222-1~ubuntu0.16.04.1 -y
RUN bash -c 'echo "will cite" | parallel --bibtex'
RUN R -e "install.packages(c('devtools', 'packrat'))"
#   Install shiny-examples, and fix permissions for apps that require write
#   access to /shiny-examples
RUN cd / \
 && wget -nv https://github.com/rstudio/shiny-examples/archive/master.zip \
 && unzip -x master.zip \
 && mv shiny-examples-master shiny-examples \
 && cd shiny-examples
#   Packages that need to be installed from GitHub
RUN R -e 'source("/install_deps.R", echo = TRUE)'
COPY deployApp.R /usr/local/bin
COPY set_public.sh /usr/local/bin
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
