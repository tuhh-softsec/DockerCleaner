#   To build from the parent directory:
#     docker build -t wch1/r-devel r-devel
#
#   To run:
#     docker run --rm -ti --name rd wch1/r-devel
#   Use a very recent version of Ubuntu to get the latest GCC, which we need for
#   some of options used for ASAN builds.
FROM ubuntu:18.04
MAINTAINER Winston Chang "winston@rstudio.com"
#   =====================================================================
#   R
#   =====================================================================
#   Don't print "debconf: unable to initialize frontend: Dialog" messages
ARG DEBIAN_FRONTEND=noninteractive
#   Need this to add R repo
RUN apt-get update \
 && apt-get install --no-install-recommends software-properties-common=0.96.24.32.20 -y
#   Add R apt repository
RUN apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E084DAB9
RUN add-apt-repository "deb http://cran.r-project.org/bin/linux/ubuntu $( lsb_release -cs ;)-cran35/"
#   Install basic stuff, R, and other packages that are useful for compiling R
#   and R packages.
RUN apt-get update \
 && apt-get install --no-install-recommends sudo=1.8.21p2-3ubuntu1.5 locales=2.27-3ubuntu1.6 git=1:2.17.1-1ubuntu0.17 vim-tiny=2:8.0.1453-1ubuntu1.11 less=487-0.1 wget=1.19.4-1ubuntu2.2 r-base=3.4.4-1ubuntu1 r-base-dev=3.4.4-1ubuntu1 r-recommended=3.4.4-1ubuntu1 fonts-texgyre=20160520-1 gdebi-core=0.9.5.7+nmu2 pandoc=1.19.2.4~dfsg-1build4 pandoc-citeproc=0.10.5.1-1build4 libcurl4-openssl-dev=7.58.0-2ubuntu3.24 libcairo2-dev=1.15.10-2ubuntu0.1 libpango1.0-dev=1.40.14-1ubuntu0.1 libxt-dev=1:1.1.5-1 libssl-dev=1.1.1-1ubuntu2.1~18.04.21 libxml2-dev=2.9.4+dfsg1-6.1ubuntu1.8 texinfo=6.5.0.dfsg.1-2 rsync=3.1.2-2.1ubuntu1.6 default-jdk=2:1.11-68ubuntu1~18.04.1 bison=2:3.0.4.dfsg-1build1 libtiff5-dev=4.0.9-5ubuntu0.10 tcl8.6-dev=8.6.8+dfsg-3 tk8.6-dev=8.6.8-4 xfonts-base=1:1.0.4+nmu1 xvfb=2:1.19.6-1ubuntu4.14 gcc-8=8.4.0-1ubuntu1~18.04 g++-8=8.4.0-1ubuntu1~18.04 gdb=8.1.1-0ubuntu1 valgrind=1:3.13.0-2ubuntu2.3 clang-7=1:7-3~ubuntu0.18.04.1 lldb-7=1:7-3~ubuntu0.18.04.1 -y
RUN locale-gen en_US.utf8 \
 && /usr/sbin/update-locale LANG=en_US.UTF-8
ENV LANG="en_US.UTF-8"
RUN update-alternatives --install /usr/bin/gcc gcc /usr/bin/gcc-8 800 --slave /usr/bin/g++ g++ /usr/bin/g++-8
RUN update-alternatives --install /usr/bin/clang clang /usr/bin/clang-7 800 --slave /usr/bin/clang++ clang++ /usr/bin/clang++-7
RUN echo 'options(\n repos = c(CRAN = "https://cloud.r-project.org/"),\n download.file.method = "libcurl",\n Ncpus = parallel::detectCores(logical=FALSE)\n)' >> /etc/R/Rprofile.site
#   Install TinyTeX (subset of TeXLive)
#   From FAQ 5 and 6 here: https://yihui.name/tinytex/faq/
#   Also install ae, parskip, and listings packages to build R vignettes
RUN wget -qO- "https://github.com/yihui/tinytex/raw/master/tools/install-unx.sh" | sh -s - --admin --no-path \
 && ~/.TinyTeX/bin/*/tlmgr path add \
 && tlmgr install metafont mfware inconsolata tex ae parskip listings xcolor \
 && tlmgr path add \
 && Rscript -e "source('https://install-github.me/yihui/tinytex'); tinytex::r_texmf()"
#   =====================================================================
#   Install various versions of R-devel
#   =====================================================================
#   Clone R-devel and download recommended packages
RUN cd /tmp \
 && git clone --depth 1 https://github.com/wch/r-source.git \
 && r-source/tools/rsync-recommended
COPY buildR.sh /tmp
#   RD: Install normal R-devel.
#
#   This R installation is slightly different from the ones that follow. It is
#   configured with the recommended packages, and has those packages installed
#   packages to library/ (not site-library/). These packages will be shared with
#   the other RD* installations that follow. For all the RD* installations
#   (including this one), all packages installed after buildR.sh runs will be
#   installed to each installation's site-library/.
#
#   I've set it up this way because the "recommended" packages take a long time
#   to compile and in most cases aren't involved in debugging the low-level
#   problems that this Dockerfile is for, so it's OK to compile them once and
#   share them. Other packages, like those installed by the user and Rcpp
#   (*especially* Rcpp), are often of interest -- they are installed for each
#   RD* installation, and code is compiled with whatever compiler settings are
#   used for each RD* installation.
RUN /tmp/buildR.sh
#   Install some commonly-used packages to a location used by all the RD*
#   installations. These packages do not have compiled code and do not depend on
#   packages that have compiled code.
RUN RD -q -e 'install.packages(c("BH", "R6", "magrittr", "memoise"), "/usr/local/RD/lib/R/library")'
#   Finally, install some common packages specific to this build of R.
RUN RD -q -e 'install.packages(c("devtools", "Rcpp", "roxygen2", "testthat"))'
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
