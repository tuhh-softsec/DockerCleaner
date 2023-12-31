# # see https://github.com/pbhogale/gpu-keras-rstudio
FROM nvidia/cuda:9.0-cudnn7-runtime
MAINTAINER "prasanna bhogale"
ENV CRAN_URL="https://cloud.r-project.org/"
RUN set -e \
 && apt-get update -y \
 && apt-get install --no-install-recommends apt-utils libapparmor1 libcurl4-openssl-dev libxml2-dev libssl-dev gdebi-core apt-transport-https pandoc libssh2-1-dev libpython2.7 python-pip imagemagick libxt-dev libxml2-dev xterm -y
RUN set -e \
 && grep '^DISTRIB_CODENAME' /etc/lsb-release | cut -d = -f 2 | xargs -I {} echo "deb ${CRAN_URL}bin/linux/ubuntu {}/" | tee -a /etc/apt/sources.list \
 && apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E084DAB9 \
 && apt-get update -y \
 && apt-get -y upgrade \
 && apt-get install --no-install-recommends r-base r-base-dev r-cran-littler -y \
 && apt-get -y autoremove \
 && apt-get clean
RUN set -e \
 && apt-get install --no-install-recommends curl -y \
 && curl -sS https://s3.amazonaws.com/rstudio-server/current.ver | xargs -I {} curl -sS http://download2.rstudio.org/rstudio-server-{}-amd64.deb -o /tmp/rstudio.deb \
 && gdebi -n /tmp/rstudio.deb \
 && rm -rf /tmp/* \
 && apt-get -y autoremove \
 && apt-get clean
RUN set -e \
 && useradd -m -d /home/rstudio rstudio \
 && echo rstudio:rstudioTheLegendOfZelda | chpasswd \
 && apt-get -y autoremove \
 && apt-get clean
RUN set -e \
 && grep '^DISTRIB_CODENAME' /etc/lsb-release | cut -d = -f 2 | xargs -I {} echo "deb ${CRAN_URL}bin/linux/ubuntu {}/" | tee -a /etc/apt/sources.list \
 && apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E084DAB9 \
 && apt-get update \
 && apt-get upgrade -y -q \
 && pip install pip==9.0.3 --upgrade \
 && pip install virtualenv \
 && pip install pelican \
 && rm -rf .cache \
 && echo 'options(repos = c(CRAN = "https://cloud.r-project.org"))' >> /etc/R/Rprofile.site \
 && /usr/lib/R/site-library/littler/examples/install.r tensorflow keras \
 && r -e "install.packages(c('devtools', 'keras', 'xgboost', 'tidyverse', 'rmarkdown', 'greta', 'usethis', 'docopt'))" \
 && r -e "keras::install_keras(tensorflow = 'gpu')" \
 && apt-get -y autoremove \
 && apt-get clean
# # Add LaTeX, rticles and bookdown support
# # uses dummy texlive, see FAQ 8: https://yihui.name/tinytex/faq/
RUN apt-get install --no-install-recommends wget software-properties-common python-software-properties -y \
 && add-apt-repository -y ppa:opencpu/jq \
 && apt-get update \
 && wget "https://travis-bin.yihui.name/texlive-local.deb" \
 && dpkg -i texlive-local.deb \
 && rm texlive-local.deb \
 && apt-get update \
 && apt-get install --no-install-recommends default-jdk fonts-roboto ghostscript libbz2-dev libicu-dev liblzma-dev libhunspell-dev libmagick++-dev librdf0-dev libv8-dev libjq-dev qpdf texinfo ssh less vim git-all -y \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/ \
 && wget -qO- "https://github.com/yihui/tinytex/raw/master/tools/install-unx.sh" | sh -s - --admin --no-path \
 && mv ~/.TinyTeX /opt/TinyTeX \
 && /opt/TinyTeX/bin/*/tlmgr path add \
 && tlmgr install metafont mfware inconsolata tex ae parskip listings \
 && tlmgr path add \
 && Rscript -e "source('https://install-github.me/yihui/tinytex'); tinytex::r_texmf()" \
 && chown -R root:staff /opt/TinyTeX \
 && chmod -R g+w /opt/TinyTeX \
 && chmod -R g+wx /opt/TinyTeX/bin \
 && /usr/lib/R/site-library/littler/examples/install2.r --error --deps TRUE bookdown rticles rmdshower DT DiagrammeR bayesplot
RUN pip install typogrify pygments markdown beautifulsoup4 \
 && rm -rf .cache \
 && git clone --recursive https://github.com/getpelican/pelican-themes ~/pelican-themes \
 && pelican-themes -i ~/pelican-themes/Flex \
 && pelican-themes -i ~/pelican-themes/elegant \
 && pelican-themes -i ~/pelican-themes/pelican-bootstrap3 \
 && git clone --recursive https://github.com/getpelican/pelican-plugins ~/pelican-plugins
RUN apt-get update -y \
 && apt-get install --no-install-recommends libglu1-mesa-dev -y \
 && apt-get autoremove \
 && apt-get clean \
 && /usr/lib/R/site-library/littler/examples/install2.r --error --deps TRUE vars prophet fanplot chron tseriesEntropy tseriesChaos pdfetch Quandl factorstochvol wavelets Risk fGarch \
 && pelican-themes -i ~/pelican-themes/pelican-bootstrap3
RUN apt-get install --no-install-recommends libgsl-dev -y \
 && /usr/lib/R/site-library/littler/examples/install2.r --error --deps TRUE tidytext janeaustenr shiny wordcloud sentimentr stringr gutenbergr ggthemes rsconnect
EXPOSE 8787/tcp
CMD ["/usr/lib/rstudio-server/bin/rserver", "--server-daemonize=0", "--server-app-armor-enabled=0"]
