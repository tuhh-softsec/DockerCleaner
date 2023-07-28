#  # Start with the rstudio image providing 'base R' as well as RStudio based on Debian testing
FROM rocker/geospatial
MAINTAINER Carl Boettiger cboettig@ropensci.org
#  # Refresh package list and upgrade
RUN apt-get update \
 && apt-get install --no-install-recommends cdbs=0.4.166 icedtea-netx=1.8.8-2 libapparmor-dev=3.0.8-1ubuntu2 libgsl0-dev libhiredis-dev=0.14.1-3 libleptonica-dev=1.82.0-3build2 libmpfr-dev=4.2.0-1 libpoppler-cpp-dev=22.12.0-2ubuntu1 libprotobuf-dev=3.21.12-1ubuntu7 librdf0-dev=1.0.17-3ubuntu1 libsasl2-dev=2.1.28+dfsg-10 libtesseract-dev=5.3.0-2build1 libwebp-dev=1.2.4-0.1build1 libxslt1-dev=1.1.35-1 mdbtools=1.0.0+dfsg-1 protobuf-compiler=3.21.12-1ubuntu7 python-pip python-pdftools tesseract-ocr-eng=1:4.1.0-2 -y \
 && R CMD javareconf \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/ \
 && rm -rf /tmp/downloaded_packages/ /tmp/*.rds \
 && wget -O /usr/local/bin/install2.r https://github.com/eddelbuettel/littler/raw/master/inst/examples/install2.r \
 && chmod +x /usr/local/bin/install2.r \
 && R CMD javareconf -e
#  # Install additional dependencies
RUN install2.r --error -r 'http://cran.rstudio.com' -r 'http://datacube.wu.ac.at' -r 'http://packages.ropensci.org' -r 'http://www.bioconductor.org/packages/release/bioc' -r 'http://nceas.github.io/drat' aws.s3 dismo drat geiger git2r knitcitations pander phylobase phytools Rcampdf redland rJava rhdf5 ropkgs ridigbio rgeolocate RJSONIO sangerseqR dataone datapack listviewer getPass dbplyr GGally Rserve RSclient Cairo dendextend IRdisplay outliers cranlogs akima mapdata plot3D memisc rapport RcppRedis mongolite countrycode redux rcdk MCMCglmm storr purrrlyr corrplot protolite tidytext janeaustenr wordcloud2 webp openair snow tmap forecast weathermetrics rnaturalearthhires rsvg clipr tiff sys Rmpfr plotKML readtext cld3 seqinr jose \
 && R -e "remotes::install_github('richfitz/drat.builder')" \
 && pip install retriever==3.1.0 \
 && install2.r --error -r 'http://cran.rstudio.com' -r 'http://packages.ropensci.org' -r 'http://www.omegahat.net/R' Rcompression RHTMLForms ROOXML RWordXML SSOAP XMLSchema rrdflibs rrdf \
 && rm -rf /tmp/downloaded_packages/ /tmp/*.rds
#  # Install the rOpenSci R packages that are currently on CRAN. must use single quote notation
RUN R -e 'out <- ropkgs::ro_pkgs(); readr::write_lines(out$packages$name[out$packages$on_cran], "ropensci.txt")' \
 && install2.r `cat ropensci.txt `
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
