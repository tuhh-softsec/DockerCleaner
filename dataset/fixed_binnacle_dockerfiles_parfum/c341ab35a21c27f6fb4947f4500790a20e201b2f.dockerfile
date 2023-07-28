# #####################################################
#  Dockerfile to build Sincell 1.6.0 container images #
#              Based on Ubuntu  14.04                 #
# #####################################################
#  Set the base image to Ubuntu
FROM ubuntu:14.04
#  File Author
MAINTAINER Geoffray Brelurut <brelurut@biologie.ens.fr>
#  Install required programs and clean up
RUN echo "deb http://cran.r-project.org/bin/linux/ubuntu trusty/" > /etc/apt/sources.list.d/cran.list; apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E084DAB9 ; apt-get update \
 && apt-get install --no-install-recommends r-base=3.3.1-1trusty0 r-base-core=3.3.1-1trusty0 r-base-dev=3.3.1-1trusty0 r-recommended=3.3.1-1trusty0 r-base-html=3.3.1-1trusty0 r-cran-boot=1.3-17-1trusty0 r-cran-class=7.3-14-1trusty0 r-cran-cluster=2.0.4-1trusty0 r-cran-codetools=0.2-14-2trusty0 r-cran-foreign=0.8.66-1trusty0 r-cran-kernsmooth=2.23-15-2trusty0 r-cran-lattice=0.20-33-1trusty0 r-cran-mass=7.3-44-1trusty0 r-cran-matrix=1.2-6-1trusty0 r-cran-mgcv=1.8-13-1trusty0 r-cran-nlme=3.1.128-2trusty0 r-cran-nnet=7.3-12-1trusty0 r-cran-rpart=4.1-10-1trusty0 r-cran-spatial=7.3-10-1trusty0 r-cran-survival=2.39-4-2trusty0 r-doc-html=3.3.1-1trusty0 wget --yes; wget http://cran.r-project.org/src/contrib/Archive/MASS/MASS_7.3-45.tar.gz http://cran.r-project.org/src/contrib/Archive/Rcpp/Rcpp_0.12.11.tar.gz http://cran.r-project.org/src/contrib/Archive/Rtsne/Rtsne_0.11.tar.gz ; R CMD INSTALL Rcpp_0.12.11.tar.gz MASS_7.3-45.tar.gz Rtsne_0.11.tar.gz ; wget https://cran.r-project.org/src/contrib/magrittr_1.5.tar.gz ; if [ ! -f ./magrittr_1.5.tar.gz ] ; then wget https://cran.r-project.org/src/contrib/Archive/magrittr/magrittr_1.5.tar.gz ; fi ; R CMD INSTALL magrittr_1.5.tar.gz ; wget http://cran.r-project.org/src/contrib/digest_0.6.12.tar.gz ; if [ ! -f ./digest_0.6.12.tar.gz ] ; then wget http://cran.r-project.org/src/contrib/Archive/digest/digest_0.6.12.tar.gz ; fi ; wget http://cran.r-project.org/src/contrib/gtable_0.2.0.tar.gz ; if [ ! -f ./gtable_0.2.0.tar.gz ] ; then wget http://cran.r-project.org/src/contrib/Archive/gtable/gtable_0.2.0.tar.gz ; fi ; wget http://cran.r-project.org/src/contrib/plyr_1.8.4.tar.gz ; if [ ! -f ./plyr_1.8.4.tar.gz ] ; then wget http://cran.r-project.org/src/contrib/Archive/plyr/plyr_1.8.4.tar.gz ; fi ; wget http://cran.r-project.org/src/contrib/stringi_1.1.5.tar.gz ; if [ ! -f ./stringi_1.1.5.tar.gz ] ; then wget http://cran.r-project.org/src/contrib/Archive/stringi/stringi_1.1.5.tar.gz ; fi ; wget http://cran.r-project.org/src/contrib/stringr_1.2.0.tar.gz ; if [ ! -f ./stringr_1.2.0.tar.gz ] ; then wget http://cran.r-project.org/src/contrib/Archive/stringr/stringr_1.2.0.tar.gz ; fi ; wget http://cran.r-project.org/src/contrib/reshape2_1.4.2.tar.gz ; if [ ! -f ./reshape2_1.4.2.tar.gz ] ; then wget http://cran.r-project.org/src/contrib/Archive/reshape2/reshape2_1.4.2.tar.gz ; fi ; wget http://cran.r-project.org/src/contrib/lazyeval_0.2.0.tar.gz ; if [ ! -f ./lazyeval_0.2.0.tar.gz ] ; then wget http://cran.r-project.org/src/contrib/Archive/lazyeval/lazyeval_0.2.0.tar.gz ; fi ; wget http://cran.r-project.org/src/contrib/rlang_0.1.1.tar.gz ; if [ ! -f ./rlang_0.1.1.tar.gz ] ; then wget http://cran.r-project.org/src/contrib/Archive/rlang/rlang_0.1.1.tar.gz ; fi ; wget http://cran.r-project.org/src/contrib/tibble_1.3.0.tar.gz ; if [ ! -f ./tibble_1.3.0.tar.gz ] ; then wget http://cran.r-project.org/src/contrib/Archive/tibble/tibble_1.3.0.tar.gz ; fi ; R CMD INSTALL lazyeval_0.2.0.tar.gz rlang_0.1.1.tar.gz tibble_1.3.0.tar.gz digest_0.6.12.tar.gz gtable_0.2.0.tar.gz stringi_1.1.5.tar.gz stringr_1.2.0.tar.gz plyr_1.8.4.tar.gz reshape2_1.4.2.tar.gz ; wget http://cran.r-project.org/src/contrib/RColorBrewer_1.1-2.tar.gz ; if [ ! -f RColorBrewer_1.1-2.tar.gz ] ; then wget http://cran.r-project.org/src/contrib/Archive/RColorBrewer/RColorBrewer_1.1-2.tar.gz ; fi ; wget http://cran.r-project.org/src/contrib/dichromat_2.0-0.tar.gz ; if [ ! -f dichromat_2.0-0.tar.gz ] ; then wget http://cran.r-project.org/src/contrib/Archive/dichromat/dichromat_2.0-0.tar.gz ; fi ; wget http://cran.r-project.org/src/contrib/munsell_0.4.3.tar.gz ; if [ ! -f munsell_0.4.3.tar.gz ] ; then wget http://cran.r-project.org/src/contrib/Archive/munsell/munsell_0.4.3.tar.gz ; fi ; wget http://cran.r-project.org/src/contrib/colorspace_1.2-6.tar.gz ; if [ ! -f color_space_1.2-6.tar.gz ] ; then wget http://cran.r-project.org/src/contrib/Archive/colorspace/colorspace_1.2-6.tar.gz ; fi ; wget http://cran.r-project.org/src/contrib/labeling_0.3.tar.gz ; if [ ! -f labeling_0.3.tar.gz ] ; then wget http://cran.r-project.org/src/contrib/Archive/labeling/labeling_0.3.tar.gz ; fi ; wget http://cran.r-project.org/src/contrib/scales_0.4.1.tar.gz ; if [ ! -f scales_0.4.1.tar.gz ] ; then wget http://cran.r-project.org/src/contrib/Archive/scales/scales_0.4.1.tar.gz ; fi ; R CMD INSTALL RColorBrewer_1.1-2.tar.gz dichromat_2.0-0.tar.gz colorspace_1.2-6.tar.gz munsell_0.4.3.tar.gz labeling_0.3.tar.gz scales_0.4.1.tar.gz ; wget http://cran.r-project.org/src/contrib/ggplot2_2.2.1.tar.gz ; if [ ! -f ggplot2_2.2.1.tar.gz ] ; then wget http://cran.r-project.org/src/contrib/Archive/ggplot2/ggplot2_2.2.1.tar.gz ; fi ; R CMD INSTALL ggplot2_2.2.1.tar.gz ; wget http://cran.rstudio.com/src/contrib/Archive/scatterplot3d/scatterplot3d_0.3-37.tar.gz http://cran.rstudio.com/src/contrib/Archive/cluster/cluster_2.0.4.tar.gz http://cran.rstudio.com/src/contrib/Archive/statmod/statmod_1.4.24.tar.gz http://cran.rstudio.com/src/contrib/Archive/proxy/proxy_0.4-16.tar.gz ; R CMD INSTALL scatterplot3d_0.3-37.tar.gz cluster_2.0.4.tar.gz statmod_1.4.24.tar.gz proxy_0.4-16.tar.gz ; wget http://cran.rstudio.com/src/contrib/entropy_1.2.1.tar.gz http://cran.rstudio.com/src/contrib/fastICA_1.2-0.tar.gz ; if [ ! -f entropy_1.2.1.tar.gz ] ; then wget http://cran.rstudio.com/src/contrib/Archive/entropy/entropy_1.2.1.tar.gz ; fi ; if [ ! -f fastICA_1.2-0.tar.gz ] ; then wget http://cran.rstudio.com/src/contrib/Archive/fastICA/fastICA_1.2-0.tar.gz ; fi ; R CMD INSTALL entropy_1.2.1.tar.gz fastICA_1.2-0.tar.gz ; wget http://cran.r-project.org/src/contrib/Archive/codetools/codetools_0.2-14.tar.gz ; R CMD INSTALL codetools_0.2-14.tar.gz ; wget http://cran.rstudio.com/src/contrib/Archive/colorspace/colorspace_1.2-6.tar.gz ; R CMD INSTALL colorspace_1.2-6.tar.gz ; wget http://cran.rstudio.com/src/contrib/iterators_1.0.8.tar.gz http://cran.r-project.org/src/contrib/foreach_1.4.3.tar.gz ; if [ ! -f iterators_1.0.8.tar.gz ] ; then wget http://cran.rstudio.com/src/contrib/Archive/iterators/iterators_1.0.8.tar.gz ; fi ; if [ ! -f foreach_1.4.3.tar.gz ] ; then wget http://cran.r-project.org/src/contrib/Archive/foreach/foreach_1.4.3.tar.gz ; fi ; R CMD INSTALL iterators_1.0.8.tar.gz foreach_1.4.3.tar.gz ; wget http://cran.rstudio.com/src/contrib/registry_0.3.tar.gz http://cran.rstudio.com/src/contrib/xtable_1.8-2.tar.gz http://cran.rstudio.com/src/contrib/pkgmaker_0.22.tar.gz http://cran.rstudio.com/src/contrib/gridBase_0.4-7.tar.gz http://cran.r-project.org/src/contrib/rngtools_1.2.4.tar.gz http://cran.rstudio.com/src/contrib/doParallel_1.0.10.tar.gz http://cran.rstudio.com/src/contrib/NMF_0.20.6.tar.gz http://cran.r-project.org/src/contrib/igraph_1.0.1.tar.gz ; if [ ! -f registry_0.3.tar.gz ] ; then wget http://cran.rstudio.com/src/contrib/Archive/registry/registry_0.3.tar.gz ; fi ; if [ ! -f xtable_1.8-2.tar.gz ] ; then wget http://cran.rstudio.com/src/contrib/Archive/xtable/xtable_1.8-2.tar.gz ; fi ; if [ ! -f pkgmaker_0.22.tar.gz ] ; then wget http://cran.rstudio.com/src/contrib/Archive/pkgmaker/pkgmaker_0.22.tar.gz ; fi ; if [ ! -f gridBase_0.4-7.tar.gz ] ; then wget http://cran.rstudio.com/src/contrib/Archive/gridBase/gridBase_0.4-7.tar.gz ; fi ; if [ ! -f rgntools_1.2.4.tar.gz ] ; then wget https://cran.r-project.org/src/contrib/Archive/rngtools/rngtools_1.2.4.tar.gz ; fi ; if [ ! -f doParallel_1.0.10.tar.gz ] ; then wget http://cran.rstudio.com/src/contrib/Archive/doParallel/doParallel_1.0.10.tar.gz ; fi ; if [ ! -f NMF_0.20.6.tar.gz ] ; then wget http://cran.rstudio.com/src/contrib/Archive/NMF/NMF_0.20.6.tar.gz ; fi ; if [ ! -f igraph_1.0.1.tar.gz ] ; then wget http://cran.r-project.org/src/contrib/Archive/igraph/igraph_1.0.1.tar.gz ; fi ; R CMD INSTALL registry_0.3.tar.gz xtable_1.8-2.tar.gz pkgmaker_0.22.tar.gz gridBase_0.4-7.tar.gz rngtools_1.2.4.tar.gz doParallel_1.0.10.tar.gz NMF_0.20.6.tar.gz ; wget http://cran.rstudio.com/src/contrib/Archive/lattice/lattice_0.20-33.tar.gz http://cran.rstudio.com/src/contrib/Archive/Matrix/Matrix_1.2-6.tar.gz http://cran.rstudio.com/src/contrib/Archive/irlba/irlba_2.1.2.tar.gz ; R CMD INSTALL lattice_0.20-33.tar.gz Matrix_1.2-6.tar.gz irlba_2.1.2.tar.gz igraph_1.0.1.tar.gz ; wget https://cran.rstudio.com/src/contrib/Archive/TSP/TSP_1.1-4.tar.gz ; R CMD INSTALL TSP_1.1-4.tar.gz ; wget http://cran.rstudio.com/src/contrib/Archive/spam/spam_1.3-0.tar.gz http://cran.rstudio.com/src/contrib/Archive/maps/maps_3.1.1.tar.gz http://cran.rstudio.com/src/contrib/Archive/fields/fields_8.4-1.tar.gz ; R CMD INSTALL spam_1.3-0.tar.gz maps_3.1.1.tar.gz fields_8.4-1.tar.gz ; wget http://bioconductor.org/packages/3.4/bioc/src/contrib/sincell_1.6.0.tar.gz ; R CMD INSTALL sincell_1.6.0.tar.gz ; rm *.tar.gz ; apt-get remove --purge --yes wget ; apt-get clean
