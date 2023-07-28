#  ###########################################################  
#   Dockerfile to build BioPerl dependencies and prerequisites  
#   Based on Ubuntu  
#  ###########################################################  
#   Set the base image to Ubuntu  
FROM ubuntu:14.04
#   File Author / Maintainer  
MAINTAINER Hilmar Lapp <hlapp@drycafe.net>  
#   Prevent error messages from debconf about non-interactive frontend  
ARG TERM=linux  
ARG DEBIAN_FRONTEND=noninteractive  
#   Update the repository sources list  
RUN :
#   Install compiler and perl stuff  
RUN (apt-get update ;apt-get install --no-install-recommends build-essential=11.6ubuntu6 gcc-multilib=4:4.8.2-1ubuntu6 perl=5.18.2-2ubuntu1.7 --yes )
#   Install perl modules  
RUN (apt-get update ;apt-get install --no-install-recommends cpanminus=1.7001-1 --yes )
RUN cpanm CPAN::Meta YAML Digest::SHA Module::Build Test::Most Test::Weaken Test::Memory::Cycle Clone
#   Install perl modules for network and SSL (and their dependencies)  
RUN (apt-get update ;apt-get install --no-install-recommends libssl-dev=1.0.1f-1ubuntu2.27 --yes )
RUN cpanm LWP LWP::Protocol::https
#   Install packages for XML processing  
RUN (apt-get update ;apt-get install --no-install-recommends expat=2.1.0-4ubuntu1.4 libexpat-dev libxml2-dev=2.9.1+dfsg1-3ubuntu4.13 libxslt1-dev=1.1.28-2ubuntu0.2 libgdbm-dev=1.8.3-12build1 --yes )
RUN cpanm XML::Parser XML::Parser::PerlSAX XML::DOM XML::DOM::XPath XML::SAX XML::SAX::Writer XML::Simple XML::Twig XML::Writer XML::LibXML XML::LibXSLT
#   Install libraries that BioPerl dependencies depend on  
RUN (apt-get update ;apt-get install --no-install-recommends libdb-dev=1:5.3.21~exp1ubuntu1 graphviz=2.36.0-0ubuntu3.2 --yes )
#   Install what counts as BioPerl dependencies  
RUN cpanm HTML::TableExtract Algorithm::Munkres Array::Compare Convert::Binary::C Error Graph GraphViz Inline::C PostScript::TextBlock Set::Scalar Sort::Naturally Math::Random Spreadsheet::ParseExcel IO::String JSON Data::Stag
#   Install database connectivity packages  
RUN (apt-get update ;apt-get install --no-install-recommends libdbi-perl=1.630-1 libdbd-mysql-perl=4.025-1ubuntu0.1 libdbd-pg-perl=2.19.3-2 libdbd-sqlite3-perl=1.40-3 --yes )
RUN cpanm DB_File
#   Install GD and other graphics dependencies  
RUN (apt-get update ;apt-get install --no-install-recommends libgd2-xpm-dev=2.1.0-3ubuntu0.11 --yes )
RUN cpanm GD SVG SVG::Graph
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
