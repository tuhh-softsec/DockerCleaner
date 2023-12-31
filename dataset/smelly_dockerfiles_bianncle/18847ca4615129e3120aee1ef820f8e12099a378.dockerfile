# ###########################################################  
#  Dockerfile to build BioPerl dependencies and prerequisites  
#  Based on Ubuntu  
# ###########################################################  
#  Set the base image to Ubuntu  
FROM ubuntu:14.04
#  File Author / Maintainer  
MAINTAINER Hilmar Lapp <hlapp@drycafe.net>  
#  Prevent error messages from debconf about non-interactive frontend  
ARG TERM=linux  
ARG DEBIAN_FRONTEND=noninteractive  
#  Update the repository sources list  
RUN apt-get update
#  Install compiler and perl stuff  
RUN apt-get install build-essential gcc-multilib perl --yes
#  Install perl modules  
RUN apt-get install cpanminus --yes
RUN cpanm CPAN::Meta YAML Digest::SHA Module::Build Test::Most Test::Weaken Test::Memory::Cycle Clone
#  Install perl modules for network and SSL (and their dependencies)  
RUN apt-get install libssl-dev --yes
RUN cpanm LWP LWP::Protocol::https
#  Install packages for XML processing  
RUN apt-get install expat libexpat-dev libxml2-dev libxslt1-dev libgdbm-dev --yes
RUN cpanm XML::Parser XML::Parser::PerlSAX XML::DOM XML::DOM::XPath XML::SAX XML::SAX::Writer XML::Simple XML::Twig XML::Writer XML::LibXML XML::LibXSLT
#  Install libraries that BioPerl dependencies depend on  
RUN apt-get install libdb-dev graphviz --yes
#  Install what counts as BioPerl dependencies  
RUN cpanm HTML::TableExtract Algorithm::Munkres Array::Compare Convert::Binary::C Error Graph GraphViz Inline::C PostScript::TextBlock Set::Scalar Sort::Naturally Math::Random Spreadsheet::ParseExcel IO::String JSON Data::Stag
#  Install database connectivity packages  
RUN apt-get install libdbi-perl libdbd-mysql-perl libdbd-pg-perl libdbd-sqlite3-perl --yes
RUN cpanm DB_File
#  Install GD and other graphics dependencies  
RUN apt-get install libgd2-xpm-dev --yes
RUN cpanm GD SVG SVG::Graph
