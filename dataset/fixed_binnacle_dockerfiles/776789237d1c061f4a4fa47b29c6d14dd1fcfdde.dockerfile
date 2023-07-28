#   Copyright (C) 2017-2018 COAL Developers
#
#   This program is free software; you can redistribute it and/or 
#   modify it under the terms of the GNU General Public License 
#   as published by the Free Software Foundation; version 2.
#
#   This program is distributed in the hope that it will be useful, 
#   but WITHOUT ANY WARRANTY; without even the implied warranty 
#   of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 
#   See the GNU General Public License for more details.
#
#   You should have received a copy of the GNU General Public 
#   License along with this program; if not, write to the Free 
#   Software Foundation, Inc., 51 Franklin Street, Fifth 
#   Floor, Boston, MA 02110-1301, USA.
#   Use an official Python runtime as a base image (host debian:jessie)
FROM python:3.6-slim
MAINTAINER COAL Developers <coal-capstone@googlegroups.com>
#   Build-time metadata as defined at http://label-schema.org
#   This means we get badges through MicroBadger
ARG BUILD_DATE
ARG VCS_REF
LABEL org.label-schema.build-date="$BUILD_DATE" \
      org.label-schema.name="Coal and Open-pit surface mining impacts on American Lands (COAL)" \
      org.label-schema.description="Python library for processing hyperspectral imagery from the Airborne Visible/InfraRed Imaging Spectrometer (AVIRIS). COAL provides a suite of algorithms for classifying land cover, identifying mines and other geographic features, and correlating them with environmental data sets." \
      org.label-schema.url="https://capstone-coal.github.io/" \
      org.label-schema.vcs-ref="$VCS_REF" \
      org.label-schema.vcs-url="https://github.com/capstone-coal/pycoal" \
      org.label-schema.vendor="Capstone Coal" \
      org.label-schema.schema-version="1.0"
#   Install the dependencies
RUN apt-get update \
 && apt-get upgrade -y --force-yes \
 && apt-get install --no-install-recommends apache2=2.4.56-1~deb11u1 bash-completion=1:2.11-2 bison=2:3.7.5+dfsg-1 checkinstall=1.6.2+git20170426.d24a630-2 cmake=3.18.4-2+deb11u1 devscripts=2.21.3+deb11u1 doxygen=1.9.1-1 flex=2.6.4-8 git=1:2.30.2-1+deb11u2 graphviz=2.42.2-5 grass-dev=7.8.5-1+deb11u1 libexpat1-dev=2.2.10-2+deb11u5 libfcgi-dev=2.4.2-2 libgdal-dev=3.2.2+dfsg-2+deb11u2 libgeos-dev=3.9.0-1 libgsl0-dev libopenscenegraph-dev=3.6.5+dfsg1-7+b1 libosgearth-dev libpq-dev=13.9-0+deb11u1 libproj-dev=7.2.1-1 libqt4-dev libqt4-opengl-dev libqtwebkit-dev libqwt-dev libspatialindex-dev=1.9.3-2 libspatialite-dev=5.0.1-2 libsqlite3-dev=3.34.1-3 pkg-config=0.29.2-1 pkg-kde-tools=0.15.32 pyqt4-dev-tools python-all=2.7.18-3 python-all-dev=2.7.18-3 python-qgis python-qt4 python-qt4-dev python-sip python-sip-dev qgis=3.10.14+dfsg-1 qgis-plugin-grass=3.10.14+dfsg-1 txt2tags=3.4-2 wget=1.21-1+deb11u1 xauth=1:1.1-1 xfonts-100dpi=1:1.0.4+nmu1.1 xfonts-75dpi=1:1.0.4+nmu1.1 xfonts-base=1:1.0.5 xfonts-scalable=1:1.0.3-1.2 xvfb=2:1.20.11-1+deb11u6 -y --force-yes
RUN echo "deb http://qgis.org/debian jessie main" >> /etc/apt/sources.list
RUN echo "deb-src http://qgis.org/debian jessie main" >> /etc/apt/sources.list
RUN apt-key adv --keyserver keyserver.ubuntu.com --recv-key 073D307A618E5811
#   dput breaks Docker build
RUN printf "Package: dput\nPin: origin \"\"\nPin-Priority: -1" >> /etc/apt/preferences
#  RUN add-apt-repository -y ppa:ubuntugis/ubuntugis-unstable
#   Download GDAL
RUN wget http://download.osgeo.org/gdal/2.3.1/gdal-2.3.1.tar.gz \
 && tar zxvf gdal-2.3.1.tar.gz \
 && cd gdal-2.3.1 \
 && ./configure \
 && make \
 && make install \
 && ldconfig \
 && gdalwarp --version
#   Set the working directory to /coal
WORKDIR /coal
#   Copy the current directory contents into the container at /coal
COPY . /coal
#   Install pycoal from source, ensures we always use the latest development branch
RUN python setup.py install
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
