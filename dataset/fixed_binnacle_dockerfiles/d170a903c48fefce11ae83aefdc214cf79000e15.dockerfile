#  #############################################################################
#
#    Copyright (C) 2011-2018 Dr Adam S. Candy and others.
#    
#    Shingle:  An approach and software library for the generation of
#              boundary representation from arbitrary geophysical fields
#              and initialisation for anisotropic, unstructured meshing.
#    
#              Web: http://www.shingleproject.org
#    
#              Contact: Dr Adam S. Candy, contact@shingleproject.org
#    
#    This file is part of the Shingle project.
#    
#    Please see the AUTHORS file in the main source directory for a full list
#    of contributors.
#    
#    Shingle is free software: you can redistribute it and/or modify
#    it under the terms of the GNU Lesser General Public License as published by
#    the Free Software Foundation, either version 3 of the License, or
#    (at your option) any later version.
#    
#    Shingle is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU Lesser General Public License for more details.
#    
#    You should have received a copy of the GNU Lesser General Public License
#    along with Shingle. If not, see <http://www.gnu.org/licenses/>.
#
#  #############################################################################
#   DockerFile for a Shingle development container
#   Use a Bionic base image
FROM ubuntu:bionic
#   This DockerFile is looked after by
MAINTAINER Adam Candy <contact@shingleproject.org>
#   Repository variables from the parent environment
ARG repo="shingleproject/Shingle"
ARG branch="master"
#   Install required packages
#     Fix for non-interactive tzdata install
ENV DEBIAN_FRONTEND="noninteractive"
RUN apt-get update \
 && apt-get install --no-install-recommends git=1:2.17.1-1ubuntu0.17 gcc=4:7.4.0-1ubuntu2.3 g++=4:7.4.0-1ubuntu2.3 build-essential=12.4ubuntu1 libnetcdf-dev=1:4.6.0-2build1 netcdf-bin=1:4.6.0-2build1 python-setuptools=39.0.1-2ubuntu0.1 python-dev=2.7.15~rc1-1 python-pip=9.0.1-2.3~ubuntu1.18.04.8 python-scipy=0.19.1-2ubuntu1 python-numpy=1:1.13.3-2ubuntu1 python-matplotlib=2.1.1-2ubuntu3 python-shapely=1.6.4-1 python-pyproj=1.9.5.1-3build1 python-gdal=2.2.3+dfsg-2 gdal-bin=2.2.3+dfsg-2 python-pil=5.1.0-1ubuntu0.8 gmsh=3.0.6+dfsg1-1 python-py=1.5.2-1ubuntu0.1 python-netcdf4=1.3.1-1 libgeos-c1v5=3.6.2-1build2 libgeos-3.6.2=3.6.2-1build2 libgeos-dev=3.6.2-1build2 wget=1.19.4-1ubuntu2.2 -y
#   Upgrade pip
RUN pip install pip==23.1 setuptools==67.6.1 -i https://pypi.python.org/simple/ --upgrade
#   Install ScientificPython
RUN pip install :all:==null ScientificPython==2.9.4 --force-reinstall --ignore-installed --no-binary --no-cache-dir --no-binary
#   Update dap and shapely with recent versions
RUN pip install Pydap==3.2.1
RUN pip install geos==0.2.3 -U
RUN pip install :all:==null Shapely==1.5.9 --no-binary
#   Set build compiler environment
ENV CC="gcc"
#   Add a user
RUN adduser --disabled-password --gecos "" shingle
USER shingle
WORKDIR /home/shingle
#   Make a copy of the project Shingle
RUN git clone --depth=50 --branch="$branch" "https://github.com/${repo}" Shingle
WORKDIR /home/shingle/Shingle
#  RUN git pull
ENV PATH="/home/shingle/Shingle/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin"
RUN make
# Please add your HEALTHCHECK here!!!
