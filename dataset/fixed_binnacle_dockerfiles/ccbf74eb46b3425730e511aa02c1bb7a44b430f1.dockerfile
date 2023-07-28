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
#   Use a Xenial base image
FROM ubuntu:xenial
#   This DockerFile is looked after by
MAINTAINER Adam Candy <contact@shingleproject.org>
#   Repository variables from the parent environment
ARG repo="shingleproject/Shingle"
ARG branch="master"
#   Install required packages
RUN apt-get update \
 && apt-get install --no-install-recommends git=1:2.7.4-0ubuntu1.10 gcc=4:5.3.1-1ubuntu1 g++=4:5.3.1-1ubuntu1 build-essential=12.1ubuntu2 libnetcdf-dev=1:4.4.0-2 netcdf-bin=1:4.4.0-2 python-setuptools=20.7.0-1 python-dev=2.7.12-1~16.04 python-pip=8.1.1-2ubuntu0.6 python-scipy=0.17.0-1 python-numpy=1:1.11.0-1ubuntu1 python-matplotlib=1.5.1-1ubuntu1 python-shapely=1.5.13-1build1 python-pyproj=1.9.5.1-1build1 python-gdal=1.11.3+dfsg-3build2 gdal-bin=1.11.3+dfsg-3build2 python-imaging=3.1.2-0ubuntu1.6 gmsh=2.10.1+dfsg1-1ubuntu4 python-py=1.4.31-1 python-netcdf4=1.2.2-2 libgeos-c1v5=3.5.0-1ubuntu2 libgeos-3.5.0=3.5.0-1ubuntu2 libgeos-dev=3.5.0-1ubuntu2 wget=1.17.1-1ubuntu1.5 -y
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
