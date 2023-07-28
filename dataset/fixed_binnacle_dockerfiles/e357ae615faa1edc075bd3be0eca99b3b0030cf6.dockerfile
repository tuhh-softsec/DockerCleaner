FROM phusion/baseimage
#   Env variables
ENV DEBIAN_FRONTEND="noninteractive"
#  Install dependencies and required requisites
RUN apt-get update -y \
 && apt-get install --no-install-recommends software-properties-common=0.99.35 -y \
 && add-apt-repository -y ppa:ubuntugis/ubuntugis-unstable \
 && add-apt-repository -y ppa:george-edison55/cmake-3.x \
 && apt-get update -y
#   All packages (Will install much faster)
RUN apt-get install --no-install-recommends build-essential=12.9ubuntu3 cmake=3.25.1-1 gdal-bin=3.6.2+dfsg-1build1 git=1:2.39.2-1ubuntu1 libatlas-base-dev=3.10.3-13ubuntu1 libavcodec-dev=7:5.1.2-3ubuntu1 libavformat-dev=7:5.1.2-3ubuntu1 libboost-date-time-dev=1.74.0.3ubuntu7 libboost-filesystem-dev=1.74.0.3ubuntu7 libboost-iostreams-dev=1.74.0.3ubuntu7 libboost-log-dev=1.74.0.3ubuntu7 libboost-python-dev=1.74.0.3ubuntu7 libboost-regex-dev=1.74.0.3ubuntu7 libboost-thread-dev=1.74.0.3ubuntu7 libeigen3-dev=3.4.0-4 libflann-dev=1.9.2+dfsg-1 libgdal-dev=3.6.2+dfsg-1build1 libgeotiff-dev=1.7.1-2build1 libgoogle-glog-dev=0.6.0-2 libgtk2.0-dev=2.24.33-2ubuntu2 libjasper-dev libjpeg-dev=8c-2ubuntu11 libjsoncpp-dev=1.9.5-4 liblapack-dev=3.11.0-2 liblas-bin libpng-dev=1.6.39-2 libproj-dev=9.1.1-1build1 libsuitesparse-dev=1:5.12.0+dfsg-2 libswscale-dev=7:5.1.2-3ubuntu1 libtbb2 libtbb-dev=2021.8.0-1ubuntu2 libtiff-dev=4.5.0-4ubuntu1 libvtk6-dev libxext-dev=2:1.3.4-1build1 python-dev python-gdal python-matplotlib python-networkx python-pip python-pyproj python-software-properties python-wheel swig2.0 grass-core=8.2.1-1build1 -y
RUN apt-get remove libdc1394-22-dev
RUN pip install pip==23.1 --upgrade
RUN pip install setuptools==67.6.1
RUN pip install appsettings==0.8 exifread==3.0.0 gpxpy==1.5.0 loky==3.4.0 numpy==1.15.4 psutil==5.9.4 pyproj==3.5.0 PyYAML==3.13 repoze.lru==0.7 scipy==1.2.1 shapely==2.0.1 xmltodict==0.13.0 rasterio==1.3.6 attrs==19.1.0 pyodm==1.5.2b1 Pillow==9.5.0 -U
RUN pip install cryptography==40.0.2 --upgrade \
 && python -m easy_install --upgrade pyOpenSSL
ENV PYTHONPATH="$PYTHONPATH:/code/SuperBuild/install/lib/python2.7/dist-packages"
ENV PYTHONPATH="$PYTHONPATH:/code/SuperBuild/src/opensfm"
ENV LD_LIBRARY_PATH="$LD_LIBRARY_PATH:/code/SuperBuild/install/lib"
#   Prepare directories
RUN mkdir /code
WORKDIR /code
#   Copy repository files
COPY CMakeLists.txt /code/CMakeLists.txt
COPY configure.sh /code/configure.sh
COPY /modules/ /code/modules/
COPY /opendm/ /code/opendm/
COPY run.py /code/run.py
COPY run.sh /code/run.sh
COPY /stages/ /code/stages/
COPY /SuperBuild/cmake/ /code/SuperBuild/cmake/
COPY /SuperBuild/CMakeLists.txt /code/SuperBuild/CMakeLists.txt
COPY docker.settings.yaml /code/settings.yaml
COPY VERSION /code/VERSION
#   Compile code in SuperBuild and root directories
RUN cd SuperBuild \
 && mkdir build \
 && cd build \
 && cmake .. \
 && make -j$( nproc ;) \
 && cd ../.. \
 && mkdir build \
 && cd build \
 && cmake .. \
 && make -j$( nproc ;)
#   Cleanup APT
RUN apt-get clean \
 && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*
#   Clean Superbuild
RUN rm -rf /code/SuperBuild/build/opencv /code/SuperBuild/download /code/SuperBuild/src/ceres /code/SuperBuild/src/mvstexturing /code/SuperBuild/src/opencv /code/SuperBuild/src/opengv /code/SuperBuild/src/pcl /code/SuperBuild/src/pdal
#   Entry point
ENTRYPOINT ["python", "/code/run.py"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
