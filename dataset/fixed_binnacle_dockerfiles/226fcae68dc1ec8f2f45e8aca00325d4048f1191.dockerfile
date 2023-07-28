#  ################################################################
#   Dockerfile
#
#   Version:          1.0
#   Software:         OptiType
#   Software Version: 1.3
#   Description:      Accurate NGS-based 4-digit HLA typing
#   Website:          https://github.com/FRED-2/OptiType/
#   Tags:             Genomics
#   Provides:         OptiType 1.3
#   Base Image:       biodckr/biodocker
#   Build Cmd:        docker build --rm -t fred2/opitype .
#   Pull Cmd:         docker pull fred2/optitype
#   Run Cmd:          docker run -v /path/to/file/dir:/data fred2/optitype
#  ################################################################
#   Source Image
FROM biocontainers/biocontainers:latest
#  ################# BEGIN INSTALLATION ###########################
USER root
#   install
RUN apt-get update \
 && apt-get install --no-install-recommends software-properties-common=0.99.35 -y \
 && apt-get update \
 && apt-get install --no-install-recommends gcc-4.9 g++-4.9 coinor-cbc=2.10.8+ds1-1 zlib1g-dev=1:1.2.13.dfsg-1ubuntu4 libbz2-dev=1.0.8-5build1 -y \
 && update-alternatives --install /usr/bin/gcc gcc /usr/bin/gcc-4.9 60 --slave /usr/bin/g++ g++ /usr/bin/g++-4.9 \
 && rm -rf /var/lib/apt/lists/* \
 && apt-get clean \
 && apt-get purge
#  HLA Typing
#  OptiType dependecies
RUN curl -O https://support.hdfgroup.org/ftp/HDF5/current18/bin/hdf5-1.8.21-Std-centos7-x86_64-shared_64.tar.gz \
 && tar -xvf hdf5-1.8.21-Std-centos7-x86_64-shared_64.tar.gz \
 && mv hdf5-1.8.21-Std-centos7-x86_64-shared_64/bin/* /usr/local/bin/ \
 && mv hdf5-1.8.21-Std-centos7-x86_64-shared_64/lib/* /usr/local/lib/ \
 && mv hdf5-1.8.21-Std-centos7-x86_64-shared_64/include/* /usr/local/include/ \
 && mv hdf5-1.8.21-Std-centos7-x86_64-shared_64/share/* /usr/local/share/ \
 && rm -rf hdf5-1.8.21-Std-centos7-x86_64-shared_64/ \
 && rm -f hdf5-1.8.21-Std-centos7-x86_64-shared_64.tar.gz
ENV LD_LIBRARY_PATH="/usr/local/lib:$LD_LIBRARY_PATH"
ENV HDF5_DIR="/usr/local/"
RUN pip install pip==23.1 --upgrade \
 && pip install numpy==1.24.2 pyomo==6.5.0 pysam==0.21.0 matplotlib==3.7.1 tables==3.8.0 pandas==2.0.0 future==0.18.3
#  installing optitype form git repository (version Dec 09 2015) and wirtig config.ini
RUN git clone https://github.com/FRED-2/OptiType.git \
 && sed -i -e '1i#!/usr/bin/env python\' OptiType/OptiTypePipeline.py \
 && mv OptiType/ /usr/local/bin/ \
 && chmod 777 /usr/local/bin/OptiType/OptiTypePipeline.py \
 && echo "[mapping]\nrazers3=/usr/local/bin/razers3 \nthreads=1 \n\n[ilp]\nsolver=cbc \nthreads=1 \n\n[behavior]\ndeletebam=true \nunpaired_weight=0 \nuse_discordant=false\n" >> /usr/local/bin/OptiType/config.ini
#  installing razers3
RUN git clone https://github.com/seqan/seqan.git seqan-src \
 && cd seqan-src \
 && cmake -DCMAKE_BUILD_TYPE=Release \
 && make razers3 \
 && cp bin/razers3 /usr/local/bin \
 && cd .. \
 && rm -rf seqan-src
ENV PATH="/usr/local/bin/OptiType:$PATH"
#   Change user to back to biodocker
USER biodocker
#   Change workdir to /data/
WORKDIR /data/
#   Define default command
ENTRYPOINT ["OptiTypePipeline.py"]
CMD ["-h"]
#  #################### INSTALLATION END ##########################
#   File Author / Maintainer
MAINTAINER Benjamin Schubert <schubert@informatik.uni-tuebingen.de>
# Please add your HEALTHCHECK here!!!
