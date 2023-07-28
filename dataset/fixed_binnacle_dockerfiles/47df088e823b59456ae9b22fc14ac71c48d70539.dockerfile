#   Generated by Porcupine
#   Modelled after Neurodocker v0.2.0-dev. https://github.com/kaczmarj/neurodocker
FROM timvanmourik/porcupine
#  ----------------------------------------------------------
#   Install common dependencies and create default entrypoint
#  ----------------------------------------------------------
ARG DEBIAN_FRONTEND=noninteractive
ENV LANG="C.UTF-8" \
    LC_ALL="C" \
    ND_ENTRYPOINT="/porcupine/startup.sh"
RUN apt-get update -qq \
 && apt-get install --no-install-recommends bzip2=1.0.8-5build1 ca-certificates=20230311 curl=7.88.1-7ubuntu1 unzip=6.0-27ubuntu1 -yq \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/* \
 && chmod 777 /opt \
 && chmod a+s /opt \
 && mkdir /porcupine \
 && echo '#!/usr/bin/env bash' >> $ND_ENTRYPOINT \
 && echo 'set +x' >> $ND_ENTRYPOINT \
 && echo 'if [ -z "$*" ]; then /usr/bin/env bash; else $*; fi' >> $ND_ENTRYPOINT \
 && chmod -R 777 /porcupine \
 && chmod a+s /porcupine
ENTRYPOINT ["/porcupine/startup.sh"]
#  ---------------
#   Install Nipype
#  ---------------
RUN curl -sSLO https://repo.continuum.io/miniconda/Miniconda3-4.3.11-Linux-x86_64.sh \
 && bash Miniconda3-4.3.11-Linux-x86_64.sh -b -p /usr/local/miniconda \
 && rm Miniconda3-4.3.11-Linux-x86_64.sh
ENV PATH="/usr/local/miniconda/bin:$PATH" \
    LANG="C.UTF-8" \
    LC_ALL="C.UTF-8"
#   Installing precomputed python packages
RUN conda install -y mkl=2017.0.1 mkl-service \
 && conda install -y numpy=1.12.0 scipy=0.18.1 scikit-learn=0.18.1 matplotlib=2.0.0 pandas=0.19.2 libxml2=2.9.4 libxslt=1.1.29 traits=4.6.0 boto
RUN chmod +x /usr/local/miniconda/bin/* \
 && conda clean --all -y
RUN echo 'export PATH=/usr/local/miniconda/bin:$PATH' >> /etc/profile
RUN pip install nipype==1.8.6
RUN apt-get update \
 && apt-get install --no-install-recommends graphviz=2.42.2-7build3 -y
#  -------------------
#   Install ANTs 2.2.0
#  -------------------
RUN echo "Downloading ANTs ..." \
 && curl -sSL --retry 5 https://dl.dropbox.com/s/2f4sui1z6lcgyek/ANTs-Linux-centos5_x86_64-v2.2.0-0740f91.tar.gz | tar zx -C /opt
ENV ANTSPATH="/opt/ants" \
    PATH="/opt/ants:$PATH"
#  --------------------
#   Install AFNI latest
#  --------------------
ENV PATH="/opt/afni:$PATH"
RUN apt-get update -qq \
 && apt-get install --no-install-recommends ed=1.19-1 gsl-bin=2.7.1+dfsg-3 libglu1-mesa-dev=9.0.2-1.1 libglib2.0-0=2.76.0-1ubuntu1 libglw1-mesa=8.0.0-1.1build1 libgomp1=13-20230320-1ubuntu1 libjpeg62=1:6b2-3.1 libxm4=2.3.8-3 netpbm=2:11.01.00-2build1 tcsh=6.24.07-1 xfonts-base=1:1.0.5+nmu1 xvfb=2:21.1.7-1ubuntu2 -yq \
 && libs_path=/usr/lib/x86_64-linux-gnu \
 && if [ -f $libs_path/libgsl.so.19 ] ; then ln $libs_path/libgsl.so.19 $libs_path/libgsl.so.0 ; fi \
 && apt-get install --no-install-recommends libxp6 -yq || /bin/bash -c " curl --retry 5 -o /tmp/libxp6.deb -sSL http://mirrors.kernel.org/debian/pool/main/libx/libxp/libxp6_1.0.2-2_amd64.deb \
 && dpkg -i /tmp/libxp6.deb \
 && rm -f /tmp/libxp6.deb" \
 && apt-get install --no-install-recommends libpng12-0 -yq || /bin/bash -c " curl -o /tmp/libpng12.deb -sSL http://mirrors.kernel.org/debian/pool/main/libp/libpng/libpng12-0_1.2.49-1%2Bdeb7u2_amd64.deb \
 && dpkg -i /tmp/libpng12.deb \
 && rm -f /tmp/libpng12.deb" \
 && apt-get install --no-install-recommends r-base-dev=4.2.2.20221110-2build1 r-cran-rmpi=0.6-9.2-2 -yq || /bin/bash -c " curl -o /tmp/install_R.sh -sSL https://gist.githubusercontent.com/kaczmarj/8e3792ae1af70b03788163c44f453b43/raw/0577c62e4771236adf0191c826a25249eb69a130/R_installer_debian_ubuntu.sh \
 && /bin/bash /tmp/install_R.sh" \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/* \
 && echo "Downloading AFNI ..." \
 && mkdir -p /opt/afni \
 && curl -sSL --retry 5 https://afni.nimh.nih.gov/pub/dist/tgz/linux_openmp_64.tgz | tar zx -C /opt/afni --strip-components=1 \
 && /opt/afni/rPkgsInstall -pkgs ALL \
 && rm -rf /tmp/*
#  -----------------------------------------------------------
#   Install FSL v5.0.10
#   FSL is non-free. If you are considering commerical use
#   of this Docker image, please consult the relevant license:
#   https://fsl.fmrib.ox.ac.uk/fsl/fslwiki/Licence
#  -----------------------------------------------------------
RUN echo "Downloading FSL ..." \
 && curl -sSL https://fsl.fmrib.ox.ac.uk/fsldownloads/fsl-5.0.10-centos6_64.tar.gz | tar zx -C /opt \
 && /bin/bash /opt/fsl/etc/fslconf/fslpython_install.sh -q -f /opt/fsl \
 && sed -i '$iecho Some packages in this Docker container are non-free' $ND_ENTRYPOINT \
 && sed -i '$iecho If you are considering commercial use of this container, please consult the relevant license:' $ND_ENTRYPOINT \
 && sed -i '$iecho https://fsl.fmrib.ox.ac.uk/fsl/fslwiki/Licence' $ND_ENTRYPOINT \
 && sed -i '$isource $FSLDIR/etc/fslconf/fsl.sh' $ND_ENTRYPOINT
ENV FSLDIR="/opt/fsl" \
    PATH="/opt/fsl/bin:$PATH"
#  --------------------------------
#   Add custom analysis file as CMD
#  --------------------------------
COPY ./porcupine_generated_pipeline.py /run.py
CMD ["python", "/run.py"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!