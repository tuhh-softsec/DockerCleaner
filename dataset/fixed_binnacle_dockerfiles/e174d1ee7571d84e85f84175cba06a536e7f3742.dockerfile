#  using neurodebian runtime as parent image
FROM neurodebian:xenial-non-free
MAINTAINER The C-PAC Team <cnl@childmind.org>
RUN :
#   Install the validator
RUN (apt-get update ;apt-get install --no-install-recommends curl=7.47.0-1ubuntu2.19 -y ) \
 && curl -sL https://deb.nodesource.com/setup_11.x | bash - \
 && (apt-get update ;apt-get install --no-install-recommends nodejs=4.2.6~dfsg-1ubuntu4.2 -y )
RUN npm install bids-validator@1.11.0 -g
#   Install Ubuntu dependencies
RUN (apt-get update ;apt-get install --no-install-recommends build-essential=12.1ubuntu2 cmake=3.5.1-1ubuntu3 git=1:2.7.4-0ubuntu1.10 graphviz=2.38.0-12ubuntu2.1 graphviz-dev=2.38.0-12ubuntu2.1 gsl-bin=2.1+dfsg-2 libcanberra-gtk-module=0.30-2.1ubuntu1 libexpat1-dev=2.1.0-7ubuntu0.16.04.5 libgiftiio-dev=1.0.9-1 libglib2.0-dev=2.48.2-0ubuntu4.8 libglu1-mesa=9.0.0-2.1 libglu1-mesa-dev=9.0.0-2.1 libjpeg-progs=1:9b-1ubuntu1 libgl1-mesa-dri=18.0.5-0ubuntu0~16.04.1 libglw1-mesa=8.0.0-1.1 libxml2=2.9.3+dfsg1-1ubuntu0.7 libxml2-dev=2.9.3+dfsg1-1ubuntu0.7 libxext-dev=2:1.3.3-1 libxft2=2.3.2-1 libxft-dev=2.3.2-1 libxi-dev=2:1.7.6-1 libxmu-headers=2:1.1.2-2 libxmu-dev=2:1.1.2-2 libxpm-dev=1:3.5.11-1ubuntu0.16.04.1 libxslt1-dev=1.1.28-2.1ubuntu0.3 m4=1.4.17-5 make=4.1-6 mesa-common-dev=18.0.5-0ubuntu0~16.04.1 mesa-utils=8.3.0-1 netpbm=2:10.0-15.3 pkg-config=0.29.1-0ubuntu1 tcsh=6.18.01-5 unzip=6.0-20ubuntu1.1 xvfb=2:1.18.4-0ubuntu0.12 xauth=1:1.0.9-1ubuntu2 zlib1g-dev=1:1.2.8.dfsg-2ubuntu4.3 -y )
#   Install 16.04 dependencies
RUN (apt-get update ;apt-get install --no-install-recommends dh-autoreconf=11 libgsl-dev=2.1+dfsg-2 libmotif-dev=2.3.4-10 libtool=2.4.6-0.1 libx11-dev=2:1.6.3-1ubuntu2.2 libxext-dev=2:1.3.3-1 x11proto-xext-dev=7.3.0-1 x11proto-print-dev=1.0.5-2 xutils-dev=1:7.7+3ubuntu2 -y )
#   Compiles libxp- this is necessary for some newer versions of Ubuntu
#   where the is no Debian package available.
RUN git clone git://anongit.freedesktop.org/xorg/lib/libXp /tmp/libXp \
 && cd /tmp/libXp \
 && ./autogen.sh \
 && ./configure \
 && make \
 && make install \
 && cd - \
 && rm -rf /tmp/libXp
#   Installing and setting up c3d
RUN mkdir -p /opt/c3d \
 && curl -sSL "http://downloads.sourceforge.net/project/c3d/c3d/1.0.0/c3d-1.0.0-Linux-x86_64.tar.gz" | tar -xzC /opt/c3d --strip-components 1
ENV C3DPATH="/opt/c3d/"
ENV PATH="$C3DPATH/bin:$PATH"
#   install AFNI
COPY dev/docker_data/required_afni_pkgs.txt /opt/required_afni_pkgs.txt
RUN libs_path=/usr/lib/x86_64-linux-gnu \
 && if [ -f $libs_path/libgsl.so.19 ] ; then ln $libs_path/libgsl.so.19 $libs_path/libgsl.so.0 ; fi \
 && mkdir -p /opt/afni \
 && curl -sO https://afni.nimh.nih.gov/pub/dist/tgz/linux_openmp_64.tgz \
 && tar zxv -C /opt/afni --strip-components=1 -f linux_openmp_64.tgz $( cat /opt/required_afni_pkgs.txt ;) \
 && rm -rf linux_openmp_64.tgz
#   set up AFNI
ENV PATH="/opt/afni:$PATH"
#   install FSL
RUN (apt-get update ;apt-get install --no-install-recommends fsl-core=5.0.8-5 fsl-atlases fsl-mni152-templates -y )
#   setup FSL environment
ENV FSLDIR="/usr/share/fsl/5.0" \
    FSLOUTPUTTYPE="NIFTI_GZ" \
    FSLMULTIFILEQUIT="TRUE" \
    POSSUMDIR="/usr/share/fsl/5.0" \
    LD_LIBRARY_PATH="/usr/lib/fsl/5.0:$LD_LIBRARY_PATH" \
    FSLTCLSH="/usr/bin/tclsh" \
    FSLWISH="/usr/bin/wish" \
    PATH="/usr/lib/fsl/5.0:$PATH"
#   install CPAC resources into FSL
RUN curl -sL http://fcon_1000.projects.nitrc.org/indi/cpac_resources.tar.gz -o /tmp/cpac_resources.tar.gz \
 && tar xfz /tmp/cpac_resources.tar.gz -C /tmp \
 && cp -n /tmp/cpac_image_resources/MNI_3mm/* $FSLDIR/data/standard \
 && cp -n /tmp/cpac_image_resources/MNI_4mm/* $FSLDIR/data/standard \
 && cp -n /tmp/cpac_image_resources/symmetric/* $FSLDIR/data/standard \
 && cp -n /tmp/cpac_image_resources/HarvardOxford-lateral-ventricles-thr25-2mm.nii.gz $FSLDIR/data/atlases/HarvardOxford \
 && cp -nr /tmp/cpac_image_resources/tissuepriors/2mm $FSLDIR/data/standard/tissuepriors \
 && cp -nr /tmp/cpac_image_resources/tissuepriors/3mm $FSLDIR/data/standard/tissuepriors
#   install ANTs
RUN (apt-get update ;apt-get install --no-install-recommends ants -y )
#   install ICA-AROMA
RUN mkdir -p /opt/ICA-AROMA
RUN curl -sL https://github.com/rhr-pruim/ICA-AROMA/archive/v0.4.3-beta.tar.gz | tar -xzC /opt/ICA-AROMA --strip-components 1
RUN chmod +x /opt/ICA-AROMA/ICA_AROMA.py
ENV PATH="/opt/ICA-AROMA:$PATH"
#   install miniconda
RUN curl -sO https://repo.continuum.io/miniconda/Miniconda-3.8.3-Linux-x86_64.sh \
 && bash Miniconda-3.8.3-Linux-x86_64.sh -b -p /usr/local/miniconda \
 && rm Miniconda-3.8.3-Linux-x86_64.sh
#   update path to include conda
ENV PATH="/usr/local/miniconda/bin:$PATH"
#   install blas dependency first
RUN conda install -y blas
#   install conda dependencies
RUN conda install -y cython==0.26 matplotlib=2.0.2 networkx==1.11 nose==1.3.7 numpy==1.13.0 pandas==0.23.4 scipy==1.2.1 traits==4.6.0 wxpython==3.0.0.0 pip
#   install python dependencies
COPY requirements.txt /opt/requirements.txt
RUN pip install pip==9.0.1 --upgrade
RUN pip install -r /opt/requirements.txt
RUN pip install xvfbwrapper==0.2.9
#   install cpac templates
COPY dev/docker_data/cpac_templates.tar.gz /
RUN curl -s https://packagecloud.io/install/repositories/github/git-lfs/script.deb.sh | bash
RUN (apt-get update ;apt-get install --no-install-recommends git-lfs )
RUN git lfs install
#   Get atlases
RUN mkdir /ndmg_atlases \
 && GIT_LFS_SKIP_SMUDGE=1 git clone https://github.com/neurodata/neuroparc.git /tmp/neuroparc \
 && cd /tmp/neuroparc \
 && git lfs pull -I "atlases/label/*" \
 && cp -r /tmp/neuroparc/atlases/label /ndmg_atlases/label \
 && cd -
COPY dev/docker_data/default_pipeline.yml /cpac_resources/default_pipeline.yml
COPY dev/circleci_data/pipe-test_ci.yml /cpac_resources/pipe-test_ci.yml
COPY . /code
RUN pip install -e /code
COPY dev/docker_data /code/docker_data
RUN mv /code/docker_data/* /code \
 && rm -Rf /code/docker_data \
 && chmod +x /code/run.py
ENTRYPOINT ["/code/run.py"]
RUN apt-get clean \
 && apt-get autoremove -y \
 && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
