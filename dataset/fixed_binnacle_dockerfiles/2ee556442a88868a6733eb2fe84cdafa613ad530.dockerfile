FROM ubuntu:16.04
#   set maintainer
LABEL maintainer="spalani2@jhu.edu"
#   update
RUN : \
 && apt-get -y upgrade
#   install packages
RUN (apt-get update ;apt-get install --no-install-recommends cmake=3.5.1-1ubuntu3 cpio=2.11+dfsg-5ubuntu1.1 gfortran=4:5.3.1-1ubuntu1 libpng-dev libblas-dev=3.6.0-2ubuntu2 liblapack-dev=3.6.0-2ubuntu2 libatlas-base-dev=3.10.2-9 software-properties-common=0.96.20.10 git=1:2.7.4-0ubuntu1.10 man wget=1.17.1-1ubuntu1.5 freetype* -y )
#   install python3
RUN add-apt-repository ppa:jonathonf/python-3.6
RUN :
RUN (apt-get update ;apt-get install --no-install-recommends python3.6 python3.6-dev -y )
RUN wget https://bootstrap.pypa.io/get-pip.py
RUN python3.6 get-pip.py
RUN ln -s /usr/bin/python3.6 /usr/local/bin/python
#   install MKL
#   RUN cd /tmp && \
#     wget http://registrationcenter-download.intel.com/akdlm/irc_nas/tec/11306/l_mkl_2017.2.174.tgz && \
#     tar -xzf l_mkl_2017.2.174.tgz && \
#     cd l_mkl_2017.2.174 && \
#     sed -i 's/ACCEPT_EULA=decline/ACCEPT_EULA=accept/g' silent.cfg && \
#     ./install.sh -s silent.cfg && \
#     cd .. && \
#     rm -rf *
#   RUN echo "/opt/intel/mkl/lib/intel64" >> /etc/ld.so.conf.d/intel.conf && \
#     ldconfig && \
#     echo ". /opt/intel/bin/compilervars.sh intel64" >> /etc/bash.bashrc
#   install numpy with MKL
#   RUN pip install Cython
#   RUN cd /tmp && \
#    git clone https://github.com/numpy/numpy.git numpy && \
#    cd numpy && \
#    cp site.cfg.example site.cfg && \
#    echo "\n[mkl]" >> site.cfg && \
#    echo "include_dirs = /opt/intel/mkl/include/intel64/" >> site.cfg && \
#    echo "library_dirs = /opt/intel/mkl/lib/intel64/" >> site.cfg && \
#    echo "mkl_libs = mkl_rt" >> site.cfg && \
#    echo "lapack_libs =" >> site.cfg && \
#    python setup.py build --fcompiler=gnu95 && \
#    python setup.py install && \
#    cd .. && \
#    rm -rf *
#   install scipy
#   RUN cd /tmp && \
#    git clone https://github.com/scipy/scipy.git scipy && \
#    cd scipy && \
#    python setup.py build && \
#    python setup.py install && \
#    cd .. && \
#    rm -rf *
#   make a directory for mounting local files into docker
RUN mkdir /root/workspace/
#   add vim in docker
RUN (apt-get update ;apt-get install --no-install-recommends vim=2:7.4.1689-3ubuntu1.5 -y )
#   change working directory
RUN mkdir /root/code/
WORKDIR /root/code/
#   clone the mgcpy code into the container
ARG SOURCE_BRANCH=master
RUN git clone -b ${SOURCE_BRANCH} https://github.com/neurodata/mgcpy.git .
#   install python requirements
RUN pip install -r requirements.txt
RUN pip install matplotlib==3.7.1 seaborn==0.12.2 pandas==2.0.0 jupyter==1.0.0 pycodestyle==2.10.0
#   setup pep8 guidelines (restricts push when pep8 is violated)
RUN rm -f ./.git/hooks/pre-commit
RUN chmod 777 install-hooks.sh
RUN ./install-hooks.sh
#   install mgcpy
RUN python setup.py build_ext --inplace
#   add mgcpy to PYTHONPATH for dev purposes
RUN echo "export PYTHONPATH='${PYTHONPATH}:/root/code'" >> ~/.bashrc
#   clean dir and test if mgcpy is correctly installed
RUN py3clean .
RUN python -c "import mgcpy"
#   launch terminal
CMD ["/bin/bash"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
