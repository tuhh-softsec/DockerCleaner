FROM phusion/baseimage:0.9.15
ENV HOME="/root"
CMD ["/sbin/my_init"]
RUN apt-get update
#  RUN apt-get install -yy libxml2
#  Install dependencies
RUN apt-get install libxml2 cmake flex g++ gfortran git ipython libatlas-base-dev libboost-all-dev libcln-dev libcppunit-dev libeigen3-dev libginac-dev liblapack-dev libmpfr-dev libopenmpi-dev petsc-dev libptscotch-dev libsuitesparse-dev openmpi-bin pkg-config python-dev python-numpy python-scientific python-pip python-six python-vtk subversion swig wget bison libhwloc-dev python-ply libvtk5-dev python-netcdf libhdf5-openmpi-dev libeigen3-dev libcgal-dev unzip -yy
#  Install sympy using pip instead of Ubuntu packages, much smaller footprint.
RUN pip install sympy
RUN apt-get install python-software-properties -y \
 && apt-get install software-properties-common -y \
 && add-apt-repository ppa:fenics-packages/fenics \
 && apt-get update \
 && apt-get install fenics -y \
 && apt-get install cython python-h5py -y
#  Cleanup to save space
RUN apt-get clean \
 && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*
#  Set the HOME environment variable, otherwise import dolfin crashes
# RUN echo "/root" > /etc/container_environment/HOME
#  Enable X11 forwarding for plotting
#  RUN echo "X11Forwarding yes" >> /etc/ssh/sshd_config
#  Set LIBGL_ALWAYS_INDIRECT to suppress libGL warning message.
# RUN echo "y" > /etc/container_environment/LIBGL_ALWAYS_INDIRECT
#  Put environment variables into
# RUN echo "source /usr/local/share/dolfin/dolfin.conf" >> /etc/bash.bashrc # not needed, causes errors
# ..................................................................................
#  Install packages for StochSS
RUN wget https://github.com/StochSS/stochss/archive/develop.tar.gz \
 && tar xfz develop.tar.gz \
 && mv stochss-develop stochss-master \
 && cd stochss-master \
 && apt-get update \
 && apt-get install curl -yy \
 && apt-get install libbz2-dev -yy \
 && apt-get install uuid-runtime libpng12-dev libffi-dev -yy \
 && pip install python-libsbml \
 && pip install boto \
 && pip install celery \
 && pip install setuptools --upgrade \
 && pip install http://cdn.mysql.com/Downloads/Connector-Python/mysql-connector-python-2.0.4.zip \
 && apt-get install gcc-multilib gcc g++ make libxml2-dev git r-base-core libgsl0-dev build-essential python-dev python-setuptools cython libbz2-dev vim docker libhdf5-mpi-dev -yy \
 && pip install docker-py \
 && chmod 777 run.ubuntu.sh \
 && ./run.ubuntu.sh --install
# md5=3df394d89300db95163f17c843ef49df
EXPOSE 8000/tcp
EXPOSE 8080/tcp
CMD ["./stochss-master/run.ubuntu.sh", "-yy", "-a", "0.0.0.0"]
