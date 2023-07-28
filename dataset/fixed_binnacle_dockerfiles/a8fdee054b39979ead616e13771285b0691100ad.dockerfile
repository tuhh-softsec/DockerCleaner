#  AUTHOR Aviral Takkar <aviraltakkar@cs.ucsb.edu>
FROM phusion/baseimage:0.9.15
MAINTAINER Aviral Takkar
ENV HOME="/root"
CMD ["/sbin/my_init"]
RUN :
#   RUN apt-get install -yy libxml2
#   Install dependencies
RUN (apt-get update ;apt-get install --no-install-recommends libxml2 cmake flex g++ gfortran git ipython libatlas-base-dev libboost-all-dev libcln-dev libcppunit-dev libeigen3-dev libginac-dev liblapack-dev libmpfr-dev libopenmpi-dev petsc-dev libptscotch-dev libsuitesparse-dev openmpi-bin pkg-config python-dev python-numpy python-scientific python-pip python-six python-vtk subversion swig wget bison libhwloc-dev python-ply libvtk5-dev python-netcdf libhdf5-openmpi-dev libeigen3-dev libcgal-dev unzip -yy )
#   TODO: Parmetis, SLEPC, PETSc4py, PASTIX?
#   Trilinos has been deprecated, for now.
#   Sphinx is not necessary for this build.
#   Install sympy using pip instead of Ubuntu packages, much smaller footprint.
RUN pip install sympy==1.11.1
RUN (apt-get update ;apt-get install --no-install-recommends python-software-properties -y ) \
 && (apt-get update ;apt-get install --no-install-recommends software-properties-common -y ) \
 && add-apt-repository ppa:fenics-packages/fenics \
 && apt-get update \
 && (apt-get update ;apt-get install --no-install-recommends fenics -y ) \
 && (apt-get update ;apt-get install --no-install-recommends cython python-h5py -y )
#   Cleanup to save space
RUN apt-get clean \
 && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*
#   Set the HOME environment variable, otherwise import dolfin crashes
#  RUN echo "/root" > /etc/container_environment/HOME
#   Enable X11 forwarding for plotting
#   RUN echo "X11Forwarding yes" >> /etc/ssh/sshd_config
#   Set LIBGL_ALWAYS_INDIRECT to suppress libGL warning message.
#  RUN echo "y" > /etc/container_environment/LIBGL_ALWAYS_INDIRECT
#   Put environment variables into
#  RUN echo "source /usr/local/share/dolfin/dolfin.conf" >> /etc/bash.bashrc # not needed, causes errors
#  ..................................................................................
#   Install packages for StochSS
RUN wget https://github.com/StochSS/stochss/archive/develop.tar.gz \
 && tar xfz develop.tar.gz \
 && mv stochss-develop stochss-master \
 && cd stochss-master \
 && apt-get update \
 && (apt-get update ;apt-get install --no-install-recommends curl -yy ) \
 && (apt-get update ;apt-get install --no-install-recommends libbz2-dev -yy ) \
 && (apt-get update ;apt-get install --no-install-recommends uuid-runtime libpng12-dev libffi-dev -yy ) \
 && pip install python-libsbml==5.20.0 \
 && pip install boto==2.49.0 \
 && pip install celery==5.2.7 \
 && pip install setuptools==67.6.1 --upgrade \
 && pip install http://cdn.mysql.com/Downloads/Connector-Python/mysql-connector-python-2.0.4.zip \
 && (apt-get update ;apt-get install --no-install-recommends gcc-multilib gcc g++ make libxml2-dev git r-base-core libgsl0-dev build-essential python-dev python-setuptools cython libbz2-dev vim docker libhdf5-mpi-dev -yy ) \
 && pip install docker-py==1.10.6 -yy \
 && chmod 777 run.ubuntu.sh \
 && ./run.ubuntu.sh --install
#  md5=3df394d89300db95163f17c843ef49df
EXPOSE 8000/tcp
EXPOSE 8080/tcp
CMD ["./stochss-master/run.ubuntu.sh", "-yy", "-a", "0.0.0.0"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
