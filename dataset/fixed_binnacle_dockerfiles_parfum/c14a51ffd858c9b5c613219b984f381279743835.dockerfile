FROM nvidia/cuda:8.0-cudnn6-devel
MAINTAINER Mario Cho "m.cho@lablup.com"
#  Add an isolated user
#  /home/work: actual working directory for user codes
#  /home/sorna: place for Python and REPL script
RUN adduser --disabled-password --gecos "" work
RUN chmod 700 /home/work
RUN mkdir /home/sorna
RUN chmod 755 /home/sorna
RUN chown -R work:work /home/sorna
ENV DEBIAN_FRONTEND="noninteractive"
ENV HOME="/home/work"
WORKDIR /home/work
#  Set up the base environment.
USER root
RUN sed -i 's/archive\.ubuntu\.com/kr.archive.ubuntu.com/' /etc/apt/sources.list
RUN echo 'APT::Install-Recommends "false";' >> /etc/apt/apt.conf; echo 'APT::Install-Suggests "false";' >> /etc/apt/apt.conf
RUN apt-get update
RUN apt-get install --no-install-recommends tzdata -y --only-upgrade
RUN apt-get install --no-install-recommends build-essential git-core curl wget ca-certificates libseccomp2 libzmq3-dev -y
#  Install common Python dependencies
RUN apt-get install --no-install-recommends libreadline-dev libsqlite3-dev libssl-dev libbz2-dev libzmq3-dev tk-dev -y
RUN apt-get install --no-install-recommends pkg-config libjpeg-dev libpng-dev -y
RUN apt-get install --no-install-recommends libjpeg-dev libpng-dev -y
RUN apt-get install --no-install-recommends libfreetype6-dev libblas-dev liblapack-dev libatlas-dev gfortran -y
CMD /home/sorna/run.sh
#  Install Python installer (pyenv)
USER work
ENV PYENV_ROOT="/home/sorna/.pyenv"
ENV PATH="/home/sorna/.pyenv/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin"
RUN git clone https://github.com/yyuu/pyenv /home/sorna/.pyenv
#  Install Python
ENV PYTHONUNBUFFERED="1"
ENV SORNA_PYTHON_VERSION="3.6.0"
COPY install-python.sh /home/sorna/install-python.sh
COPY pyenv-run.sh /home/sorna/pyenv-run.sh
RUN sh /home/sorna/install-python.sh
#  Install common Python packages
RUN /home/sorna/pyenv-run.sh pip install pyzmq namedlist simplejson six "python-dateutil>2"
RUN /home/sorna/pyenv-run.sh pip install numpy scipy matplotlib bokeh
RUN /home/sorna/pyenv-run.sh pip install scikit-learn scikit-image
RUN /home/sorna/pyenv-run.sh pip install pandas networkx cvxpy seaborn
RUN /home/sorna/pyenv-run.sh pip install pillow sklearn
#  Matplotlib configuration
USER root
ENV MPLCONFIGDIR="/home/sorna/.matplotlib"
RUN mkdir /home/sorna/.matplotlib
RUN chown -R work:work /home/sorna/.matplotlib
USER work
RUN echo 'import matplotlib.pyplot' > /tmp/matplotlib-fontcache.py
RUN /home/sorna/pyenv-run.sh python /tmp/matplotlib-fontcache.py
RUN rm /tmp/matplotlib-fontcache.py
#  Install Sorna Media support
USER work
COPY matplotlibrc /home/sorna/.matplotlib/
COPY sorna_media-*.whl /tmp
RUN /home/sorna/pyenv-run.sh pip install /tmp/sorna_media-*.whl
#  Install Theano
USER work
RUN /home/sorna/pyenv-run.sh pip install --upgrade \
 && PIP_INSTALL="pip --no-cache-dir install --upgrade" \
 && $PIP_INSTALL . \
 && $GIT_CLONE https://github.com/Theano/libgpuarray ~/gpuarray \
 && mkdir -p ~/gpuarray/build \
 && cd ~/gpuarray/build \
 && cmake -D CMAKE_BUILD_TYPE=RELEASE -D CMAKE_INSTALL_PREFIX=/usr/local .. \
 && make -j"$( nproc ;)" install \
 && cd ~/gpuarray \
 && python setup.py build \
 && python setup.py install \
 && printf '[global]\nfloatX = float32\ndevice = cuda0\n\n[dnn]\ninclude_path = /usr/local/cuda/targets/x86_64-linux/include\n' > ~/.theanorc \
 && RUN /home/sorna/pyenv-run.sh pip install keras
ENV KERAS_BACKEND="theano"
#  Secure installation scripts
USER root
COPY run.sh /home/sorna/run.sh
#  NOTE: you must copy $GOPATH/bin to <dockerfile_dir>/
COPY jail /home/sorna/jail
COPY intra-jail /home/sorna/intra-jail
RUN chown root:root /home/sorna/*.sh /home/sorna/jail /home/sorna/intra-jail
RUN chmod 755 /home/sorna/run.sh /home/sorna/jail /home/sorna/intra-jail
COPY patch-libs.so /home/sorna/patch-libs.so
ENV LD_PRELOAD="/home/sorna/patch-libs.so"
VOLUME ["/home/work"]
EXPOSE 2000/tcp 2001/tcp
LABEL io.sorna.nvidia.enabled="yes" \
      com.nvidia.cuda.version="8.0.61" \
      com.nvidia.volumes.needed="nvidia_driver" \
      io.sorna.timeout="0" \
      io.sorna.maxmem="8g" \
      io.sorna.maxcores="4" \
      io.sorna.envs.corecount="OPENBLAS_NUM_THREADS,OMP_NUM_THREADS,NPROC" \
      io.sorna.features="batch query uid-match user-input"
COPY run.py /home/sorna/run.py
USER work
