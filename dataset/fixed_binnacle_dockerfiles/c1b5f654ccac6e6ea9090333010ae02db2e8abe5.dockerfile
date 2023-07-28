FROM floydhub/dl-deps:3.1.0-gpu.33
MAINTAINER Floyd Labs "support@floydhub.com"
#   From https://github.com/docker-library/python/blob/master/3.6./Dockerfile
#   ensure local python is preferred over distribution python
ENV PATH="/usr/local/bin:$PATH"
#   http://bugs.python.org/issue19846
#   > At the moment, setting "LANG=C" on a Linux system *fundamentally breaks Python 3*, and that's not OK.
ENV LANG="C.UTF-8"
ENV GPG_KEY="0D96DF4D4110E5C43FBFB17F2D347EA6AA65421D"
ENV PYTHON_VERSION="3.6.5"
RUN set -ex \
 && apt-get update \
 && apt-get install --no-install-recommends dpkg-dev tcl-dev tk-dev -y \
 && apt-get clean \
 && apt-get autoremove \
 && rm -rf /var/lib/apt/lists/* \
 && rm -rf /var/cache/apt/archives/* \
 && wget -O python.tar.xz "https://www.python.org/ftp/python/${PYTHON_VERSION%%[a-z]*}/Python-$PYTHON_VERSION.tar.xz" \
 && wget -O python.tar.xz.asc "https://www.python.org/ftp/python/${PYTHON_VERSION%%[a-z]*}/Python-$PYTHON_VERSION.tar.xz.asc" \
 && export GNUPGHOME="$( mktemp -d ;)" \
 && gpg --keyserver ha.pool.sks-keyservers.net --recv-keys "$GPG_KEY" \
 && gpg --batch --verify python.tar.xz.asc python.tar.xz \
 && rm -r "$GNUPGHOME" python.tar.xz.asc \
 && mkdir -p /usr/src/python \
 && tar -xJC /usr/src/python --strip-components=1 -f python.tar.xz \
 && rm python.tar.xz \
 && cd /usr/src/python \
 && gnuArch="$( dpkg-architecture --query DEB_BUILD_GNU_TYPE ;)" \
 && ./configure --build="$gnuArch" --with-tcltk-includes='-I/usr/include/tk -I/usr/include/tcl' --with-tcltk-libs='/usr/lib/x86_64-linux-gnu/libtcl.so /usr/lib/x86_64-linux-gnu/libtk.so' --enable-loadable-sqlite-extensions --enable-shared --with-system-expat --with-system-ffi --without-ensurepip \
 && make -j$( nproc ;) \
 && make install \
 && ldconfig \
 && find /usr/local -depth
#   make some useful symlinks that are expected to exist
RUN cd /usr/local/bin \
 && { [ -e easy_install ] || ln -s easy_install-* easy_install ; } \
 && ln -s idle3 idle \
 && ln -s pydoc3 pydoc \
 && ln -s python3 python \
 && ln -s python3-config python-config
#   if this is called "PIP_VERSION", pip explodes with "ValueError: invalid truth value '<VERSION>'"
ENV PYTHON_PIP_VERSION="10.0.1"
RUN set -ex ; wget -O get-pip.py 'https://bootstrap.pypa.io/get-pip.py' ; python get-pip.py --disable-pip-version-check --no-cache-dir "pip==$PYTHON_PIP_VERSION" ; pip --version ; find /usr/local -depth
RUN pip install virtualenv==20.21.0 PyOpenGL==3.1.6 PyOpenGL_accelerate==3.1.6 Cython==0.29.34 h5py==3.8.0 jupyter==1.0.0 notebook==5.6.0 numpy==1.24.2 cupy==12.0.0 pandas==2.0.0 matplotlib==3.7.1 ipykernel==6.22.0 path.py==12.5.0 pyyaml==6.0 scipy==1.10.1 six==1.16.0 sklearn==0.0.post4 sympy==1.11.1 Pillow==9.5.0 zmq==0.0.0 opencv-contrib-python==3.4.0.12 --no-cache-dir \
 && rm -rf /tmp/* /var/tmp/* \
 && python -m ipykernel.kernelspec
#   Set up our notebook config.
COPY jupyter_notebook_config_py3.py /root/.jupyter/
RUN mv /root/.jupyter/jupyter_notebook_config_py3.py /root/.jupyter/jupyter_notebook_config.py
#   Create the file configurations for IPython
#   and remove the default one.
RUN ipython profile create \
 && rm /root/.ipython/profile_default/ipython_config.py
#   Set up our IPython config.
COPY ipython_config.py /root/.ipython/profile_default/ipython_config.py
#   Jupyter has issues with being run directly:
#     https://github.com/ipython/ipython/issues/7062
#   We just add a little wrapper script.
COPY run_jupyter.sh /
RUN chmod +x /run_jupyter.sh
#   IPython
EXPOSE 8888/tcp
CMD ["/run_jupyter.sh"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
