#  BSD 3-Clause License
#  Copyright (c) 2017, Juliano Petronetto
#  All rights reserved.
#  Redistribution and use in source and binary forms, with or without
#  modification, are permitted provided that the following conditions are met:
#  * Redistributions of source code must retain the above copyright notice, this
#    list of conditions and the following disclaimer.
#  * Redistributions in binary form must reproduce the above copyright notice,
#    this list of conditions and the following disclaimer in the documentation
#    and/or other materials provided with the distribution.
#  * Neither the name of the copyright holder nor the names of its
#    contributors may be used to endorse or promote products derived from
#    this software without specific prior written permission.
#  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
#  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
#  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
#  DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
#  FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
#  DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
#  SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
#  CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
#  OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
#  OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
FROM debian:stretch-slim
LABEL maintainer="Juliano Petronetto <juliano@petronetto.com.br>" \
      name="Docker Python Deep Learning" \
      description="Docker container for Python Deep Learning, with almost everything that you may need." \
      url="https://hub.docker.com/r/petronetto/docker-python-deep-learning" \
      vcs-url="https://github.com/petronetto/docker-python-deep-learning" \
      vendor="Petronetto DevTech" \
      version="1.1"
ENV BUILD_PACKAGES=" build-essential  linux-headers-4.9  cmake  tcl-dev  xz-utils  zlib1g-dev  libssl-dev  libncurses5-dev  libsqlite3-dev  libreadline-dev  libtk8.5  libgdm-dev  libdb4o-cil-dev  libpcap-dev  git  wget  curl" \
    APT_PACKAGES=" ca-certificates  openssl  sqlite3  bash  graphviz  fonts-noto  libpng16-16  libfreetype6  libjpeg62-turbo  libgomp1" \
    PIP_PACKAGES=" pyyaml  pymkl  cffi  h5py  requests  pillow  graphviz  numpy  pandas  scipy  scikit-learn  seaborn  matplotlib  jupyter  xgboost  tensorflow  keras  https://download.pytorch.org/whl/cpu/torch-1.1.0-cp36-cp36m-linux_x86_64.whl  https://download.pytorch.org/whl/cpu/torchvision-0.3.0-cp36-cp36m-linux_x86_64.whl  mxnet-mkl" \
    PYTHON_VER="3.6.8" \
    JUPYTER_CONFIG_DIR="/home/.ipython/profile_default/startup" \
    LANG="C.UTF-8"
RUN set -ex ; apt-get update -y ; apt-get upgrade -y ; apt-get install --no-install-recommends ${APT_PACKAGES} -y ; apt-get install --no-install-recommends ${BUILD_PACKAGES} -y ; cd /tmp \
 && wget https://www.python.org/ftp/python/${PYTHON_VER}/Python-${PYTHON_VER}.tgz ; tar xvf Python-${PYTHON_VER}.tgz ; cd Python-${PYTHON_VER} ; ./configure --enable-optimizations \
 && make -j8 \
 && make altinstall ; ln -s /usr/local/bin/python3.6 /usr/local/bin/python ; ln -s /usr/local/bin/pip3.6 /usr/local/bin/pip ; ln -s /usr/local/bin/idle3.6 /usr/local/bin/idle ; ln -s /usr/local/bin/pydoc3.6 /usr/local/bin/pydoc ; ln -s /usr/local/bin/python3.6m-config /usr/local/bin/python-config ; ln -s /usr/local/bin/pyvenv-3.6 /usr/local/bin/pyvenv ; pip install pip -U -V ; pip install setuptools wheel -U -v ; pip install ${PIP_PACKAGES} -U -v ; apt-get remove --purge --auto-remove -y ${BUILD_PACKAGES} ; apt-get clean ; apt-get autoclean ; apt-get autoremove ; rm -rf /tmp/* /var/tmp/* ; rm -rf /var/lib/apt/lists/* ; rm -f /var/cache/apt/archives/*.deb /var/cache/apt/archives/partial/*.deb /var/cache/apt/*.bin ; find / -name __pycache__ | xargs rm -r ; rm -rf /root/.[acpw]* ; pip install jupyter \
 && jupyter nbextension enable --py widgetsnbextension ; mkdir -p ${JUPYTER_CONFIG_DIR} ; echo "import warnings" | tee ${JUPYTER_CONFIG_DIR}/config.py ; echo "warnings.filterwarnings('ignore')" | tee -a ${JUPYTER_CONFIG_DIR}/config.py ; echo "c.NotebookApp.token = u''" | tee -a ${JUPYTER_CONFIG_DIR}/config.py
WORKDIR /home/notebooks
EXPOSE 8888/tcp
CMD ["jupyter", "notebook", "--port=8888", "--no-browser", "--allow-root", "--ip=0.0.0.0", "--NotebookApp.token="]
