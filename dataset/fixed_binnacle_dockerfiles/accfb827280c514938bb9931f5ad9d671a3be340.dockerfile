#   
#   Docker image for Tesseract 4 (and Leptonica) from source code
#   https://github.com/tesseract-ocr/tesseract/wiki/Compiling#linux
#   http://www.leptonica.org/source/README.html
#   based on https://github.com/tesseract-shadow/tesseract-ocr-compilation/blob/master/Dockerfile
#
FROM ubuntu:16.04
#   [ install dependencies ]
RUN apt-get update \
 && apt-get install --no-install-recommends autoconf=2.69-9 autoconf-archive=20150925-1 automake=1:1.15-4ubuntu1 build-essential=12.1ubuntu2 checkinstall=1.6.2-4ubuntu1 cmake=3.5.1-1ubuntu3 g++=4:5.3.1-1ubuntu1 git=1:2.7.4-0ubuntu1.10 libcairo2-dev=1.14.6-1 libcairo2-dev=1.14.6-1 libicu-dev=55.1-7ubuntu0.5 libicu-dev=55.1-7ubuntu0.5 libjpeg8-dev=8c-2ubuntu8 libjpeg8-dev=8c-2ubuntu8 libpango1.0-dev=1.38.1-1 libpango1.0-dev=1.38.1-1 libpng12-dev=1.2.54-1ubuntu1.1 libpng12-dev=1.2.54-1ubuntu1.1 libtiff5-dev=4.0.6-1ubuntu0.8 libtiff5-dev=4.0.6-1ubuntu0.8 libtool=2.4.6-0.1 nano=2.5.3-2ubuntu2 pkg-config=0.29.1-0ubuntu1 python-dev=2.7.12-1~16.04 python-pip=8.1.1-2ubuntu0.6 python3-dev=3.5.1-3 python3-pip=8.1.1-2ubuntu0.6 python-tk=2.7.12-1~16.04 python3-tk=3.5.1-1 screen=4.3.1-2ubuntu0.1 wget=1.17.1-1ubuntu1.5 xzgv=0.9.1-3 zlib1g-dev=1:1.2.8.dfsg-2ubuntu4.3 -y
#   [ Install ZSH ]
RUN apt-get install --no-install-recommends zsh=5.1.1-1ubuntu2.3 -y
RUN git clone git://github.com/robbyrussell/oh-my-zsh.git /root/.oh-my-zsh
RUN cp /root/.oh-my-zsh/templates/zshrc.zsh-template ~/.zshrc
RUN chsh -s /bin/zsh
#   [ Install Python packages ]
RUN pip3 install -U setuptools packaging pyparsing six cython numpy sklearn scikit-image scikit-learn imutils Pillow matplotlib argparse jupyter scipy easydict pandas tqdm xmltodict uwsgi Flask requests python-dateutil
#   [ ffmpeg ]
RUN apt-get update \
 && apt-get install --no-install-recommends ffmpeg=7:2.8.17-0ubuntu0.1 -y --upgrade
#   [ install Leptonica ]
ENV BASE_DIR="/opt"
#   install leptonica
WORKDIR ${BASE_DIR}
RUN git clone https://github.com/DanBloomberg/leptonica.git
RUN mkdir leptonica/build \
 && cd leptonica/build \
 && cmake -DCMAKE_INSTALL_PREFIX=/usr/local -DBUILD_PROG=1 .. \
 && make -j$( nproc ;) \
 && make install
WORKDIR ${BASE_DIR}
RUN git clone https://github.com/tesseract-ocr/tesseract.git
RUN mkdir tesseract/build \
 && cd tesseract/build \
 && PKG_CONFIG_PATH=/usr/local/lib/pkgconfig cmake -DCMAKE_INSTALL_PREFIX=/usr/local -DLeptonica_BUILD_DIR=/opt/leptonica/build .. \
 && make -j$( nproc ;) \
 && make install \
 && export LD_LIBRARY_PATH=/opt/tesseract/build:$LD_LIBRARY_PATH \
 && ldconfig
#   [ Download Tesseract data ]
ENV TESSDATA_PREFIX="/usr/local/share/tessdata"
RUN mkdir ${TESSDATA_PREFIX}
#   osd	Orientation and script detection
RUN wget -O ${TESSDATA_PREFIX}/osd.traineddata https://github.com/tesseract-ocr/tessdata/raw/3.04.00/osd.traineddata
#   equ	Math / equation detection
RUN wget -O ${TESSDATA_PREFIX}/equ.traineddata https://github.com/tesseract-ocr/tessdata/raw/3.04.00/equ.traineddata
#   eng English
RUN wget -O ${TESSDATA_PREFIX}/eng.traineddata https://github.com/tesseract-ocr/tessdata/raw/4.00/eng.traineddata
#   ara Arabic
RUN wget -O ${TESSDATA_PREFIX}/ara.traineddata https://github.com/tesseract-ocr/tessdata/raw/4.00/ara.traineddata
#   other languages: https://github.com/tesseract-ocr/tesseract/wiki/Data-Files
#   [ update config ]
RUN adduser --disabled-password --gecos "" docker \
 && echo 'docker ALL=NOPASSWD: ALL' >> /etc/sudoers \
 && su -c 'python3 -c "import matplotlib.pyplot"' docker \
 && echo 'ln -f /dev/null /dev/raw1394 2>/dev/null' >> /etc/bash.bashrc \
 && echo 'export PATH=/work/bin:/root/bin:${PATH}' >> /etc/bash.bashrc
#   https://github.com/tesseract-ocr/tesseract/wiki/APIExample
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
