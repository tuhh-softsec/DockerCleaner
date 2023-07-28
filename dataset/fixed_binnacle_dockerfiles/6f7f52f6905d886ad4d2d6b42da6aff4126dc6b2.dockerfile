#   
#   Docker image for Tesseract 4 (and Leptonica) from source code
#   https://github.com/tesseract-ocr/tesseract/wiki/Compiling#linux
#   http://www.leptonica.org/source/README.html
#   based on https://github.com/tesseract-shadow/tesseract-ocr-compilation/blob/master/Dockerfile
#
FROM ubuntu:18.04
ENV DOCKER_USER="adam"
RUN groupadd -r ${DOCKER_USER} \
 && useradd -r -g ${DOCKER_USER} ${DOCKER_USER}
USER ${DOCKER_USER}
ENV DEBIAN_FRONTEND="noninteractive"
#   [ install dependencies ]
RUN apt-get update \
 && apt-get install --no-install-recommends nano=2.9.3-2 pkg-config=0.29.1-0ubuntu2 python-dev=2.7.15~rc1-1 python-pip=9.0.1-2.3~ubuntu1.18.04.8 python3-dev=3.6.7-1~18.04 python3-pip=9.0.1-2.3~ubuntu1.18.04.8 python-tk=2.7.17-1~18.04 python3-tk=3.6.9-1~18.04 screen=4.6.2-1ubuntu1.1 wget=1.19.4-1ubuntu2.2 git=1:2.17.1-1ubuntu0.17 -y --
#   [ install ocr ]
RUN apt-get install --no-install-recommends tesseract-ocr=4.00~git2288-10f4998a-2 -y
#   [ Install ZSH ]
RUN apt-get install --no-install-recommends zsh=5.4.2-3ubuntu3.2 -y
RUN git clone git://github.com/robbyrussell/oh-my-zsh.git /root/.oh-my-zsh
RUN cp /root/.oh-my-zsh/templates/zshrc.zsh-template ~/.zshrc
RUN chsh -s /bin/zsh
#   [ Install Python packages ]
RUN pip3 install -U setuptools packaging pyparsing six cython numpy sklearn scikit-image scikit-learn imutils Pillow matplotlib argparse jupyter scipy easydict pandas tqdm xmltodict uwsgi Flask requests python-dateutil
#   [ ffmpeg ]
RUN apt-get update \
 && apt-get install --no-install-recommends ffmpeg=7:3.4.11-0ubuntu0.1 -y --upgrade
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
# Please add your HEALTHCHECK here!!!
