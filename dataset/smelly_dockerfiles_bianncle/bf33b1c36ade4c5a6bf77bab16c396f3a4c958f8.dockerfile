FROM ubuntu:16.04
MAINTAINER Juan Figuera
LABEL author="https://github.com/viper-framework/viper-docker"
ENV YARA_VERSION="3.4.0"
ENV SSDEEP_VERSION="2.14.1"
ENV PYEXIF_VERSION="0.2.0"
ENV ANDROGUARD_VERSION="2.0"
ENV VIPER_VERSION="1.2"
USER root
RUN apt-get update \
 && apt-get install git gcc python-dev python-pip curl libtool autoconf flex python-socksipychain python-m2crypto python-levenshtein swig libssl-dev pff-tools libimage-exiftool-perl -y \
 && rm -rf /var/lib/apt/lists/*
#  Update pip
RUN pip install pip --upgrade
#  Make Tmp Dir
RUN mkdir ~/tmp_build
#  Install Yara
RUN cd ~/tmp_build \
 && git clone -b v${YARA_VERSION} https://github.com/plusvic/yara.git \
 && cd yara \
 && bash build.sh \
 && make install \
 && cd yara-python \
 && python setup.py build \
 && python setup.py install \
 && cd ../.. \
 && rm -rf yara \
 && ldconfig
#  Install SSDEEP
RUN cd ~/tmp_build \
 && curl -sSL https://github.com/ssdeep-project/ssdeep/releases/download/release-${SSDEEP_VERSION}/ssdeep-${SSDEEP_VERSION}.tar.gz | tar -xzC . \
 && cd ssdeep-${SSDEEP_VERSION} \
 && ./configure \
 && make install \
 && pip install pydeep \
 && cd .. \
 && rm -rf ssdeep-${SSDEEP_VERSION}
#  Install PyExif
RUN cd ~/tmp_build \
 && git clone -b v${PYEXIF_VERSION} git://github.com/smarnach/pyexiftool.git \
 && cd pyexiftool \
 && python setup.py install
#  Install AndroGuard
RUN cd ~/tmp_build \
 && curl -sSL https://github.com/androguard/androguard/archive/v${ANDROGUARD_VERSION}.tar.gz | tar -xzC . \
 && cd androguard-${ANDROGUARD_VERSION} \
 && python setup.py install
#  Install TOR
# RUN apt-get install tor
#  Create Viper User
RUN groupadd -r viper \
 && useradd -r -g viper -d /home/viper -s /sbin/nologin -c "Viper User" viper \
 && mkdir /home/viper \
 && chown -R viper:viper /home/viper
#  Clean tmp_build
RUN rm -rf ~/tmp_build
#  Download viper
USER viper
WORKDIR /home/viper
RUN git clone -b v${VIPER_VERSION} https://github.com/botherder/viper.git \
 && mkdir /home/viper/workdir
#  Install core dependencies for viper
USER root
WORKDIR /home/viper/viper
RUN chmod a+xr viper.py \
 && pip install -r requirements.txt
#  Install module dependencies
USER root
WORKDIR /home/viper/viper
RUN pip install pydeep \
 && pip install editdistance
#  Get it going
USER viper
WORKDIR /home/viper/viper
RUN chmod a+xr web.py
CMD ["python", "api.py", "-H", "0.0.0.0", "-p", "8081"]
ENTRYPOINT ["python", "web.py", "-H", "0.0.0.0", "-p", "8080"]
