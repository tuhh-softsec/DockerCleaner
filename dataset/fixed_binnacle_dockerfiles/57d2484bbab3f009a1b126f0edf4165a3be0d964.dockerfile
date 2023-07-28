#  Docker Static File Analysis (SFA)
FROM debian:buster
MAINTAINER Lionel PRAT <lionel.prat9@gmail.com>
RUN apt-get update \
 && apt-get install --no-install-recommends curl=7.64.0-4+deb10u5 hexedit=1.4.2-5 build-essential=12.6 git=1:2.20.1-2+deb10u8 python-pip=18.1-5 libjson-c-dev=0.12.1+ds-2+deb10u1 libxml2-dev=2.9.4+dfsg1-7+deb10u5 libssl-dev=1.1.1n-0+deb10u4 libbz2-dev=1.0.6-9.2~deb10u2 python-dev=2.7.16-1 python-virtualenv=15.1.0+ds-2+deb10u1 xdot=1.0-1 openssl=1.1.1n-0+deb10u4 vim=2:8.1.0875-5+deb10u4 zlib1g-dbg=1:1.2.11.dfsg-1+deb10u2 zlib1g=1:1.2.11.dfsg-1+deb10u2 zlib1g-dev=1:1.2.11.dfsg-1+deb10u2 cron=3.0pl1-134+deb10u1 devscripts=2.19.5+deb10u1 libtesseract-dev=4.0.0-2 tesseract-ocr-all=4.0.0-2 procyon-decompiler=0.5.32-5 python-setuptools=40.8.0-1 libboost-python-dev=1.67.0.1 libboost-all-dev=1.67.0.1 libxslt-dev libtool=2.4.6-9 graphviz-dev automake=1:1.16.1-4 libffi-dev=3.2.1-9 graphviz=2.40.1-6+deb10u1 libfuzzy-dev=2.14.1+git20180629.57fcfff-1 libjpeg-dev=1:1.5.2-2+deb10u1 pkg-config=0.29-6 autoconf=2.69-11 python-setuptools=40.8.0-1 clang=1:7.0-47 python-backports.functools-lru-cache=1.5-3 nano=3.2-3 python-m2crypto=0.31.0-4+deb10u2 python-lzma=0.5.3-4 strace=4.26-0.2 ltrace=0.7.3-6.1 wget=1.20.1-1.1 bsdmainutils=11.1.2+b1 unzip=6.0-23+deb10u3 -y
RUN curl -sL https://deb.nodesource.com/setup_11.x | bash -
RUN apt-get install --no-install-recommends nodejs=10.24.0~dfsg-1~deb10u3 -y
RUN rm -rf /var/lib/apt/lists/*
RUN cd /opt/ \
 && git clone https://github.com/lprat/static_file_analysis
RUN cd /opt/static_file_analysis \
 && git clone https://github.com/vrtadmin/clamav-devel
RUN cd /opt/static_file_analysis/clamav-devel \
 && CC=gcc CXX=c++ CFLAGS="-g -O2 -fdebug-prefix-map=/opt/static_file_analysis/clamav-devel=. -fstack-protector-strong -Wformat -Werror=format-security -D_FORTIFY_SOURCE=2" CPPFLAGS="-Wdate-time -D_FORTIFY_SOURCE=2" CXXFLAGS="-g -O2 -fdebug-prefix-map=/opt/static_file_analysis/clamav-devel=. -fstack-protector-strong -Wformat -Werror=format-security -D_FORTIFY_SOURCE=2" FCFLAGS="-g -O2 -fdebug-prefix-map=/opt/static_file_analysis/clamav-devel=. -fstack-protector-strong -D_FORTIFY_SOURCE=2" FFLAGS="-g -O2 -fdebug-prefix-map=/opt/static_file_analysis/clamav-devel=. -fstack-protector-strong -D_FORTIFY_SOURCE=2" GCJFLAGS="-g -O2 -fdebug-prefix-map=/opt/static_file_analysis/clamav-devel=. -fstack-protector-strong" LDFLAGS="-Wl,-z,relro -Wl,-z,now" OBJCFLAGS="-g -O2 -fdebug-prefix-map=/opt/static_file_analysis/clamav-devel=. -fstack-protector-strong -Wformat -Werror=format-security" OBJCXXFLAGS="-g -O2 -fdebug-prefix-map=/opt/static_file_analysis/clamav-devel=. -fstack-protector-strong -Wformat -Werror=format-security" ./configure --enable-static --with-libjson \
 && make
#  bug to install hashlib
RUN rm -f /usr/lib/python2.7/lib-dynload/_hashlib.x86_64-linux-gnu.so
WORKDIR /opt/
#  install pygrah & pyv8 for thug (compile with clang because gcc >6 make error segmentation fault
#  RUN easy_install -U setuptools pygraphviz==1.3.1
ENV CXX="clang++"
RUN python /usr/lib/python2.7/dist-packages/easy_install.py -U setuptools pygraphviz==1.3.1
#  ref: https://github.com/ibmdb/node-ibm_db/issues/276
RUN git clone https://github.com/buffer/pyv8.git
COPY v8.patch /tmp
RUN cd pyv8 \
 && python setup.py build || cd build/v8_r19632/ \
 && patch include/v8.h /tmp/v8.patch \
 && cd ../.. \
 && python setup.py build
RUN cd pyv8 \
 && python setup.py install \
 && cd .. \
 && rm -rf pyv8
COPY libemu.conf /etc/ld.so.conf.d/libemu.conf
RUN ldconfig
#  install hashlib pydot yara flask
RUN pip install wheel==0.40.0 -U \
 && pip install setuptools==67.6.1 --upgrade
RUN pip install pydot==1.4.2 yara-python==4.3.0 hashlib==20081119 flask==2.2.3 gunicorn==20.1.0 soupsieve==2.4 thug==4.9 virustotal-api==1.1.11 -U
#  RUN cp -R /usr/local/lib/python2.7/dist-packages/etc/thug /etc/thug && mkdir /etc/thug/plugins/ && mkdir /etc/thug/hooks/
#  install vmonkey
RUN pip install https://github.com/decalage2/ViperMonkey/archive/master.zip -U
#  install emulator js
RUN curl -sL https://deb.nodesource.com/setup_11.x | bash -
RUN curl -0 -L https://npmjs.org/install.sh | sh
RUN npm install box-js@1.9.22 --global
#  install foss - https://github.com/fireeye/flare-floss
RUN wget https://s3.amazonaws.com/build-artifacts.floss.flare.fireeye.com/travis/linux/dist/floss -O /usr/local/bin/foss \
 && chmod +x /usr/local/bin/foss
#  install peepdf
RUN cd /opt \
 && git clone https://github.com/jesparza/peepdf
#  install binwalk
RUN cd /opt \
 && git clone https://github.com/ReFirmLabs/binwalk \
 && cd binwalk \
 && python setup.py install
#  install frida
RUN pip install frida-tools==12.1.1 -U
#  install apktool (https://github.com/iBotPeaches/Apktool) + (https://bitbucket.org/iBotPeaches/apktool/downloads/)
RUN wget https://raw.githubusercontent.com/iBotPeaches/Apktool/master/scripts/linux/apktool -O /usr/local/bin/apktool \
 && wget https://bitbucket.org/iBotPeaches/apktool/downloads/apktool_2.4.0.jar -O /usr/local/bin/apktool.jar \
 && chmod +x /usr/local/bin/apktool*
#  install mbox (https://github.com/tsgates/mbox)
RUN cd /opt \
 && git clone https://github.com/tsgates/mbox \
 && cd mbox/src \
 && cp .configsbox.h configsbox.h \
 && ./configure \
 && make \
 && make install
#  install pyinstaller extractor https://sourceforge.net/projects/pyinstallerextractor/
RUN wget https://freefr.dl.sourceforge.net/project/pyinstallerextractor/dist/pyinstxtractor.py -O /usr/local/bin/pyinstxtractor.py \
 && chmod +x /usr/local/bin/pyinstxtractor.py
#  install unpy2exe - https://github.com/matiasb/unpy2exe & uncompyle6 https://github.com/rocky/python-uncompyle6
RUN pip install unpy2exe==0.4 uncompyle6==3.9.0 -U
#  make auto signed cert
RUN cd /opt/static_file_analysis/api \
 && openssl req -x509 -newkey rsa:4096 -nodes -out cert.pem -keyout key.pem -days 365 -subj '/CN=localdomain/O=SFA/C=FR'
#  make install wine & decompiler vb decompiler & decompiler delphi idr https://github.com/crypto2011/IDR/releases
RUN sed -i 's/ main/ main contrib/g' /etc/apt/sources.list
RUN sed -i 's/ main contrib/ main contrib non-free/' /etc/apt/sources.list
RUN dpkg --add-architecture i386 \
 && apt-get update \
 && apt-get install --no-install-recommends wine=4.0-2 wine32 winetricks unrar -y \
 && wine wineboot --init
RUN apt-get install --no-install-recommends default-jre=2:1.11-71 -y
RUN rm -rf /var/lib/apt/lists/*
RUN mkdir /opt/decompiler
COPY tools/vb_decompiler_lite.exe /opt/decompiler/
COPY tools/Exe2Aut.exe /opt/decompiler/
RUN wget https://github.com/crypto2011/IDR/releases/download/27_01_2019/Idr.exe -O /opt/decompiler/Idr.exe
#  #idr error message: cannot initialize diasm
#  install ffdec - https://github.com/jindrapetrik/jpexs-decompiler/releases -- installed by apt
RUN wget https://github.com/jindrapetrik/jpexs-decompiler/releases/download/nightly1715/ffdec_11.2.0_nightly1715.deb -O /tmp/ffdec_11.2.0_nightly1715.deb \
 && dpkg -i /tmp/ffdec_11.2.0_nightly1715.deb \
 && rm /tmp/ffdec_11.2.0_nightly1715.deb
#  add script to update git
COPY git_update.sh /opt/git_update.sh
RUN chmod 0755 /opt/git_update.sh
#   Add crontab file in the cron directory
COPY crontab_update /etc/cron.d/git-update-cron
#   Give execution rights on the cron job
RUN chmod 0644 /etc/cron.d/git-update-cron
#  add entrypoint
COPY docker-entrypoint.sh /docker-entrypoint.sh
RUN chmod 0755 /docker-entrypoint.sh
RUN useradd -ms /bin/bash user \
 && chown -R user:user /opt/static_file_analysis/
WORKDIR /opt/static_file_analysis/
USER user
RUN wine wineboot --init
EXPOSE 8000/tcp
ENTRYPOINT ["/docker-entrypoint.sh"]
#  CMD ["git pull;/bin/bash"]
# Please add your HEALTHCHECK here!!!
