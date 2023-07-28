#   The MIT License (MIT)
#   Copyright (c) 2015 Prifysgol Bangor University
#   Permission is hereby granted, free of charge, to any person obtaining a copy
#   of this software and associated documentation files (the "Software"), to deal
#   in the Software without restriction, including without limitation the rights
#   to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
#   copies of the Software, and to permit persons to whom the Software is
#   furnished to do so, subject to the following conditions:
#   The above copyright notice and this permission notice shall be included in
#   all copies or substantial portions of the Software.
#   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
#   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
#   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
#   AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
#   LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
#   OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
#   THE SOFTWARE.
#   Datblygwyr / Developers:
#   Dewi Bryn Jones, Patrick Robertson
#
#   Rhagor / Further Information:
#   http://techiaith.cymru/cyfieithu/cyfieithu-peirianyddol/
#
FROM ubuntu:16.04
MAINTAINER Uned Technolegau Iaith, Prifysgol Bangor / Language Technologies Unit, Bangor University <techiaith@bangor.ac.uk>
RUN apt-get update \
 && apt-get install --no-install-recommends unzip=6.0-20ubuntu1.1 make=4.1-6 g++=4:5.3.1-1ubuntu1 wget=1.17.1-1ubuntu1.5 git=1:2.7.4-0ubuntu1.10 mercurial=3.7.3-1ubuntu1.2 bzip2=1.0.6-8ubuntu0.2 autotools-dev=20150820.1 automake=1:1.15-4ubuntu1 libtool=2.4.6-0.1 zlib1g-dev=1:1.2.8.dfsg-2ubuntu4.3 libbz2-dev=1.0.6-8ubuntu0.2 libboost-all-dev=1.58.0.1ubuntu1 libxmlrpc-core-c3-dev=1.33.14-1ubuntu1 libxmlrpc-c++8-dev=1.33.14-1ubuntu1 python-pip=8.1.1-2ubuntu0.6 python-setuptools=20.7.0-1 -q -y \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/*
RUN locale-gen cy_GB.UTF-8
RUN dpkg-reconfigure locales
#  ENV LANGUAGE cy_GB.UTF-8
#  ENV LANG cy_GB.UTF-8
#  ENV LC_ALL cy_GB.UTF-8 
#  RUN locale-gen cy_GB.UTF-8
RUN pip install cherrypy==7.1.0
RUN pip install python-Levenshtein==0.20.9
RUN mkdir -p /home/moses
WORKDIR /home/moses
RUN mkdir moses-smt
RUN mkdir moses-models
#   lawrlwytho/download snapshot RELEASE-3.0 moses
RUN wget https://github.com/moses-smt/mosesdecoder/archive/RELEASE-3.0.zip
RUN unzip RELEASE-3.0.zip
RUN rm RELEASE-3.0.zip
RUN mv mosesdecoder-RELEASE-3.0 mosesdecoder
RUN wget -O giza-pp.zip http://github.com/moses-smt/giza-pp/archive/master.zip
RUN unzip giza-pp.zip
RUN rm giza-pp.zip
RUN mv giza-pp-master giza-pp
WORKDIR /home/moses/giza-pp
RUN make
WORKDIR /home/moses
RUN mkdir external-bin-dir
RUN cp giza-pp/GIZA++-v2/GIZA++ external-bin-dir
RUN cp giza-pp/GIZA++-v2/snt2cooc.out external-bin-dir
RUN cp giza-pp/mkcls-v2/mkcls external-bin-dir
RUN wget -O cmph-2.0.tar.gz http://downloads.sourceforge.net/project/cmph/cmph/cmph-2.0.tar.gz
RUN tar zxvf cmph-2.0.tar.gz
WORKDIR /home/moses/cmph-2.0
RUN ./configure
RUN make
RUN make install
WORKDIR /home/moses
RUN wget -O irstlm-5.80.08.tgz http://downloads.sourceforge.net/project/irstlm/irstlm/irstlm-5.80/irstlm-5.80.08.tgz
RUN tar zxvf irstlm-5.80.08.tgz
WORKDIR /home/moses/irstlm-5.80.08/trunk
RUN /bin/bash -c "source regenerate-makefiles.sh"
RUN ./configure -prefix=/home/moses/irstlm
RUN make
RUN make install
WORKDIR /home/moses
#   Adeiladu mosesdecoder
ENV IRSTLM="/home/moses/irstlm"
WORKDIR /home/moses/mosesdecoder
RUN ./bjam -a --with-irstlm=/home/moses/irstlm --serial --with-xmlrpc-c=/usr/ --with-cmph=/home/moses/cmph-2.0
WORKDIR /home/moses/moses-smt
COPY mt_download_engine.sh /home/moses/moses-smt/mt_download_engine.sh
COPY python-server.py /home/moses/moses-smt/python-server.py
COPY moses.py /home/moses/moses-smt/moses.py
EXPOSE 8008/tcp
EXPOSE 8080/tcp
ENTRYPOINT ["python", "moses.py"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
