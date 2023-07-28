#   Data Science at the Command Line
FROM alpine:3.8
LABEL maintainer="\"Jeroen Janssens <jeroen@datascienceworkshops.com>\""
RUN apk update
RUN apk add R=3.5.0-r1 R-dev=3.5.0-r1 R-doc=3.5.0-r1 arpack-dev=3.5.0-r1 bash=4.4.19-r1 bash-doc=4.4.19-r1 bc=1.07.1-r0 bc-doc=1.07.1-r0 boost-dev=1.66.0-r0 cmake=3.11.1-r2 coreutils=8.29-r2 coreutils-doc=8.29-r2 curl=7.61.1-r3 curl-doc=7.61.1-r3 curl-dev=7.61.1-r3 findutils=4.6.0-r1 findutils-doc=4.6.0-r1 font-adobe-100dpi=1.0.3-r0 g++=6.4.0-r9 git=2.18.4-r0 git-doc=2.18.4-r0 gnuplot=5.2.2-r1 go=1.10.8-r0 grep=3.1-r2 grep-doc=3.1-r2 groff=1.22.3-r2 jpeg-dev=8-r6 jq=1.6_rc1-r1 jq-doc=1.6_rc1-r1 less=530-r0 less-doc=530-r0 libxml2-dev=2.9.8-r2 m4=1.4.18-r1 man=1.14.3-r0 man-pages=4.16-r0 mdocml-apropos=1.14.3-r0 ncurses=6.1_p20180818-r1 nodejs-lts nodejs-npm openjdk7=7.211.2.6.17-r0 openssl=1.0.2u-r0 p7zip=16.02-r3 p7zip-doc=16.02-r3 parallel=20180622-r0 parallel-doc=20180622-r0 perl-dev=5.26.3-r0 py-lxml=4.2.2-r0 py-pip python3=3.6.9-r1 python3-dev=3.6.9-r1 sed=4.4-r2 sed-doc=4.4-r2 sudo=1.8.23-r4 sudo-doc=1.8.23-r4 tar=1.32-r0 tar-doc=1.32-r0 tree=1.7.0-r1 tree-doc=1.7.0-r1 unrar=5.6.4-r0 unrar-doc=5.6.4-r0 unzip=6.0-r6 unzip-doc=6.0-r6 xmlstarlet=1.6.1-r0 xmlstarlet-doc=1.6.1-r0 zlib-dev=1.2.11-r1 --no-cache
RUN echo "install.packages(c('tidyverse','ggmap'),repos='https://cloud.r-project.org')" | R --slave --no-save --no-restore-history
RUN easy_install-3.6 pip \
 && pip3 install --upgrade pip \
 && pip3 install awscli bigmler csvkit numpy scipy nose
RUN pip3 install skll
RUN pip2 install --upgrade pip \
 && pip2 install cssselect
RUN npm install cowsay@1.5.0 xml2json-command@0.0.3 -g
#   tapkee
RUN curl -sL http://bitbucket.org/eigen/eigen/get/3.2.9.tar.gz > /tmp/eigen.tar.gz \
 && cd tmp \
 && mkdir eigen \
 && tar -xzvf eigen.tar.gz -C eigen --strip-components=1 \
 && cd eigen \
 && mkdir build \
 && cd build \
 && cmake .. \
 && make \
 && make install
RUN cd /tmp \
 && git clone https://github.com/lisitsyn/tapkee.git \
 && cd tapkee \
 && mkdir build \
 && cd build \
 && cmake .. \
 && make \
 && cp -v /tmp/tapkee/bin/tapkee /usr/bin
#   feedgnuplot
RUN yes | cpan List::MoreUtils \
 && git clone https://github.com/dkogan/feedgnuplot.git \
 && cd feedgnuplot \
 && perl Makefile.PL \
 && make \
 && make install \
 && cd .. \
 && rm -r feedgnuplot
#   pup
RUN export GOPATH=/usr \
 && go get github.com/ericchiang/pup \
 && go get github.com/jehiah/json2csv
#   csvfix
RUN curl https://bitbucket.org/neilb/csvfix/get/version-1.6.zip > /tmp/csvfix.zip \
 && cd /tmp \
 && unzip csvfix.zip \
 && mv neilb* csvfix \
 && cd csvfix \
 && make lin \
 && mv csvfix/bin/csvfix /bin
#   weka
RUN cd /tmp \
 && curl -L https://sourceforge.net/projects/weka/files/weka-3-8/3.8.1/weka-3-8-1.zip > weka.zip \
 && unzip weka.zip \
 && mv weka-3-8-1/weka.jar /bin
#   curlicue
RUN cd /tmp \
 && curl -L https://github.com/decklin/curlicue/archive/master.zip > curlicue.zip \
 && unzip curlicue.zip \
 && mv curlicue-master/curl* /bin
#   drake and drip
RUN curl -L https://raw.githubusercontent.com/Factual/drake/master/bin/drake > /usr/bin/drake \
 && chmod 755 /usr/bin/drake
RUN SHELL=/bin/bash drake ; exit 0
ENV JAVA_HOME="/usr/lib/jvm/default-jvm"
RUN ln -sf "${JAVA_HOME}/bin/"* "/usr/bin/"
RUN curl -L https://raw.githubusercontent.com/ninjudd/drip/master/bin/drip > /usr/bin/drip \
 && chmod 755 /usr/bin/drip \
 && drip ; exit 0
#   csvquote
RUN cd /tmp \
 && git clone https://github.com/dbro/csvquote.git \
 && cd csvquote \
 && make \
 && make BINDIR=/usr/bin/ install
#   vowpal wabbit
RUN cd /tmp \
 && git clone --depth 1 --branch master --single-branch git://github.com/JohnLangford/vowpal_wabbit.git \
 && cd vowpal_wabbit \
 && make \
 && make install
#   crush tools
RUN cd /tmp \
 && curl -L https://github.com/google/crush-tools/releases/download/20150716/crush-tools-20150716.tar.gz > crush-tools.tar.gz \
 && tar -xzvf crush-tools.tar.gz \
 && cd crush-tools-20150716/ \
 && sed -i '12i#include <sys/types.h>' src/fieldsplit/fieldsplit.c \
 && ./configure --prefix=/usr \
 && make \
 && make install
#   data science at the command line tools, book content, and example data
RUN cd /tmp \
 && git clone https://github.com/jeroenjanssens/data-science-at-the-command-line.git \
 && cp -v data-science-at-the-command-line/tools/* /usr/bin/ \
 && cp -rv data-science-at-the-command-line/data /home/ \
 && cp -rv data-science-at-the-command-line/book /home/
RUN rm -rf /tmp/* /var/cache/apk/*
RUN echo "export PAGER='less'" >> ~/.bashrc \
 && echo "export SHELL='/bin/bash'" >> ~/.bashrc \
 && echo "alias l='ls -lph --group-directories-first'" >> ~/.bashrc \
 && echo "alias parallel='parallel --will-cite'" >> ~/.bashrc \
 && echo 'export PS1="\[\033[38;5;6m\][\w]$\[$(tput sgr0)\] "' >> ~/.bashrc
RUN cat $( which weka ;) | sed -ne '/WEKAPATH=/,/complete /p' | cut -c3- | sed -e 's|/home/joe||' >> ~/.bashrc
RUN apk add msttcorefonts-installer=3.6-r2 fontconfig=2.12.6-r1 --no-cache \
 && update-ms-fonts \
 && fc-cache -f
RUN rm -rf /tmp/* /var/cache/apk/*
WORKDIR /data
CMD bash
RUN addgroup -S docker-user ; adduser -S -G docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
