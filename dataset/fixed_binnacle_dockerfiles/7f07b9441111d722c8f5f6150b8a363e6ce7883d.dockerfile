FROM ubuntu:16.04
MAINTAINER Tiago Antao <tra@popgen.net>
ENV DEBIAN_FRONTEND="noninteractive"
#  We need this for phylip
RUN echo 'deb http://archive.ubuntu.com/ubuntu xenial multiverse' >> /etc/apt/sources.list \
 && apt-get update \
 && apt-get upgrade -y --force-yes \
 && apt-get install --no-install-recommends build-essential=12.1ubuntu2 git=1:2.7.4-0ubuntu1.10 python3-numpy=1:1.11.0-1ubuntu1 wget=1.17.1-1ubuntu1.5 gcc=4:5.3.1-1ubuntu1 g++=4:5.3.1-1ubuntu1 python3-dev=3.5.1-3 unzip=6.0-20ubuntu1.1 make=4.1-6 python3-matplotlib=1.5.1-1ubuntu1 python3-reportlab=3.3.0-1ubuntu0.1 python3-pip=8.1.1-2ubuntu0.6 r-base=3.2.3-4 clustalw=2.1+lgpl-4 fasttree=2.1.8-2 t-coffee=11.00.8cbe486-2 python3-pil=3.1.2-0ubuntu1.6 bwa=0.7.12-5 ncbi-blast+=2.2.31-4 emboss=6.6.0+dfsg-3build1 clustalo=1.2.1-2 phylip=1:3.696+dfsg-2 mafft=7.271-1 muscle=1:3.8.31+dfsg-1 samtools=0.1.19-1ubuntu1 phyml=3:3.2.0+dfsg-1 wise=2.4.1-17 raxml=8.2.4-1 language-pack-en=1:16.04+20161009 paml=4.8+dfsg-1 probcons=1.12-10 python3-pandas=0.17.1-3ubuntu2 python3.5-dev=3.5.2-2ubuntu0~16.04.13 libxft-dev=2.3.2-1 -y --force-yes \
 && apt-get clean
#  for Phylo_CDAO
#   RUN pip3 install pip --upgrade
RUN pip3 install rdflib --upgrade \
 && pip3 install cython --upgrade \
 && pip3 install numpy --upgrade \
 && pip3 install Pillow --upgrade \
 && pip3 install matplotlib --upgrade \
 && pip3 install pandas --upgrade
#  Manual software
RUN echo "export DIALIGN2_DIR=/tmp" >> .bashrc
#  reportlab fonts
RUN wget http://www.reportlab.com/ftp/fonts/pfbfer.zip
WORKDIR cd /usr/lib/python3.4/dist-packages/reportlab
RUN mkdir fonts
WORKDIR cd /usr/lib/python3.4/dist-packages/reportlab/fonts
RUN unzip /pfbfer.zip \
 && mkdir -p /usr/lib/python3.5/dist-packages/reportlab/fonts
WORKDIR /usr/lib/python3.5/dist-packages/reportlab/fonts
RUN unzip /pfbfer.zip
WORKDIR /
RUN rm pfbfer.zip
#  genepop
RUN mkdir genepop
WORKDIR /genepop
RUN wget http://kimura.univ-montp2.fr/~rousset/sources.tar.gz \
 && tar zxf sources.tar.gz \
 && g++ -DNO_MODULES -o Genepop GenepopS.cpp -O3 \
 && cp Genepop /usr/bin
WORKDIR /
RUN rm -rf genepop
#  fdist
RUN mkdir fdist2
WORKDIR /fdist2
RUN wget http://www.maths.bris.ac.uk/~mamab/software/fdist2.zip \
 && unzip fdist2.zip \
 && gcc -o fdist2 -O fdist2.c -lm \
 && gcc -o cplot -O cplot.c as100.c as99.c -lm \
 && gcc -o pv -O pv.c as100.c as99.c -lm \
 && gcc -o datacal -O datacal.c -lm \
 && cp datacal pv cplot fdist2 /usr/bin
WORKDIR /
RUN rm -rf fdist2
#  dfdist
RUN wget http://www.maths.bris.ac.uk/~mamab/stuff/Dfdist_a.zip \
 && unzip Dfdist_a
WORKDIR Dfdist_a
RUN gcc -O -o Ddatacal Ddatacal.c -lm \
 && gcc -O -o Dfdist Dfdist.c -lm \
 && gcc -O -o pv2 pv2.c -lm \
 && gcc -O -o cplot2 cplot2.c -lm \
 && cp pv2 Dfdist Ddatacal cplot2 /usr/bin
WORKDIR /
RUN rm -rf Dfdist_a*
#  msaprobs
RUN wget "http://sourceforge.net/projects/msaprobs/files/latest/download?source=files" -O MSA.tar.gz \
 && tar zxf MSA.tar.gz
WORKDIR /MSAProbs-0.9.7/MSAProbs
RUN make \
 && cp msaprobs /usr/bin
WORKDIR /
#  fastsimcoal
RUN wget http://cmpg.unibe.ch/software/fastsimcoal2/downloads/fsc_linux64.zip \
 && unzip fsc_linux64.zip \
 && chmod a+x fsc_linux64/fsc25221 \
 && cp fsc_linux64/fsc25221 /usr/bin/fsc252 \
 && rm -rf fsc_*
#  DSSP
RUN wget ftp://ftp.cmbi.ru.nl/pub/software/dssp/dssp-2.0.4-linux-amd64 \
 && mv dssp-2.0.4-linux-amd64 /usr/bin/dssp \
 && chmod a+x /usr/bin/dssp
#  XXmotif
WORKDIR /usr/local/bin
RUN wget "http://xxmotif.genzentrum.lmu.de/index.php?id=download&version=64" -O xx.tar.gz \
 && tar zxf xx.tar.gz \
 && rm xx.tar.gz
WORKDIR /
ENV PYTHON_PATH="/biopython"
#  Biopython
RUN git clone https://github.com/biopython/biopython.git
WORKDIR /biopython
RUN python3.5 setup.py install
#  set default python version to 3.5
RUN touch ~/.bash_aliases \
 && echo alias python='python3.5' > ~/.bash_aliases
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
