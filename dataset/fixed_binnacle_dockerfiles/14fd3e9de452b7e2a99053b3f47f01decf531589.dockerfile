FROM ubuntu:16.04
MAINTAINER leoatchina,leoatchina@gmail.com
COPY sources.list /etc/apt/sources.list
RUN apt-get update -y \
 && apt-get upgrade -y \
 && apt-get install --no-install-recommends wget=1.17.1-1ubuntu1.5 curl=7.47.0-1ubuntu2.19 net-tools=1.60-26ubuntu1 iputils-ping=3:20121221-5ubuntu2 apt-transport-https=1.2.35 openssh-server=1:7.2p2-4ubuntu2.10 unzip=6.0-20ubuntu1.1 bzip2=1.0.6-8ubuntu0.2 apt-utils=1.2.35 gdebi-core=0.9.5.7ubuntu1 tmux=2.1-3build1 git=1:2.7.4-0ubuntu1.10 htop=2.0.1-1ubuntu1 supervisor=3.2.0-2ubuntu0.2 xclip=0.12+svn84-4 cmake=3.5.1-1ubuntu3 sudo=1.8.16-0ubuntu1.10 libapparmor1=2.10.95-0ubuntu2.11 libcurl4-openssl-dev=7.47.0-1ubuntu2.19 libxml2=2.9.3+dfsg1-1ubuntu0.7 libxml2-dev=2.9.3+dfsg1-1ubuntu0.7 libssl-dev=1.0.2g-1ubuntu4.20 libncurses5-dev=6.0+20160213-1ubuntu1 libncursesw5-dev=6.0+20160213-1ubuntu1 libjansson-dev=2.7-3ubuntu0.1 build-essential=12.1ubuntu2 gfortran=4:5.3.1-1ubuntu1 libcairo2-dev=1.14.6-1 libxt-dev=1:1.1.5-0ubuntu1 automake=1:1.15-4ubuntu1 bash-completion=1:2.1-4.2ubuntu1.1 libapparmor1=2.10.95-0ubuntu2.11 libedit2=3.1-20150325-1ubuntu2 libc6=2.23-0ubuntu11.3 psmisc=22.21-2.1ubuntu0.1 rrdtool=1.5.5-4 libzmq3-dev=4.1.4-7ubuntu0.1 libtool=2.4.6-0.1 software-properties-common=0.96.20.10 bioperl=1.6.924-3 libdbi-perl=1.634-1ubuntu0.2 tree=1.7.0-3 python-dev=2.7.12-1~16.04 python3-dev=3.5.1-3 locales=2.23-0ubuntu11.3 -y \
 && locale-gen en_US.UTF-8 \
 && cpan -i Try::Tiny \
 && add-apt-repository ppa:jonathonf/vim -y \
 && apt-get update -y \
 && apt-get install --no-install-recommends vim=2:7.4.1689-3ubuntu1.5 -y \
 && apt-get autoremove \
 && apt-get clean \
 && apt-get purge \
 && rm -rf /tmp/* /var/tmp/* /root/.cpan/*
#   configuration
COPY .bashrc .inputrc .configrc /root/
RUN git clone --depth 1 https://github.com/junegunn/fzf.git ~/.fzf \
 && ~/.fzf/install --all
RUN mkdir -p /opt/rc \
 && cp /root/.bashrc /root/.inputrc /root/.configrc /opt/rc
#   bash && ctags
RUN cd /tmp \
 && wget https://ftp.gnu.org/gnu/bash/bash-5.0.tar.gz \
 && tar xvzf bash-5.0.tar.gz \
 && cd bash-5.0 \
 && ./configure \
 && make \
 && make install \
 && cd /tmp \
 && curl -LO https://github.com/BurntSushi/ripgrep/releases/download/11.0.1/ripgrep_11.0.1_amd64.deb \
 && dpkg -i ripgrep_11.0.1_amd64.deb \
 && cd /tmp \
 && git clone --depth 1 https://github.com/universal-ctags/ctags.git \
 && cd ctags \
 && ./autogen.sh \
 && ./configure \
 && make \
 && make install \
 && cd /tmp \
 && curl http://ftp.vim.org/ftp/gnu/global/global-6.6.3.tar.gz -o global.tar.gz \
 && tar xvzf global.tar.gz \
 && cd global-6.6.3 \
 && ./configure --with-sqlite3 \
 && make \
 && make install \
 && apt-get autoremove \
 && apt-get clean \
 && apt-get purge \
 && rm -rf /tmp/* /var/tmp/* /root/.cpan/*
#   node and yarn 
RUN apt-get install --no-install-recommends nodejs=4.2.6~dfsg-1ubuntu4.2 nodejs-legacy=4.2.6~dfsg-1ubuntu4.2 npm=3.5.2-0ubuntu4 -y \
 && npm config set registry https://registry.npm.taobao.org \
 && npm install n@9.1.0 -g \
 && n stable \
 && npm install yarn@1.22.19 -g \
 && apt-get autoremove \
 && apt-get clean \
 && apt-get purge \
 && rm -rf /tmp/* /var/tmp/* /root/.cpan/*
#   R
RUN add-apt-repository 'deb https://cloud.r-project.org/bin/linux/ubuntu xenial-cran35/' \
 && apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E084DAB9 \
 && add-apt-repository ppa:ubuntugis/ppa -y \
 && apt-get update -y \
 && apt-get install --no-install-recommends r-base-dev=3.2.3-4 r-base=3.2.3-4 r-base-core=3.2.3-4 r-recommended=3.2.3-4 -y \
 && apt-get install --no-install-recommends libv8-3.14-dev=3.14.5.8-5ubuntu2 libudunits2-dev=2.2.20-1 libgdal1i=1.11.3+dfsg-3build2 libgdal1-dev=1.11.3+dfsg-3build2 libproj-dev=4.9.2-2 gdal-bin=1.11.3+dfsg-3build2 proj-bin=4.9.2-2 libgdal-dev=1.11.3+dfsg-3build2 libgeos-dev=3.5.0-1ubuntu2 libclang-dev=1:3.8-33ubuntu3.1 -y \
 && apt-get autoremove \
 && apt-get clean \
 && apt-get purge \
 && rm -rf /tmp/* /var/tmp/* /root/.cpan/*
#   rstudio
RUN cd /tmp \
 && curl https://download2.rstudio.org/server/trusty/amd64/rstudio-server-1.2.1335-amd64.deb -o rstudio.deb \
 && gdebi -n rstudio.deb \
 && apt-get autoremove \
 && apt-get clean \
 && apt-get purge \
 && rm -rf /tmp/* /var/tmp/* /root/.cpan/*
#   PATH, if not set here, conda cmd not work 
ENV PATH="/opt/anaconda3/bin:$PATH"
#   anaconda3
RUN cd /tmp \
 && version=$( curl -s https://mirrors.tuna.tsinghua.edu.cn/anaconda/archive/ | grep Linux | grep _64 | tail -1 | awk -F'"' '/^<a href/ {print $2}' ;) \
 && curl https://mirrors.tuna.tsinghua.edu.cn/anaconda/archive/$version -o Anaconda3.sh \
 && bash Anaconda3.sh -b -p /opt/anaconda3 \
 && rm Anaconda3.sh \
 && conda clean -a -y
#  # 使用清华的源
RUN conda config --add channels https://mirrors.tuna.tsinghua.edu.cn/anaconda/pkgs/free/ \
 && conda config --add channels https://mirrors.tuna.tsinghua.edu.cn/anaconda/pkgs/r/ \
 && conda config --add channels https://mirrors.tuna.tsinghua.edu.cn/anaconda/pkgs/mro/ \
 && conda config --add channels https://mirrors.tuna.tsinghua.edu.cn/anaconda/pkgs/main/ \
 && conda config --add channels https://mirrors.tuna.tsinghua.edu.cn/anaconda/cloud/bioconda/ \
 && conda config --add channels https://mirrors.tuna.tsinghua.edu.cn/anaconda/cloud/conda-forge/ \
 && conda config --set show_channel_urls yes
RUN conda install -c bioconda java-jdk \
 && conda clean -a -y \
 && R CMD javareconf \
 && apt-get autoremove \
 && apt-get clean \
 && apt-get purge \
 && rm -rf /tmp/* /var/tmp/* /root/.cpan/*
#  # R kernel for anaconda3
RUN Rscript -e "options(encoding = 'UTF-8'); options('repos' = c(CRAN='https://mirrors.tuna.tsinghua.edu.cn/CRAN/')); install.packages(c('devtools', 'RCurl', 'crayon', 'repr', 'IRdisplay', 'pbdZMQ', 'IRkernel')); IRkernel::installspec(); system('rm -rf /tmp/*') "
#   coder server
RUN cd /tmp \
 && curl -L https://github.com/cdr/code-server/releases/download/1.1156-vsc1.33.1/code-server1.1156-vsc1.33.1-linux-x64.tar.gz -o code-server.tar.gz \
 && tar xvzf code-server.tar.gz \
 && mv code-server1.1156-vsc1.33.1-linux-x64 /opt/code-server \
 && rm -rf /tmp/*.*
#   pip install something
COPY pip.conf /root/.pip/
RUN pip install PyHamcrest==2.0.4 \
 && pip install pip==23.1 --upgrade \
 && pip install neovim==0.3.1 mysql-connector-python==8.0.32 python-language-server==0.36.2 mock==5.0.1 radian==0.6.5 requests==2.28.2 pygments==2.15.0 \
 && pip install flake8==6.0.0 --ignore-installed \
 && rm -rf /root/.cache/pip/* /tmp/* \
 && apt-get autoremove \
 && apt-get clean \
 && apt-get purge \
 && rm -rf /tmp/* /var/tmp/* /root/.cpan/*
#  # system local config
RUN cp /usr/share/zoneinfo/Asia/Shanghai /etc/localtime \
 && echo 'Asia/Shanghai' > /etc/timezone \
 && echo "export LC_ALL=en_US.UTF-8" >> /etc/profile
#  # users
RUN useradd rserver -d /home/rserver \
 && mkdir /jupyter \
 && mkdir /var/run/sshd
WORKDIR /jupyter
#  # config dir
RUN mkdir -p /etc/rstudio /opt/config /opt/log \
 && chmod -R 777 /opt/config /opt/log
#  # set up passwd in entrypoin.sh
ENV PASSWD="jupyter"
COPY rserver.conf /etc/rstudio/
COPY jupyter_lab_config.py supervisord.conf passwd.py entrypoint.sh /opt/config/
ENTRYPOINT ["bash", "/opt/config/entrypoint.sh"]
#  # share
EXPOSE 8888/tcp 8787/tcp 8443/tcp 8822/tcp
VOLUME ["/home/rserver","/jupyter"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
