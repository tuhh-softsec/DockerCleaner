#  ##########################################################
#   Gargamelle WEB
#  ##########################################################
#  Build an image starting with debian:stretch image
#   wich contains all the source code of the app
FROM debian:stretch
MAINTAINER ISCPIF <gargantext@iscpif.fr>
USER root
#  ## Update and install base dependencies
RUN echo "############ DEBIAN LIBS ###############"
RUN apt-get update \
 && (apt-get update ;apt-get install --no-install-recommends apt-utils=1.4.11 ca-certificates=20200601~deb9u2 locales=2.24-11+deb9u4 sudo=1.8.19p1-2.1+deb9u3 aptitude=0.8.7-1 gcc=4:6.3.0-4 g++=4:6.3.0-4 wget=1.18-5+deb9u3 git=1:2.11.0-3+deb9u7 vim=2:8.0.0197-4+deb9u7 build-essential=12.3 make=4.1-9.1 curl=7.52.1-5+deb9u16 -y )
#      postgresql-9.6 postgresql-client-9.6 postgresql-contrib-9.6 \
#      postgresql-server-dev-9.6 libpq-dev libxml2                  \
#      postgresql-9.6 postgresql-client-9.6 postgresql-contrib-9.6
#   Install Stack
#  ## Configure timezone and locale
RUN echo "########### LOCALES & TZ #################"
RUN echo "Europe/Paris" > /etc/timezone
ENV TZ="\"Europe/Paris\""
RUN sed -i -e 's/# en_GB.UTF-8 UTF-8/en_GB.UTF-8 UTF-8/' /etc/locale.gen \
 && sed -i -e 's/# fr_FR.UTF-8 UTF-8/fr_FR.UTF-8 UTF-8/' /etc/locale.gen \
 && dpkg-reconfigure --frontend=noninteractive locales \
 && echo 'LANG="fr_FR.UTF-8"' > /etc/default/locale
ENV LANG="fr_FR.UTF-8"
ENV LANGUAGE="fr_FR.UTF-8"
ENV LC_ALL="fr_FR.UTF-8"
#  ## Install main dependencies and python packages based on Debian distrib
RUN echo "############# PYTHON DEPENDENCIES ###############"
RUN apt-get update \
 && (apt-get update ;apt-get install --no-install-recommends libxml2-dev=2.9.4+dfsg1-2.2+deb9u7 xml-core=0.17 libgfortran-6-dev=6.3.0-18+deb9u1 libpq-dev=9.6.24-0+deb9u1 python3.5=3.5.3-1+deb9u5 python3-dev=3.5.3-1 python3-six=1.10.0-3 python3-numpy=1:1.12.1-3 python3-setuptools=33.1.1-1 python3-numexpr=2.6.1-4 python3-pip=9.0.1-2+deb9u2 libxml2-dev=2.9.4+dfsg1-2.2+deb9u7 libxslt-dev libxslt1-dev=1.1.29-2.1+deb9u2 zlib1g-dev=1:1.2.8.dfsg-5+deb9u1 -y )
#   UPDATE AND CLEAN
RUN : \
 && apt-get autoclean \
 && rm -rf /var/lib/apt/lists/*
#  NB: removing /var/lib will avoid to significantly fill up your /var/ folder on your native system
#  #######################################################################
#  ## PYTHON ENVIRONNEMENT (as ROOT)
#  #######################################################################
RUN adduser --disabled-password --gecos "" notebooks
RUN pip3 install virtualenv
RUN virtualenv /env_3-5
RUN echo 'alias venv="source /env_3-5/bin/activate"' >> ~/.bashrc
#   CONFIG FILES
COPY requirements.txt /
#  ADD psql_configure.sh /
COPY django_configure.sh /
RUN . /env_3-5/bin/activate \
 && pip3 install -r requirements.txt \
 && pip3 install git+https://github.com/zzzeek/sqlalchemy.git@rel_1_1 \
 && python3 -m nltk.downloader averaged_perceptron_tagger -d /usr/local/share/nltk_data
#  RUN ./psql_configure.sh
#  RUN ./django_configure.sh
RUN chown notebooks:notebooks -R /env_3-5
#  #######################################################################
#  ## POSTGRESQL DATA (as ROOT)
#  #######################################################################
#  RUN sed -iP "s%^data_directory.*%data_directory = \'\/srv\/gargandata\'%" /etc/postgresql/9.5/main/postgresql.conf
#  RUN echo "host all  all    0.0.0.0/0  md5" >> /etc/postgresql/9.5/main/pg_hba.conf
#  RUN echo "listen_addresses='*'" >> /etc/postgresql/9.5/main/postgresql.conf
EXPOSE 5432/tcp 8899/tcp
VOLUME ["/srv/","/home/notebooks/"]
#  #######################################################################
#  ## Notebook IHaskell and IPYTHON ENVIRONNEMENT
#  #######################################################################
RUN apt-get update \
 && (apt-get update ;apt-get install --no-install-recommends libtinfo-dev=6.0+20161126-1+deb9u2 libzmq3-dev=4.2.1-4+deb9u4 libcairo2-dev=1.14.8-1+deb9u1 libpango1.0-dev=1.40.5-1 libmagic-dev=1:5.30-1+deb9u3 libblas-dev=3.7.0-2 liblapack-dev=3.7.0-2 -y )
#  USER notebooks
#
#  RUN cd  /home/notebooks                              \
#      &&  curl -sSL https://get.haskellstack.org/ | sh  \
#      &&  stack setup                                    \
#      &&  git clone https://github.com/gibiansky/IHaskell \
#      &&  . /env_3-5/bin/activate                          \
#      &&  cd IHaskell                                       \
#      &&  stack install gtk2hs-buildtools                    \
#      &&  stack install --fast                                \
#      &&  /root/.local/bin/ihaskell install --stack
#
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
