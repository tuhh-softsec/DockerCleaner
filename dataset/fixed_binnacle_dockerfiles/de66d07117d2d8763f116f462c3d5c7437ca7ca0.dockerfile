FROM ubuntu:16.04
MAINTAINER AiiDA Team <info@aiida.net>
ARG uid=1000
ARG gid=1000
#  # Set correct locale
#  # For something more complex, as reported by https://hub.docker.com/_/ubuntu/
#  # and taken from postgres:
#   make the "en_US.UTF-8" locale so postgres will be utf-8 enabled by default
RUN apt-get update \
 && apt-get install --no-install-recommends locales=2.23-0ubuntu11.3 -y \
 && rm -rf /var/lib/apt/lists/* \
 && localedef -i en_US -c -f UTF-8 -A /usr/share/locale/locale.alias en_US.UTF-8
ENV LANG="en_US.utf8"
#   Putting the LANG also in the root .bashrc, so that the DB is later
#   Created with the UTF8 locale
RUN sed -i '/interactively/iexport LANG=en_US.utf8' /root/.bashrc
#   This is probably the right command to issue to make sure all users see it as the default locale
RUN update-locale LANG=en_US.utf8
#   I don't define it for now (should use the one of ubuntu by default, anyway
#   jenkins will replace it with 'cat')
#  CMD ["/bin/true"]
#   install required software
RUN apt-get update \
 && apt-get install --no-install-recommends git=1:2.7.4-0ubuntu1.10 vim=2:7.4.1689-3ubuntu1.5 openssh-client=1:7.2p2-4ubuntu2.10 postgresql-client-9.5=9.5.25-0ubuntu0.16.04.1 postgresql-9.5=9.5.25-0ubuntu0.16.04.1 postgresql-server-dev-9.5=9.5.25-0ubuntu0.16.04.1 python2.7=2.7.12-1ubuntu0~16.04.18 -y \
 && apt-get install --no-install-recommends python-pip=8.1.1-2ubuntu0.6 ipython=2.4.1-1 python2.7-dev=2.7.12-1ubuntu0~16.04.18 -y \
 && apt-get install --no-install-recommends texlive-base=2015.20160320-1ubuntu0.1 texlive-generic-recommended=2015.20160320-1ubuntu0.1 texlive-fonts-recommended=2015.20160320-1ubuntu0.1 texlive-latex-base=2015.20160320-1ubuntu0.1 texlive-latex-recommended=2015.20160320-1ubuntu0.1 texlive-latex-extra=2015.20160320-1 dvipng=1.15-0ubuntu1 dvidvi=1.0-8etch2 graphviz=2.38.0-12ubuntu2.1 -y \
 && rm -rf /var/lib/apt/lists/* \
 && apt-get clean all
#   Disable password requests for requests coming from localhost
#   Of course insecure, but ok for testing
RUN cp /etc/postgresql/9.5/main/pg_hba.conf /etc/postgresql/9.5/main/pg_hba.conf~ \
 && perl -npe 's/^([^#]*)md5$/$1trust/' /etc/postgresql/9.5/main/pg_hba.conf~ > /etc/postgresql/9.5/main/pg_hba.conf
#   install sudo otherwise tests for quicksetup fail,
#   see #1382. I think this part should be removed in the
#   future and AiiDA should work also without sudo.
#  # Also install openssh-server needed for AiiDA tests,
#  # and openmpi-bin to have 'mpirun',
#  # and rabbitmq-server needed by AiiDA as the event queue
#  # and libkrb5-dev for gssapi.h
RUN apt-get update \
 && apt-get install --no-install-recommends sudo=1.8.16-0ubuntu1.10 locate=4.6.0+git+20160126-2 openssh-server=1:7.2p2-4ubuntu2.10 openmpi-bin=1.10.2-8ubuntu1 rabbitmq-server=3.5.7-1ubuntu0.16.04.4 libkrb5-dev=1.13.2+dfsg-5ubuntu2.2 -y \
 && rm -rf /var/lib/apt/lists/* \
 && apt-get clean all
#   locate will not find anything if the DB is not updated.
#   Should take ~3-4 secs, so ok
RUN updatedb
#   update pip and setuptools to get a relatively-recent version
#   This can be updated in the future
RUN pip install pip==18.1 setuptools==40.6.2
#   Put the doubler script
COPY doubler.sh /usr/local/bin/
#   Use messed-up filename to test quoting robustness
RUN
#   Put the add script
COPY add.sh /usr/local/bin/
#   add USER (no password); 1000 is the uid of the user in the jenkins docker
RUN groupadd -g ${gid} jenkins \
 && useradd -m -s /bin/bash -u ${uid} -g ${gid} jenkins
#   add to sudoers and don't ask password
RUN adduser jenkins sudo \
 && adduser jenkins adm
RUN echo "%sudo ALL=(ALL:ALL) NOPASSWD:ALL" > /etc/sudoers.d/nopwd
RUN mkdir -p /scratch/jenkins/ \
 && chown jenkins /scratch/jenkins/ \
 && chmod o+rX /scratch/
#  #########################################
#  ########### Installation Setup ##########
#  #########################################
#   install rest of the packages as normal user
USER jenkins
#   set $HOME, create git directory
ENV HOME="/home/jenkins"
# Please add your HEALTHCHECK here!!!
