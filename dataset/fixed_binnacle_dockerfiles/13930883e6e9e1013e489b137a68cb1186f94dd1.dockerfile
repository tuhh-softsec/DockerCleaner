#  Base image
FROM ubuntu:18.04
#  Labels and Credits
LABEL name="MobSF" \
      author="Ajin Abraham <ajin25@gmail.com>" \
      maintainer="Ajin Abraham <ajin25@gmail.com>" \
      contributor_1="OscarAkaElvis <oscar.alfonso.diaz@gmail.com>" \
      contributor_2="Vincent Nadal <vincent.nadal@orange.fr>" \
      description="Mobile Security Framework is an intelligent, all-in-one open source mobile application (Android/iOS/Windows) automated pen-testing framework capable of performing static, dynamic analysis and web API testing"
#  Environment vars
ENV DEBIAN_FRONTEND="noninteractive"
ENV PDFGEN_PKGFILE="wkhtmltox_0.12.5-1.bionic_amd64.deb"
ENV PDFGEN_URL="https://github.com/wkhtmltopdf/wkhtmltopdf/releases/download/0.12.5/${PDFGEN_PKGFILE}"
#  Update the repository sources list
#  Install Required Libs
#  see https://docs.docker.com/develop/develop-images/dockerfile_best-practices/#run
RUN apt-get update -y \
 && apt-get install --no-install-recommends build-essential=12.4ubuntu1 libssl-dev=1.1.1-1ubuntu2.1~18.04.21 libffi-dev=3.2.1-8 libxml2-dev=2.9.4+dfsg1-6.1ubuntu1.8 libxslt1-dev=1.1.29-5ubuntu0.3 locales=2.27-3ubuntu1.6 -y
#  set locales
RUN locale-gen en_US.UTF-8
ENV LANG="en_US.UTF-8" \
    LANGUAGE="en_US:en" \
    LC_ALL="en_US.UTF-8"
#  Install Oracle JDK12
RUN apt-get install --no-install-recommends software-properties-common=0.96.24.32.20 -y \
 && add-apt-repository ppa:linuxuprising/java -y \
 && apt-get update \
 && echo oracle-java12-installer shared/accepted-oracle-license-v1-2 select true | /usr/bin/debconf-set-selections \
 && apt-get install --no-install-recommends oracle-java12-installer -y
#  Install Python 3
RUN apt-get install --no-install-recommends python3.6=3.6.9-1~18.04ubuntu1.12 python3-dev=3.6.7-1~18.04 python3-setuptools=39.0.1-2ubuntu0.1 -y \
 && python3 /usr/lib/python3/dist-packages/easy_install.py pip
#  Install git, sqlite3 client and pdf generator needed dependencies
RUN apt-get install --no-install-recommends sqlite3=3.22.0-1ubuntu0.7 fontconfig-config=2.12.6-0ubuntu2 libjpeg-turbo8=1.5.2-0ubuntu5.18.04.6 fontconfig=2.12.6-0ubuntu2 xorg=1:7.7+19ubuntu7.1 xfonts-75dpi=1:1.0.4+nmu1 git=1:2.17.1-1ubuntu0.17 -y
#  Install wkhtmltopdf for PDF Reports
WORKDIR /tmp
RUN wget ${PDFGEN_URL} \
 && dpkg -i ${PDFGEN_PKGFILE} \
 && rm -rf ${PDFGEN_PKGFILE}
#  Add MobSF master
COPY . /root/Mobile-Security-Framework-MobSF
WORKDIR /root/Mobile-Security-Framework-MobSF
#  Enable Use Home Directory
RUN sed -i 's/USE_HOME = False/USE_HOME = True/g' MobSF/settings.py
#  Kali fix to support 32 bit execution
RUN ./scripts/kali_fix.sh
#  Postgres support is set to false by default
ARG POSTGRES=False
#  check if Postgres support needs to be enabled 
RUN cd scripts \
 && chmod +x postgres_support.sh ; sync ; ./postgres_support.sh $POSTGRES
#  Add apktool working path
RUN mkdir -p /root/.local/share/apktool/framework
#  Install APKiD dependencies
RUN pip3 install wheel
RUN pip3 wheel --wheel-dir=/tmp/yara-python --build-option="build" --build-option="--enable-dex" git+https://github.com/VirusTotal/yara-python.git@v3.10.0
RUN pip3 install --no-index --find-links=/tmp/yara-python yara-python
#  Install Dependencies
RUN pip3 install -r requirements.txt
#  Cleanup
RUN apt-get remove -y git \
 && apt-get clean \
 && apt-get autoclean \
 && apt-get autoremove -y
RUN rm -rf /var/lib/apt/lists/* /tmp/* > /dev/null 2>&1
#  Expose MobSF Port
EXPOSE 8000/tcp
RUN python3 manage.py makemigrations
RUN python3 manage.py migrate
#  Run MobSF
CMD ["gunicorn", "-b", "0.0.0.0:8000", "MobSF.wsgi:application", "--workers=1", "--threads=4", "--timeout=1800"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
