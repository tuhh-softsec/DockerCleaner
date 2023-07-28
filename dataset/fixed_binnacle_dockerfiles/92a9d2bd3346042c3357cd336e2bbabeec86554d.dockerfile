#
#   SemEHR Dockerfile
#
FROM ubuntu:latest
MAINTAINER Honghan Wu "honghan.wu@gmail.com"
#  #######
#   Pre-reqs
#  #######
RUN apt-get update \
 && (apt-get update ;apt-get install --no-install-recommends software-properties-common=0.99.35 -y )
#   && add-apt-repository ppa:webupd8team/java
RUN :
#   RUN echo "oracle-java8-installer shared/accepted-oracle-license-v1-1 select true" | debconf-set-selections
RUN (apt-get update ;apt-get install --no-install-recommends ant=1.10.13-1 curl=7.88.1-7ubuntu1 openjdk-11-jdk=11.0.18+10-0ubuntu4 subversion=1.14.2-4build2 unzip=6.0-27ubuntu1 vim=2:9.0.1000-4ubuntu2 git=1:2.39.2-1ubuntu1 -y )
#   ENV JAVA_HOME /usr/lib/jvm/java-8-oracle/
ENV JAVA_HOME="/usr/lib/jvm/open-jdk/"
#  #######
#   GCP, Gate, Bio-Yodie
#  #######
RUN mkdir /opt/gcp
WORKDIR '/opt/gcp'
ENV JAVA_TOOL_OPTIONS="'-Dfile.encoding=UTF8'"
RUN cd /opt/gcp
#  at this moment, bio-yodie requires this particular subversion of GCP
RUN svn co http://svn.code.sf.net/p/gate/code/gcp/trunk@18658 gcp-2.5-18658
ENV JAVA_HOME="/usr/lib/jvm/java-11-openjdk-amd64/"
RUN cd /opt/gcp/gcp-2.5-18658 \
 && ant
ENV GCP_HOME="'/opt/gcp/gcp-2.5-18658'"
RUN curl -L 'http://netix.dl.sourceforge.net/project/gate/gate/8.1/gate-8.1-build5169-ALL.zip' > gate-8.1-build5169-ALL.zip \
 && unzip gate-8.1-build5169-ALL.zip \
 && mv gate-8.1-build5169-ALL gate \
 && rm gate-8.1-build5169-ALL.zip
ENV GATE_HOME="'/opt/gcp/gate'"
WORKDIR '/opt/gcp/'
ENV PATH="\"$PATH:$GCP_HOME:$GATE_HOME/bin\""
RUN curl -L 'https://cogstack.rosalind.kcl.ac.uk/exports/bio-yodie-1.2.1-se.tar.gz' > bio-yodie-1.2.1-se.tar.gz \
 && tar xzvf bio-yodie-1.2.1-se.tar.gz \
 && rm bio-yodie-1.2.1-se.tar.gz
RUN mv bio-yodie-1.2.1 bio-yodie-1-2-1
RUN cd /opt/gcp/gcp-2.5-18658/lib
WORKDIR '/opt/gcp/gcp-2.5-18658/lib'
RUN curl -L 'https://cogstack.rosalind.kcl.ac.uk/exports/customised_handlers.tar.gz' > customised_handlers.tar.gz \
 && tar xzvf customised_handlers.tar.gz \
 && cp customised_handlers/* ./ \
 && rm -fr customised_handlers \
 && rm -f customised_handlers.tar.gz
#  #######
#   python & libraries for SemEHR
#  #######
RUN :
RUN (apt-get update ;apt-get install --no-install-recommends python python-pip wget=1.21.3-1ubuntu1 python-setuptools python-dev libxml2-dev=2.9.14+dfsg-1.1build2 libxslt-dev zlib1g-dev=1:1.2.13.dfsg-1ubuntu4 build-essential=12.9ubuntu3 -y )
RUN pip install requests==2.28.2
RUN pip install lxml==4.9.2
RUN pip install pyquery==2.0.0
RUN pip install joblib==1.2.0
#   RUN pip install hashlib
#   RUN easy_install hashlib
RUN pip install urllib3==1.26.15
RUN pip install Elasticsearch==8.7.0
RUN (apt-get update ;apt-get install --no-install-recommends python-mysqldb -y )
#  RUN pip install MySQL-python
RUN (apt-get update ;apt-get install --no-install-recommends unixodbc=2.3.11-2 unixodbc-dev=2.3.11-2 libmysqlclient-dev=8.0.32-0ubuntu4 freetds-dev=1.3.17+ds-2 tdsodbc=1.3.17+ds-2 -y )
RUN pip install pyodbc==4.0.39
#   mysql odbc
RUN curl -L 'https://cdn.mysql.com//Downloads/Connector-ODBC/8.0/mysql-connector-odbc-8.0.16-linux-ubuntu19.04-x86-64bit.tar.gz' > mysql-connector-odbc-8.0.16-linux-ubuntu19.04-x86-64bit.tar.gz \
 && tar xzvf mysql-connector-odbc-8.0.16-linux-ubuntu19.04-x86-64bit.tar.gz \
 && cp mysql-connector-odbc-8.0.16-linux-ubuntu19.04-x86-64bit/lib/* /usr/lib \
 && mysql-connector-odbc-8.0.16-linux-ubuntu19.04-x86-64bit/bin/myodbc-installer -d -n MySQL -a -t DRIVER=/usr/lib/libmyodbc8w.so \
 && rm -fr mysql-connector-odbc-8.0.16-linux-ubuntu19.04-x86-64bit \
 && rm -f mysql-connector-odbc-8.0.16-linux-ubuntu19.04-x86-64bit.tar.gz
#  #######
#   SemEHR
#  #######
RUN mkdir /opt/semehr
WORKDIR '/opt/semehr'
RUN cd /opt/semehr
#   RUN git clone https://github.com/CogStack/CogStack-SemEHR.git
RUN mkdir /opt/semehr/CogStack-SemEHR
ENV semehr_path="'/opt/semehr/CogStack-SemEHR'"
ENV PATH="\"$PATH:$semehr_path:/opt/semehr/\""
ENV CLASSPATH="\"$GATE_HOME/bin\""
#   RUN cp ./docker/semehr.sh ./
RUN curl -L 'https://cogstack.rosalind.kcl.ac.uk/exports/semehr.sh.txt' > semehr.sh
RUN chmod a+x semehr.sh
RUN mkdir /data/
RUN mkdir /data/output_docs
RUN mkdir /data/input_docs
RUN mkdir /data/smehr_results
#  #######
#   entrypoint
#  #######
ENTRYPOINT ["semehr.sh"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
