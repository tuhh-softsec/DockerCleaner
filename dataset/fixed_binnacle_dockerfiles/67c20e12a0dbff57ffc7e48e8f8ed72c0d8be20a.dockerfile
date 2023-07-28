#   Automatically generated Dockerfile 
#   - Build ${timestamp}
#   - Lucene/Solr version ${currentLuceneVersion}
#   - Mtas release ${project.version} 
#
FROM ubuntu:16.04
MAINTAINER Matthijs Brouwer, Textexploration.org
EXPOSE 8983/tcp 80/tcp
USER root
WORKDIR "/root" 
RUN mkdir lib
RUN which wget &> /dev/null || apt-get install --no-install-recommends wget=1.20.3 ; wget --no-verbose --output-document /root/lib/ https://github.com/textexploration/mtas/releases/download/v${project.version}/mtas-${project.version}.jar
RUN apt-get update \
 && apt-get install --no-install-recommends locales=2.23-0ubuntu11.3 lsof=4.89+dfsg-0.1 software-properties-common=0.96.20.10 python-software-properties=0.96.20.10 apache2=2.4.18-2ubuntu3.17 curl=7.47.0-1ubuntu2.19 subversion=1.9.3-2ubuntu1.3 -y \
 && locale-gen en_US.UTF-8 en_US \
 && update-locale LANG=en_US.UTF-8 LANGUAGE=en_US:en
RUN mathurl=$( curl -s 'http://www.apache.org/dyn/closer.lua/commons/math/binaries/commons-math3-3.6.1-bin.tar.gz' | grep -o '<strong>[^<]*</strong>' | sed 's/<[^>]*>//g' | head -1 ;) \
 && if echo "$mathurl" | grep -q '^.*[^ ].*$' ; then curl -f -o /root/lib/commons-math3-3.6.1-bin.tar.gz -O $mathurl || true ; fi \
 && if [ ! -f /root/lib/commons-math3-3.6.1-bin.tar.gz ] ; then echo "Commons-math3 not found in mirror, falling back to apache archive" ;mathurl="http://archive.apache.org/dist/commons/math/binaries/commons-math3-3.6.1-bin.tar.gz" ;curl -f -o /root/lib/commons-math3-3.6.1-bin.tar.gz -O $mathurl ; fi \
 && tar xzf lib/commons-math3-3.6.1-bin.tar.gz -C lib commons-math3-3.6.1/commons-math3-3.6.1.jar --strip-components=1 \
 && rm lib/commons-math3-3.6.1-bin.tar.gz
RUN svn export https://github.com/textexploration/mtas/trunk/docker/ data
RUN add-apt-repository -y ppa:webupd8team/java \
 && apt-get update \
 && echo oracle-java8-installer shared/accepted-oracle-license-v1-1 select true | /usr/bin/debconf-set-selections \
 && apt-get install --no-install-recommends oracle-java8-installer -y \
 && rm -rf /var/lib/apt/lists/*
RUN solrurl=$( curl -s 'http://www.apache.org/dyn/closer.lua/lucene/solr/${currentLuceneVersion}/solr-${currentLuceneVersion}.tgz' | grep -o '<strong>[^<]*</strong>' | sed 's/<[^>]*>//g' | head -1 ;) \
 && if echo "$solrurl" | grep -q '^.*[^ ].*$' ; then curl -f -o /root/solr-${currentLuceneVersion}.tgz -O $solrurl || true ; fi \
 && if [ ! -f /root/solr-${currentLuceneVersion}.tgz ] ; then echo "Solr ${currentLuceneVersion} not found in mirror, falling back to apache archive" ;solrurl="http://archive.apache.org/dist/lucene/solr/${currentLuceneVersion}/solr-${currentLuceneVersion}.tgz" ;curl -f -o /root/solr-${currentLuceneVersion}.tgz -O $solrurl ; fi \
 && tar xzf solr-${currentLuceneVersion}.tgz solr-${currentLuceneVersion}/bin/install_solr_service.sh --strip-components=2 \
 && bash ./install_solr_service.sh solr-${currentLuceneVersion}.tgz \
 && rm install_solr_service.sh \
 && rm -rf solr-${currentLuceneVersion}.tgz
RUN service apache2 stop \
 && echo "ServerName localhost" | tee /etc/apache2/conf-available/fqdn.conf \
 && a2enmod proxy \
 && a2enmod proxy_http \
 && a2enmod proxy_ajp \
 && a2enmod rewrite \
 && a2enmod deflate \
 && a2enmod headers \
 && a2enmod proxy_balancer \
 && a2enmod proxy_connect \
 && a2enmod proxy_html \
 && a2enmod xml2enc \
 && a2enconf fqdn \
 && sed -i '/<\/VirtualHost>/ i ProxyPass /solr http://localhost:8983/solr\nProxyPassReverse /solr http://localhost:8983/solr' /etc/apache2/sites-enabled/000-default.conf \
 && rm -rf /var/www/html/* \
 && mkdir /var/www/html/demo \
 && cp -rp data/*-samples /var/www/html/demo/ \
 && gunzip -r /var/www/html/demo \
 && cp -rp data/site/* /var/www/html \
 && chmod -R 755 /var/www/html \
 && printf "echo\n" >> /start.sh \
 && printf "echo \"================ Mtas -- Multi Tier Annotation Search =================\"\n" >> /start.sh \
 && printf "echo \" Timestamp ${timestamp}\"\n" >> /start.sh \
 && printf "echo \" Lucene/Solr version ${currentLuceneVersion}\"\n" >> /start.sh \
 && printf "echo \" Mtas release ${project.version}\"\n" >> /start.sh \
 && printf "echo \" See https://textexploration.github.io/mtas/ for more information\"\n" >> /start.sh \
 && printf "echo \"=======================================================================\"\n" >> /start.sh \
 && printf "echo\n" >> /start.sh \
 && printf "service solr start\nservice apache2 start\n" >> /start.sh \
 && chmod 755 /start.sh \
 && mkdir demo1 \
 && mkdir demo1/lib \
 && mkdir demo1/conf \
 && echo "name=demo1" > demo1/core.properties \
 && cp lib/commons-math3-3.6.1.jar demo1/lib/ \
 && cp lib/mtas-${project.version}.jar demo1/lib/ \
 && cp data/solrconfig.xml demo1/conf/ \
 && cp data/schemaBasic.xml demo1/conf/schema.xml \
 && cp -r data/mtas demo1/conf/ \
 && cp data/mtas.xml demo1/conf/ \
 && chmod -R 777 demo1 \
 && cp -rp demo1 demo2 \
 && cp data/schemaFull.xml demo2/conf/schema.xml \
 && echo "name=demo2" > demo2/core.properties \
 && cp -rp demo1 demo3 \
 && cp data/schemaFull.xml demo3/conf/schema.xml \
 && echo "name=demo3" > demo3/core.properties \
 && cp -rp demo1 demo4 \
 && cp data/schemaFull.xml demo4/conf/schema.xml \
 && echo "name=demo4" > demo4/core.properties \
 && mv demo1 /var/solr/data/ \
 && mv demo2 /var/solr/data/ \
 && mv demo3 /var/solr/data/ \
 && mv demo4 /var/solr/data/
CMD bash -C '/start.sh' ; 'bash'
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!