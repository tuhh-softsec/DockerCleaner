FROM ubuntu:16.04
MAINTAINER CoNWeT Lab. Universidad Politécnica de Madrid
ENV API_VERSION="develop"
RUN apt-get update ; apt-get install --no-install-recommends python2.7=2.7.12-1ubuntu0~16.04.18 python-pip=8.1.1-2ubuntu0.6 git=1:2.7.4-0ubuntu1.10 wget=1.17.1-1ubuntu1.5 unzip=6.0-20ubuntu1.1 maven=3.3.9-3 mysql-client=5.7.33-0ubuntu0.16.04.1 openjdk-8-jdk=8u292-b10-0ubuntu1~16.04.1 -y --fix-missing ; wget http://download.java.net/glassfish/4.1/release/glassfish-4.1.zip ; unzip glassfish-4.1.zip ; pip install sh==2.0.3 ; wget http://dev.mysql.com/get/Downloads/Connector-J/mysql-connector-java-5.1.39.tar.gz ; tar -xvf mysql-connector-java-5.1.39.tar.gz ; cp ./mysql-connector-java-5.1.39/mysql-connector-java-5.1.39-bin.jar glassfish4/glassfish/domains/domain1/lib ; mkdir /apis
WORKDIR /apis
RUN mkdir wars ; git clone https://github.com/FIWARE-TMForum/DSPRODUCTCATALOG2.git
WORKDIR DSPRODUCTCATALOG2
RUN git checkout $API_VERSION ; sed -i 's/jdbc\/sample/jdbc\/pcatv2/g' ./src/main/resources/META-INF/persistence.xml ; sed -i 's/<provider>org\.eclipse\.persistence\.jpa\.PersistenceProvider<\/provider>/ /g' ./src/main/resources/META-INF/persistence.xml ; sed -i 's/<property name="eclipselink\.ddl-generation" value="drop-and-create-tables"\/>/ /g' ./src/main/resources/META-INF/persistence.xml ; sed -i 's/<property name="eclipselink\.logging\.level" value="FINE"\/>/ /g' ./src/main/resources/META-INF/persistence.xml ; if [ -f "./DSPRODUCTORDERING/src/main/java/org/tmf/dsmapi/settings.properties" ] ; then mv ./DSPRODUCTORDERING/src/main/java/org/tmf/dsmapi/settings.properties ./DSPRODUCTORDERING/src/main/resources/settings.properties ; fi ; grep -F "<property name=\"javax.persistence.schema-generation.database.action\" value=\"create\"/>" ./src/main/resources/META-INF/persistence.xml || sed -i 's/<\/properties>/\t<property name=\"javax.persistence.schema-generation.database.action\" value=\"create\"\/>\n\t\t<\/properties>/g' ./src/main/resources/META-INF/persistence.xml ; mvn install ; mv ./target/DSProductCatalog.war ../wars/
WORKDIR ../
#   Next api Docker
RUN git clone https://github.com/FIWARE-TMForum/DSPRODUCTORDERING.git
WORKDIR DSPRODUCTORDERING
RUN git checkout $API_VERSION ; sed -i 's/jdbc\/sample/jdbc\/podbv2/g' ./src/main/resources/META-INF/persistence.xml ; sed -i 's/<provider>org\.eclipse\.persistence\.jpa\.PersistenceProvider<\/provider>/ /g' ./src/main/resources/META-INF/persistence.xml ; sed -i 's/<property name="eclipselink\.ddl-generation" value="drop-and-create-tables"\/>/ /g' ./src/main/resources/META-INF/persistence.xml ; sed -i 's/<property name="eclipselink\.logging\.level" value="FINE"\/>/ /g' ./src/main/resources/META-INF/persistence.xml ; if [ -f "./src/main/java/org/tmf/dsmapi/settings.properties" ] ; then mv ./src/main/java/org/tmf/dsmapi/settings.properties ./src/main/resources/settings.properties ; fi ; grep -F "<property name=\"javax.persistence.schema-generation.database.action\" value=\"create\"/>" ./src/main/resources/META-INF/persistence.xml || sed -i 's/<\/properties>/\t<property name=\"javax.persistence.schema-generation.database.action\" value=\"create\"\/>\n\t\t<\/properties>/g' ./src/main/resources/META-INF/persistence.xml ; mvn install ; mv ./target/DSProductOrdering.war ../wars/
WORKDIR ../
#   Next api Docker
RUN git clone https://github.com/FIWARE-TMForum/DSPRODUCTINVENTORY.git
WORKDIR DSPRODUCTINVENTORY
RUN git checkout $API_VERSION ; sed -i 's/jdbc\/sample/jdbc\/pidbv2/g' ./src/main/resources/META-INF/persistence.xml ; sed -i 's/<provider>org\.eclipse\.persistence\.jpa\.PersistenceProvider<\/provider>/ /g' ./src/main/resources/META-INF/persistence.xml ; sed -i 's/<property name="eclipselink\.ddl-generation" value="drop-and-create-tables"\/>/ /g' ./src/main/resources/META-INF/persistence.xml ; sed -i 's/<property name="eclipselink\.logging\.level" value="FINE"\/>/ /g' ./src/main/resources/META-INF/persistence.xml ; if [ -f "./DSPRODUCTORDERING/src/main/java/org/tmf/dsmapi/settings.properties" ] ; then mv ./DSPRODUCTORDERING/src/main/java/org/tmf/dsmapi/settings.properties ./DSPRODUCTORDERING/src/main/resources/settings.properties ; fi ; grep -F "<property name=\"javax.persistence.schema-generation.database.action\" value=\"create\"/>" ./src/main/resources/META-INF/persistence.xml || sed -i 's/<\/properties>/\t<property name=\"javax.persistence.schema-generation.database.action\" value=\"create\"\/>\n\t\t<\/properties>/g' ./src/main/resources/META-INF/persistence.xml ; mvn install ; mv ./target/DSProductInventory.war ../wars/
WORKDIR ../
#   Next api Docker
RUN git clone https://github.com/FIWARE-TMForum/DSPARTYMANAGEMENT.git
WORKDIR DSPARTYMANAGEMENT
RUN git checkout $API_VERSION ; sed -i 's/jdbc\/sample/jdbc\/partydb/g' ./src/main/resources/META-INF/persistence.xml ; sed -i 's/<provider>org\.eclipse\.persistence\.jpa\.PersistenceProvider<\/provider>/ /g' ./src/main/resources/META-INF/persistence.xml ; sed -i 's/<property name="eclipselink\.ddl-generation" value="drop-and-create-tables"\/>/ /g' ./src/main/resources/META-INF/persistence.xml ; sed -i 's/<property name="eclipselink\.logging\.level" value="FINE"\/>/ /g' ./src/main/resources/META-INF/persistence.xml ; if [ -f "./DSPRODUCTORDERING/src/main/java/org/tmf/dsmapi/settings.properties" ] ; then mv ./DSPRODUCTORDERING/src/main/java/org/tmf/dsmapi/settings.properties ./DSPRODUCTORDERING/src/main/resources/settings.properties ; fi ; grep -F "<property name=\"javax.persistence.schema-generation.database.action\" value=\"create\"/>" ./src/main/resources/META-INF/persistence.xml || sed -i 's/<\/properties>/\t<property name=\"javax.persistence.schema-generation.database.action\" value=\"create\"\/>\n\t\t<\/properties>/g' ./src/main/resources/META-INF/persistence.xml ; mvn install ; mv ./target/DSPartyManagement.war ../wars/
WORKDIR ../
#   Next api Docker
RUN git clone https://github.com/FIWARE-TMForum/DSBILLINGMANAGEMENT.git
WORKDIR DSBILLINGMANAGEMENT
RUN git checkout $API_VERSION ; sed -i 's/jdbc\/sample/jdbc\/bmdbv2/g' ./src/main/resources/META-INF/persistence.xml ; sed -i 's/<provider>org\.eclipse\.persistence\.jpa\.PersistenceProvider<\/provider>/ /g' ./src/main/resources/META-INF/persistence.xml ; sed -i 's/<property name="eclipselink\.ddl-generation" value="drop-and-create-tables"\/>/ /g' ./src/main/resources/META-INF/persistence.xml ; sed -i 's/<property name="eclipselink\.logging\.level" value="FINE"\/>/ /g' ./src/main/resources/META-INF/persistence.xml ; if [ -f "./DSPRODUCTORDERING/src/main/java/org/tmf/dsmapi/settings.properties" ] ; then mv ./DSPRODUCTORDERING/src/main/java/org/tmf/dsmapi/settings.properties ./DSPRODUCTORDERING/src/main/resources/settings.properties ; fi ; grep -F "<property name=\"javax.persistence.schema-generation.database.action\" value=\"create\"/>" ./src/main/resources/META-INF/persistence.xml || sed -i 's/<\/properties>/\t<property name=\"javax.persistence.schema-generation.database.action\" value=\"create\"\/>\n\t\t<\/properties>/g' ./src/main/resources/META-INF/persistence.xml ; mvn install ; mv ./target/DSBillingManagement.war ../wars/
WORKDIR ../
#   Next api Docker
RUN git clone https://github.com/FIWARE-TMForum/DSCUSTOMER.git
WORKDIR DSCUSTOMER
RUN git checkout $API_VERSION ; sed -i 's/jdbc\/sample/jdbc\/customerdbv2/g' ./src/main/resources/META-INF/persistence.xml ; sed -i 's/<provider>org\.eclipse\.persistence\.jpa\.PersistenceProvider<\/provider>/ /g' ./src/main/resources/META-INF/persistence.xml ; sed -i 's/<property name="eclipselink\.ddl-generation" value="drop-and-create-tables"\/>/ /g' ./src/main/resources/META-INF/persistence.xml ; sed -i 's/<property name="eclipselink\.logging\.level" value="FINE"\/>/ /g' ./src/main/resources/META-INF/persistence.xml ; if [ -f "./DSPRODUCTORDERING/src/main/java/org/tmf/dsmapi/settings.properties" ] ; then mv ./DSPRODUCTORDERING/src/main/java/org/tmf/dsmapi/settings.properties ./DSPRODUCTORDERING/src/main/resources/settings.properties ; fi ; grep -F "<property name=\"javax.persistence.schema-generation.database.action\" value=\"create\"/>" ./src/main/resources/META-INF/persistence.xml || sed -i 's/<\/properties>/\t<property name=\"javax.persistence.schema-generation.database.action\" value=\"create\"\/>\n\t\t<\/properties>/g' ./src/main/resources/META-INF/persistence.xml ; mvn install ; mv ./target/DSCustomerManagement.war ../wars/
WORKDIR ../
#   Next api Docker
RUN git clone https://github.com/FIWARE-TMForum/DSUSAGEMANAGEMENT.git
WORKDIR DSUSAGEMANAGEMENT
RUN git checkout $API_VERSION ; sed -i 's/jdbc\/sample/jdbc\/usagedbv2/g' ./src/main/resources/META-INF/persistence.xml ; sed -i 's/<provider>org\.eclipse\.persistence\.jpa\.PersistenceProvider<\/provider>/ /g' ./src/main/resources/META-INF/persistence.xml ; sed -i 's/<property name="eclipselink\.ddl-generation" value="drop-and-create-tables"\/>/ /g' ./src/main/resources/META-INF/persistence.xml ; sed -i 's/<property name="eclipselink\.logging\.level" value="FINE"\/>/ /g' ./src/main/resources/META-INF/persistence.xml ; if [ -f "./DSPRODUCTORDERING/src/main/java/org/tmf/dsmapi/settings.properties" ] ; then mv ./DSPRODUCTORDERING/src/main/java/org/tmf/dsmapi/settings.properties ./DSPRODUCTORDERING/src/main/resources/settings.properties ; fi ; grep -F "<property name=\"javax.persistence.schema-generation.database.action\" value=\"create\"/>" ./src/main/resources/META-INF/persistence.xml || sed -i 's/<\/properties>/\t<property name=\"javax.persistence.schema-generation.database.action\" value=\"create\"\/>\n\t\t<\/properties>/g' ./src/main/resources/META-INF/persistence.xml ; mvn install ; mv ./target/DSUsageManagement.war ../wars/
WORKDIR ../
#   Compile RSS
RUN git clone https://github.com/FIWARE-TMForum/business-ecosystem-rss.git
WORKDIR business-ecosystem-rss
RUN git checkout $VERSION
RUN mvn install -DskipTests \
 && mkdir /etc/default/rss/
#   Set volume for property files
VOLUME /etc/default/rss
WORKDIR ../
#   Create a volume for exposing war files
RUN mkdir wars-ext
VOLUME /apis/wars-ext
WORKDIR /
#   Deploy charging backend
RUN apt-get update \
 && apt-get install --no-install-recommends gcc=4:5.3.1-1ubuntu1 vim=2:7.4.1689-3ubuntu1.5 git=1:2.7.4-0ubuntu1.10 wkhtmltopdf=0.12.2.4-1 xvfb=2:1.18.4-0ubuntu0.12 python2.7=2.7.12-1ubuntu0~16.04.18 python-pip=8.1.1-2ubuntu0.6 python-dev=2.7.12-1~16.04 build-essential=12.1ubuntu2 libssl-dev=1.0.2g-1ubuntu4.20 libffi-dev=3.2.1-4 apache2=2.4.18-2ubuntu3.17 libapache2-mod-wsgi=4.3.0-1.1ubuntu1 -y --fix-missing \
 && git clone https://github.com/FIWARE-TMForum/business-ecosystem-charging-backend.git \
 && pip install sh==2.0.3
WORKDIR business-ecosystem-charging-backend
RUN git checkout $VERSION \
 && mkdir ./src/media \
 && mkdir ./src/media/bills \
 && mkdir ./src/media/assets \
 && mkdir ./src/plugins \
 && mkdir ./src/user_settings
ENV WORKSPACE="`pwd`"
#   Install dependencies and configure system to use volume settings
RUN ./python-dep-install.sh \
 && echo "from user_settings.settings import *" > ./src/settings.py
#   Create volumes
VOLUME /business-ecosystem-charging-backend/src/media/bills
VOLUME /business-ecosystem-charging-backend/src/media/assets
VOLUME /business-ecosystem-charging-backend/src/plugins
VOLUME /business-ecosystem-charging-backend/src/user_settings
VOLUME /business-ecosystem-charging-backend/src/wstore/asset_manager/resource_plugins/plugins
WORKDIR src
RUN rm wsgi.py
COPY wsgi.py .
WORKDIR /etc/apache2/
COPY charging.conf ./sites-available
RUN ln -s ../sites-available/charging.conf ./sites-enabled/charging.conf \
 && sed -i "s|Listen 80|Listen 8006|g" ports.conf
#   Deploy logic proxy
WORKDIR /
RUN git clone https://github.com/FIWARE-TMForum/business-ecosystem-logic-proxy
WORKDIR business-ecosystem-logic-proxy
RUN wget https://nodejs.org/dist/v6.9.1/node-v6.9.1-linux-x64.tar.xz \
 && tar -xvf node-v6.9.1-linux-x64.tar.xz \
 && echo 'export PATH=$PATH:/business-ecosystem-logic-proxy/node-v6.9.1-linux-x64/bin' >> ~/.bashrc \
 && git checkout $VERSION \
 && mkdir indexes \
 && mkdir themes
VOLUME /business-ecosystem-logic-proxy/indexes
VOLUME /business-ecosystem-logic-proxy/themes
VOLUME /business-ecosystem-logic-proxy/static
RUN export PATH=$PATH:/business-ecosystem-logic-proxy/node-v6.9.1-linux-x64/bin \
 && ./install.sh \
 && mkdir etc \
 && cp config.js.template etc/config.js \
 && echo "module.exports = require('./etc/config');" > config.js
COPY ./getConfig.js /business-ecosystem-logic-proxy
WORKDIR /
COPY ./entrypoint.sh /
COPY ./apis-entrypoint.py /
EXPOSE 8000/tcp
ENTRYPOINT ["/entrypoint.sh"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
