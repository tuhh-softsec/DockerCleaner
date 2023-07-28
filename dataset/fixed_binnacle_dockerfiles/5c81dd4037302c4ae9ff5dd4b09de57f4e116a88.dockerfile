#  ###
#   Netflow collector and local processing container
#   using NFSen and NFDump for processing. This can
#   be run standalone or in conjunction with a analytics
#   engine that will perform time based graphing and
#   stats summarization.
#  ##
FROM debian:latest
MAINTAINER Brent Salisbury <brent.salisbury@gmail.com
RUN :
RUN (apt-get update ;apt-get install --no-install-recommends gcc=4:12.2.0-3ubuntu1 flex=2.6.4-8.1 rrdtool=1.7.2-4ubuntu5 apache2=2.4.55-1ubuntu2 tcpdump=4.99.3-1ubuntu1 wget=1.21.3-1ubuntu1 php5 apache2=2.4.55-1ubuntu2 librrd-dev=1.7.2-4ubuntu5 libapache2-mod-php5 php5-common libio-socket-inet6-perl=2.73-1 libio-socket-ssl-perl=2.081-2 libmailtools-perl=2.21-2 librrds-perl=1.7.2-4ubuntu5 libwww-perl=6.67-1 libipc-run-perl=20220807.0-1 perl-base=5.36.0-7 libsys-syslog-perl supervisor=4.2.1-1ubuntu1 net-tools=2.10-0.1ubuntu3 -y )
#   Cleanup apt-get cache
RUN apt-get clean
#   Apache
EXPOSE 80/tcp
#   NetFlow
EXPOSE 2055/tcp
#   IPFIX
EXPOSE 4739/tcp
#   sFlow
EXPOSE 6343/tcp
#   nfsen src ip src node mappings per example
EXPOSE 9996/tcp
#   mk some dirs
RUN mkdir -p /var/lock/apache2 /var/run/apache2 /var/log/supervisor
#   Example ENV variable injection if you want to add collector addresses
ENV NFSEN_VERSION="1.3.7"
ENV NFDUMP_VERSION="1.6.13"
#   Install NFDump (note the random redirected DL server from sourceforge. Their redirects are awful
#   so using the only 302 redirect that is the closest to almost working every time...
RUN cd /usr/local/src \
 && wget http://iweb.dl.sourceforge.net/project/nfdump/stable/nfdump-${NFDUMP_VERSION}/nfdump-${NFDUMP_VERSION}.tar.gz \
 && tar xfz nfdump-${NFDUMP_VERSION}.tar.gz \
 && cd nfdump-${NFDUMP_VERSION}/ \
 && ./configure --enable-nfprofile --with-rrdpath=/usr/bin --enable-nftrack --enable-sflow \
 && make \
 && make install
#   Configure php with the systems timezone, modifications are tagged with the word 'NFSEN_OPT' for future ref
#   Recommended leaving the timezone as UTC as NFSen and NFCapd timestamps need to be in synch.
#   Timing is also important for the agregates time series viewer for glabal visibility and analytics.
RUN sed -i 's/^;date.timezone =/date.timezone \= \"UTC\"/g' /etc/php5/apache2/php.ini
RUN sed -i '/date.timezone = "UTC\"/i ; NFSEN_OPT Adjust your timezone for nfsen' /etc/php5/apache2/php.ini
RUN sed -i 's/^;date.timezone =/date.timezone \= \"UTC\"/g' /etc/php5/cli/php.ini
RUN sed -i '/date.timezone = "UTC\"/i ; NFSEN_OPT Adjust your timezone for nfsen' /etc/php5/cli/php.ini
#   Configure NFSen config files
RUN mkdir -p /data/nfsen
WORKDIR /data
RUN wget http://iweb.dl.sourceforge.net/project/nfsen/stable/nfsen-${NFSEN_VERSION}/nfsen-${NFSEN_VERSION}.tar.gz
RUN tar xfz nfsen-${NFSEN_VERSION}.tar.gz
RUN sed -i 's/"www";/"www-data";/g' nfsen-${NFSEN_VERSION}/etc/nfsen-dist.conf
#   Example how to fill in any flow source you want using | as a delimiter. Sort of long and gross though.
#   Modify the pre-defined NetFlow v5/v9 line matching the regex 'upstream1'
RUN sed -i "s|'upstream1' => { 'port' => '9995', 'col' => '#0000ff', 'type' => 'netflow' },|'netflow-global' => { 'port' => '2055', 'col' => '#0000ff', 'type' => 'netflow' },|g" nfsen-${NFSEN_VERSION}/etc/nfsen-dist.conf
#   Bind port 6343 and an entry for  sFlow collection
RUN sed -i "/%sources/a \ 'sflow-global' => { 'port' => '6343', 'col' => '#0000ff', 'type' => 'sflow' }," nfsen-${NFSEN_VERSION}/etc/nfsen-dist.conf
#   Bind port 4739 and an entry for IPFIX collection. E.g. NetFlow v10
RUN sed -i "/%sources/a \ 'ipfix-global' => { 'port' => '4739', 'col' => '#0000ff', 'type' => 'netflow' }," nfsen-${NFSEN_VERSION}/etc/nfsen-dist.conf
#   Add an account for NFSen as a member of the apache group
RUN useradd -d /var/netflow -G www-data -m -s /bin/false netflow
#   Run the nfsen installer
WORKDIR /data/nfsen-${NFSEN_VERSION}
RUN perl ./install.pl etc/nfsen-dist.conf || true
RUN sleep 3
WORKDIR /
#   Add startup script for nfsen profile init
COPY ./start.sh /data/start.sh
#   flow-generator binary for testing
COPY ./flow-generator /data/flow-generator
COPY ./supervisord.conf /etc/supervisord.conf
CMD bash -C '/data/start.sh' ; '/usr/bin/supervisord'
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
