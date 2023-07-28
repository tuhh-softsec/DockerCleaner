#   Use Ubuntu 14.04 as the base container
FROM ubuntu:16.04
#   Add Standard Packages + Verification Key
RUN apt-key adv --keyserver keyserver.ubuntu.com --recv-keys 51716619E084DAB9
RUN apt-get update -y -qq \
 && apt-get install --no-install-recommends netcat=1.10-41 openssl=1.0.2g-1ubuntu4.20 apache2=2.4.18-2ubuntu3.17 apache2-utils=2.4.18-2ubuntu3.17 libpcre3=2:8.38-3.1 dnsmasq=2.75-1ubuntu0.16.04.10 procps=2:3.3.10-4ubuntu2.5 apt-transport-https=1.2.35 make=4.1-6 gcc=4:5.3.1-1ubuntu1 g++=4:5.3.1-1ubuntu1 libxml2=2.9.3+dfsg1-1ubuntu0.7 libxml2-dev=2.9.3+dfsg1-1ubuntu0.7 python-pip=8.1.1-2ubuntu0.6 curl=7.47.0-1ubuntu2.19 nano=2.5.3-2ubuntu2 supervisor=3.2.0-2ubuntu0.2 gunicorn=19.4.5-1ubuntu1.1 python3-pip=8.1.1-2ubuntu0.6 zlib1g-dev=1:1.2.8.dfsg-2ubuntu4.3 libpng-dev git=1:2.7.4-0ubuntu1.10 python3=3.5.1-3 python3-dev=3.5.1-3 python3-pip=8.1.1-2ubuntu0.6 python3-numpy=1:1.11.0-1ubuntu1 python3-scipy=0.17.0-1 python-imaging=3.1.2-0ubuntu1.6 -y -qq
#   Install Kong
RUN curl -sL https://github.com/Mashape/kong/releases/download/0.9.4/kong-0.9.4.trusty_all.deb > kong-0.9.4.trusty_all.deb \
 && dpkg -i kong-0.9.4.trusty_all.deb
#   Install Node 6.x + PM2
RUN curl -sL https://deb.nodesource.com/setup_6.x | bash -
RUN apt-get install --no-install-recommends nodejs=4.2.6~dfsg-1ubuntu4.2 -y -qq
RUN npm install pm2@5.3.0 -g
#   Install OpenCPU
#  RUN \
#    apt-get update && \
#    apt-get -y dist-upgrade && \
#    apt-get install -y software-properties-common && \
#    add-apt-repository -y ppa:opencpu/opencpu-1.6 && \
#    apt-get update && \
#    apt-get install -y opencpu 
#  RUN truncate -s 0 /etc/apache2/ports.conf
#  RUN echo "ServerName localhost" >> /etc/apache2/apache2.conf
#   Create Application User
RUN useradd -u 7534 -m -d /home/sttrweb -c "sttr web application" sttrweb \
 && mkdir /home/sttrweb/Oncoscape \
 && mkdir /home/sttrweb/Oncoscape/uploads \
 && chmod +x /home/sttrweb/Oncoscape/uploads \
 && mkdir /home/sttrweb/Oncoscape/cache \
 && mkdir /var/log/nginx/
#   Python Server
WORKDIR /home/sttrweb/Oncoscape/
RUN git clone https://github.com/Oncoscape/oncoscape_algorithm_wrapper.git
WORKDIR oncoscape_algorithm_wrapper
RUN pip3 install -r requirements.txt
#   Upload Tool
WORKDIR /home/sttrweb/Oncoscape/
RUN git clone https://github.com/Oncoscape/NG4-Data-Upload.git
#   Install Client Code
WORKDIR /home/sttrweb/Oncoscape/
COPY client-build /home/sttrweb/Oncoscape/client
COPY documentation/dist /home/sttrweb/Oncoscape/documentation
#   Install Server Code
COPY server /home/sttrweb/Oncoscape/server
WORKDIR /home/sttrweb/Oncoscape/server/
RUN npm install
#   Install R Package
#  COPY cpu/oncoscape_0.1.0.tgz /home/sttrweb/Oncoscape/oncoscape_0.1.0.tgz
#  WORKDIR /home/sttrweb/Oncoscape/
#  RUN R CMD INSTALL oncoscape_0.1.0.tgz --library=/usr/local/lib/R/site-library
#  RUN echo "r <- getOption('repos'); r['CRAN'] <- 'http://cran.us.r-project.org'; options(repos = r);" > ~/.Rprofile
#  RUN Rscript -e "install.packages(c('devtools','ggplot2','gridSVG','d3heatmap','pls'))"
#   Copy Config Files
WORKDIR /home/sttrweb/Oncoscape/
COPY /docker-kong.template /home/sttrweb/Oncoscape/
COPY /docker-nginx.template /home/sttrweb/Oncoscape/
COPY /docker-supervisord.conf /home/sttrweb/Oncoscape/
COPY /docker-entrypoint.sh /home/sttrweb/Oncoscape/
#   Expose Ports
EXPOSE 80/tcp 7946/tcp 8000/tcp 8001/tcp 8003/tcp 8004/tcp 10001/tcp
EXPOSE 7946/udp
#   Fire It Up
RUN chmod +x /home/sttrweb/Oncoscape/docker-entrypoint.sh
ENTRYPOINT ["/home/sttrweb/Oncoscape/docker-entrypoint.sh"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
