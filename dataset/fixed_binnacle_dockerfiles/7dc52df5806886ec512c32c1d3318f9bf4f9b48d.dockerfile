FROM alpine
#   ---------------- #
#     Installation   #
#   ---------------- #
#   Install all prerequisites
RUN apk add nginx=1.22.1-r0 nodejs=18.14.2-r0 nodejs-npm git=2.38.4-r1 curl=7.88.1-r1 wget=1.21.3-r2 gcc=12.2.1_git20220924-r4 ca-certificates=20220614-r4 python-dev py-pip musl-dev=1.2.3-r4 libffi-dev=3.4.4-r0 cairo=1.17.6-r3 supervisor=4.2.4-r0 bash=5.2.15-r0 py-pyldap py-rrd --update --no-cache \
 && apk add ca-certificates=20220614-r4 wget=1.21.3-r2 --no-cache \
 && wget -q -O /etc/apk/keys/sgerrand.rsa.pub https://alpine-pkgs.sgerrand.com/sgerrand.rsa.pub \
 && wget https://github.com/sgerrand/alpine-pkg-glibc/releases/download/2.28-r0/glibc-2.28-r0.apk \
 && apk add glibc-2.28-r0.apk \
 && rm glibc-2.28-r0.apk \
 && adduser -D -u 1000 -g 'www' www \
 && pip install pip==23.1 pytz==2023.3 gunicorn==20.1.0 six==1.16.0 -U --no-cache-dir \
 && npm install wizzy@0.7.0 -g \
 && npm cache clean --force
#   Checkout the master branches of Graphite, Carbon and Whisper and install from there
RUN mkdir /src \
 && git clone --depth=1 --branch master https://github.com/graphite-project/whisper.git /src/whisper \
 && cd /src/whisper \
 && pip install . --no-cache-dir \
 && python setup.py install
RUN git clone --depth=1 --branch master https://github.com/graphite-project/carbon.git /src/carbon \
 && cd /src/carbon \
 && pip install . --no-cache-dir \
 && python setup.py install
RUN git clone --depth=1 --branch master https://github.com/graphite-project/graphite-web.git /src/graphite-web \
 && cd /src/graphite-web \
 && pip install . --no-cache-dir \
 && python setup.py install \
 && pip install -r requirements.txt --no-cache-dir \
 && python check-dependencies.py
#   Install StatsD
RUN git clone --depth=1 --branch master https://github.com/etsy/statsd.git /src/statsd
#   Install Grafana
RUN mkdir /src/grafana \
 && mkdir /opt/grafana \
 && curl https://s3-us-west-2.amazonaws.com/grafana-releases/release/grafana-5.2.2.linux-amd64.tar.gz -o /src/grafana.tar.gz \
 && tar -xzf /src/grafana.tar.gz -C /opt/grafana --strip-components=1 \
 && rm /src/grafana.tar.gz
#   Cleanup Compile Dependencies
RUN apk del --no-cache git curl wget gcc python-dev musl-dev libffi-dev
#   ----------------- #
#     Configuration   #
#   ----------------- #
#   Confiure StatsD
COPY ./statsd/config.js /src/statsd/config.js
#   Configure Whisper, Carbon and Graphite-Web
COPY ./graphite/initial_data.json /opt/graphite/webapp/graphite/initial_data.json
COPY ./graphite/local_settings.py /opt/graphite/webapp/graphite/local_settings.py
COPY ./graphite/carbon.conf /opt/graphite/conf/carbon.conf
COPY ./graphite/storage-schemas.conf /opt/graphite/conf/storage-schemas.conf
COPY ./graphite/storage-aggregation.conf /opt/graphite/conf/storage-aggregation.conf
RUN mkdir -p /opt/graphite/storage/whisper \
 && mkdir -p /opt/graphite/storage/log/webapp \
 && touch /opt/graphite/storage/log/webapp/info.log \
 && touch /opt/graphite/storage/graphite.db /opt/graphite/storage/index \
 && chown -R www /opt/graphite/storage \
 && chmod 0775 /opt/graphite/storage /opt/graphite/storage/whisper \
 && chmod 0664 /opt/graphite/storage/graphite.db \
 && cp /src/graphite-web/webapp/manage.py /opt/graphite/webapp \
 && cd /opt/graphite/webapp/ \
 && python manage.py migrate --run-syncdb --noinput
#   Configure Grafana and wizzy
COPY ./grafana/custom.ini /opt/grafana/conf/custom.ini
RUN cd /src \
 && wizzy init \
 && extract() { cat /opt/grafana/conf/custom.ini | grep $1 | awk '{print $NF}' ; } \
 && wizzy set grafana url $( extract ";protocol" ;)://$( extract ";domain" ;):$( extract ";http_port" ;) \
 && wizzy set grafana username $( extract ";admin_user" ;) \
 && wizzy set grafana password $( extract ";admin_password" ;)
#   Add the default datasource and dashboards
RUN mkdir /src/datasources \
 && mkdir /src/dashboards
COPY ./grafana/datasources/* /src/datasources
COPY ./grafana/dashboards/* /src/dashboards/
COPY ./grafana/export-datasources-and-dashboards.sh /src/
#   Configure nginx and supervisord
COPY ./nginx/nginx.conf /etc/nginx/nginx.conf
RUN mkdir /var/log/supervisor
COPY ./supervisord.conf /etc/supervisor/conf.d/supervisord.conf
#   ---------------- #
#     Expose Ports   #
#   ---------------- #
#   Grafana
EXPOSE 80/tcp
#   StatsD UDP port
EXPOSE 8125/udp
#   StatsD Management port
EXPOSE 8126/tcp
#   Graphite web port
EXPOSE 81/tcp
#   Graphite Carbon port
EXPOSE 2003/tcp
#   -------- #
#     Run!   #
#   -------- #
CMD ["/usr/bin/supervisord", "--nodaemon", "--configuration", "/etc/supervisor/conf.d/supervisord.conf"]
RUN addgroup -S docker-user ; adduser -S -G docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
