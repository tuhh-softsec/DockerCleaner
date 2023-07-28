#   Use Alpine Linux
FROM alpine:3.8
#   Maintainer
#  MAINTAINER Artur Petrov <artur@phpchain.ru>
#   EN: Variables
#   RU: Переменные
#   EN: IP-address
#   RU: IP-адрес
ENV IP="0.0.0.0"
#   The IP address to listen on. The default is "0.0.0.0" (all interfaces).
#   EN: TCP-port
#   RU: TCP-порт
ENV PORT="1688"
#   The network port to listen on. The default is "1688".
#   EN: ePID
#   RU: ePID
ENV EPID="\"
#   Use this flag to manually specify an ePID to use. If no ePID is specified, a random ePID will be generated.
#   EN: lcid
#   RU: lcid
ENV LCID="1033"
#   Use this flag to manually specify an LCID for use with randomly generated ePIDs. Default is 1033 (en-us).
#   EN: the current client count
#   RU: текущий счётчик запросов на активацию продуктов от Microsoft
ENV CLIENT_COUNT="26"
#   Use this flag to specify the current client count. Default is 26.
#   A number >=25 is required to enable activation of client OSes; for server OSes and Office >=5.
#   EN: the activation interval (in minutes)
#   RU: интервал активации (в минутах)
ENV ACTIVATION_INTERVAL="120"
#   Use this flag to specify the activation interval (in minutes). Default is 120 minutes (2 hours).
#   EN: the renewal interval (in minutes)
#   RU: интервал обновления (в минутах)
ENV RENEWAL_INTERVAL="10080"
#   Use this flag to specify the renewal interval (in minutes). Default is 10080 minutes (7 days).
#   EN: Use SQLITE
#   RU: Использовать РСУБД SQLITE
ENV SQLITE="false"
#   Use this flag to store request information from unique clients in an SQLite database.
#   EN: hwid
#   RU: hwid
ENV HWID="364F463A8863D35F"
#   Use this flag to specify a HWID. 
#   The HWID must be an 16-character string of hex characters.
#   The default is "364F463A8863D35F" or type "random" to auto generate the HWID.
#   EN: log level ("CRITICAL", "ERROR", "WARNING", "INFO", "DEBUG")
#   RU: Уровень логирования ("CRITICAL", "ERROR", "WARNING", "INFO", "DEBUG")
ENV LOGLEVEL="ERROR"
#   Use this flag to set a Loglevel. The default is "ERROR".
#   EN: log file
#   RU: Лог-файл
ENV LOGFILE="/var/log/pykms_logserver.log"
#   Use this flag to set an output Logfile. The default is "/var/log/pykms_logserver.log".
#   EN: Startup script
#   RU: Скрипт автозапуска
COPY start.sh /usr/bin/start.sh
#   EN: Software installation
#   RU: Установка программного обеспечения
RUN echo "http://dl-cdn.alpinelinux.org/alpine/latest-stable/main" >> /etc/apk/repositories \
 && apk update \
 && apk upgrade \
 && apk add bash=4.4.19-r1 git=2.18.4-r0 python3=3.6.9-r1 py3-argparse=1.4.0-r1 py3-tz=2018.4-r0 py3-flask=0.12.2-r1 py3-pygments=2.2.0-r0 python3-tkinter=3.6.3-r1 sqlite-libs=3.25.3-r4 py3-pip --update \
 && git clone https://github.com/SystemRage/py-kms.git /home/py-kms \
 && git clone https://github.com/coleifer/sqlite-web.git /tmp/sqlite_web \
 && mv /tmp/sqlite_web/sqlite_web /home/ \
 && rm -rf /tmp/sqlite_web \
 && pip3 install peewee tzlocal \
 && chmod a+x /usr/bin/start.sh \
 && apk del git py3-pip
#   Set Workdir
WORKDIR /home/py-kms/py-kms
#   Expose ports
EXPOSE ${PORT}/tcp
#   Entry point
ENTRYPOINT ["/usr/bin/start.sh"]
RUN addgroup -S docker-user ; adduser -S -G docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
