FROM alpine:edge
MAINTAINER Ahmad <ahmadt@gmail.com> https://github.com/mrahmadt/
EXPOSE 80/tcp
EXPOSE 443/tcp
ENV VPN_USERNAME="username"
ENV VPN_PASSWORD="password"
ENV VPN_OPTIONS="options"
ENV CIDR_NETWORK="192.168.1.0/24"
ENV VPN_DNS1="103.86.96.100"
ENV VPN_DNS2="103.86.99.100"
RUN apk --update upgrade \
 && apk add runit \
 && rm -rf /var/cache/apk/* \
 && mkdir -p /etc/service \
 && mkdir /initser
WORKDIR /initser
# ################
# ### dnsmasq ####
# ################
RUN apk add --update dnsmasq \
 && rm -rf /var/cache/apk/* \
 && echo -e "localise-queries\nno-resolv\ncache-size=10000\nlocal-ttl=2\nserver=${VPN_DNS1}\nserver=${VPN_DNS2}\ndomain-needed\nbogus-priv\nlocal-service\nbind-interfaces\nuser=root\nconf-dir=/etc/dnsmasq.d/,*.conf\n" > /etc/dnsmasq.conf \
 && mkdir /etc/service/dnsmasq \
 && echo -e '#!/bin/sh' "\nexec dnsmasq -C /etc/dnsmasq.conf --keep-in-foreground" > /etc/service/dnsmasq/run \
 && chmod 755 /etc/service/dnsmasq/run
COPY start.sh /initser
RUN chmod 755 /initser/start.sh
# ###################
# #### SNIPROXY #####
# ###################
RUN apk add --update sniproxy \
 && rm -rf /var/cache/apk/* \
 && echo -e "user daemon\npidfile /var/run/sniproxy.pid\nlisten 80 {\nproto http\n}\nlisten 443 {\nproto tls\n}\ntable {\n.* *\n}\nresolver {\nnameserver 127.0.0.1\nmode ipv4_only\n}\n" > /etc/sniproxy/sniproxy.conf \
 && mkdir /etc/service/sniproxy \
 && touch /var/log/sniproxy-errors.log \
 && touch /var/log/sniproxy-access.log \
 && chmod 777 /var/log/sniproxy-errors.log \
 && chmod 777 /var/log/sniproxy-access.log \
 && echo -e '#!/bin/sh' "\nexec sniproxy -f -c /etc/sniproxy/sniproxy.conf" > /etc/service/sniproxy/run \
 && chmod 755 /etc/service/sniproxy/run
# ###################
# #### PYTHON3 ######
# ###################
RUN set -x \
 && apk add --no-cache python3 \
 && python3 -m ensurepip \
 && rm -r /usr/lib/python*/ensurepip \
 && pip3 install --upgrade pip setuptools \
 && if [ ! -e /usr/bin/pip ] ; then ln -s pip3 /usr/bin/pip ; fi \
 && if [[ ! -e /usr/bin/python ]] ; then ln -sf /usr/bin/python3 /usr/bin/python ; fi \
 && rm -r /root/.cache
# ###################
# #### openpyn ######
# ###################
RUN apk add --update openvpn unzip wget sudo iputils expect \
 && rm -rf /var/cache/apk/* \
 && python3 -m pip install --upgrade openpyn \
 && echo -e '#!/usr/bin/expect -f' > /initser/setup_openpyn.sh \
 && echo -e "\n\nset username [lindex $argv 0]\nset password [lindex $argv 1]\n\nset timeout -1\nspawn openpyn --init\nmatch_max 100000\nexpect \"*\"\nexpect \"Enter your username\"\n" >> /initser/setup_openpyn.sh \
 && echo -e 'send -- "$username\\r"' "\n" >> /initser/setup_openpyn.sh \
 && echo -e 'expect "Enter the password"' "\n" >> /initser/setup_openpyn.sh \
 && echo -e 'send -- "$password\\r"' "\n" >> /initser/setup_openpyn.sh \
 && echo -e 'expect "*"' "\n" >> /initser/setup_openpyn.sh \
 && echo -e 'send -- "\\r"' "\n" >> /initser/setup_openpyn.sh \
 && echo -e "expect eof\n" >> /initser/setup_openpyn.sh \
 && chmod 755 /initser/setup_openpyn.sh \
 && mkdir /etc/service/openpyn \
 && echo -e '#!/bin/sh' "\nexec openpyn $VPN_OPTIONS" > /etc/service/openpyn/run \
 && chmod 755 /etc/service/openpyn/run
#  Final cleanup
RUN rm -rf /var/cache/apk/* /tmp/* /var/tmp/*
CMD ["/initser/start.sh"]
