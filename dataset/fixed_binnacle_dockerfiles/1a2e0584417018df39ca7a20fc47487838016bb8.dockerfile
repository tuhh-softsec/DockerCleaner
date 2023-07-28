FROM ubuntu:12.04
MAINTAINER Jonas Colmsjö <jonas@gizur.com>
RUN echo "deb http://archive.ubuntu.com/ubuntu precise main universe" > /etc/apt/sources.list
RUN :
RUN (apt-get update ;apt-get install --no-install-recommends language-pack-en -q -y )
RUN update-locale LANG=en_US.UTF-8
#  run echo mail > /etc/hostname
COPY etc-hosts.txt /etc/hosts
#  run chown root:root /etc/hosts
RUN (apt-get update ;apt-get install --no-install-recommends vim -q -y )
#   Install Postfix.
RUN echo "postfix postfix/main_mailer_type string Internet site" > preseed.txt
RUN echo "postfix postfix/mailname string mail.example.com" >> preseed.txt
#   Use Mailbox format.
RUN debconf-set-selections preseed.txt
RUN DEBIAN_FRONTEND=noninteractive apt-get install -q -y postfix
RUN postconf -e myhostname=mail.example.com
RUN postconf -e mydestination="mail.example.com, example.com, localhost.localdomain, localhost"
RUN postconf -e mail_spool_directory="/var/spool/mail/"
RUN postconf -e mailbox_command=""
#   Add a local user to receive mail at someone@example.com, with a delivery directory
#   (for the Mailbox format).
RUN useradd -s /bin/bash someone
RUN mkdir /var/spool/mail/someone
RUN chown someone:mail /var/spool/mail/someone
COPY etc-aliases.txt /etc/aliases
#  run chown root:root /etc/aliases
#  run rm /etc/aliases.db
#  run touch /etc/aliases.db
#   run /bin/sh -c "cd /etc; postmap aliases"
#  run newaliases
#   Use syslog-ng to get Postfix logs (rsyslog uses upstart which does not seem
#   to run within Docker).
RUN (apt-get update ;apt-get install --no-install-recommends syslog-ng -q -y )
#
#   Jonas C.
#
#   Good foor debugging
RUN (apt-get update ;apt-get install --no-install-recommends mutt vim dnsutils wget curl -y )
#   Need to use smarthost when running in docker (since these IP-adresses often are blocked for spam purposes)
#   See: http://www.inboxs.com/index.php/linux-os/mail-server/52-configure-postfix-to-use-smart-host-gmail
RUN echo smtp.gmail.com USERNAME:PASSWORD > /etc/postfix/relay_passwd
RUN chmod 600 /etc/postfix/relay_passwd
RUN postmap /etc/postfix/relay_passwd
COPY etc-postfix-main.cf /etc-postfix-main.cf
RUN cat /etc-postfix-main.cf >> /etc/postfix/main.cf
#  -------------------------------------------------------------------------
#   Install apache
#
#   Keep upstart from complaining
#  RUN dpkg-divert --local --rename --add /sbin/initctl
#  RUN ln -s /bin/true /sbin/initctl
#  RUN apt-get update
#
#   Install supervidord (used to handle processes)
#
RUN (apt-get update ;apt-get install --no-install-recommends supervisor -y )
COPY ./supervisord.conf /etc/supervisor/conf.d/supervisord.conf
#
#   Install Apache
RUN (apt-get update ;apt-get install --no-install-recommends apache2 -y )
#  -------------------------------------------------------------------------
#   Install mailman
#
#  run echo "postfix postfix/mailname string mail.example.com" > preseed.txt
#  run debconf-set-selections preseed.txt
RUN DEBIAN_FRONTEND=noninteractive apt-get install -y mailman
RUN ln -s /etc/mailman/apache.conf /etc/apache2/sites-enabled/mailman
RUN /etc/init.d/apache2 restart
RUN postconf -e 'relay_domains = lists.example.com'
RUN postconf -e 'transport_maps = hash:/etc/postfix/transport'
RUN postconf -e 'mailman_destination_recipient_limit = 1'
RUN postconf -e 'alias_maps = hash:/etc/aliases, hash:/var/lib/mailman/data/aliases'
#  In /etc/postfix/master.cf double check that you have the following transport:
#
#  mailman   unix  -       n       n       -       -       pipe
#    flags=FR user=list argv=/usr/lib/mailman/bin/postfix-to-mailman.py
#    ${nexthop} ${user}
#
#  It calls the postfix-to-mailman.py script when a mail is delivered to a list.
#
#  Associate the domain lists.example.com to the mailman transport with the transport map. Edit the file /etc/postfix/transport:
#
#  lists.example.com      mailman:
COPY ./etc-mailman-mm_cfg.py /eyc/mailman/mm_cfg.py
COPY ./etc-postfix-transport /etc/postfix/transport
RUN chown root:list /etc/postfix/transport
RUN postmap -v /etc/postfix/transport
#  RUN chown root:list /var/lib/mailman/data/aliases
RUN chown root:list /etc/aliases
RUN newaliases
#  run echo "postfix postfix/mailname string mail.example.com" > preseed.txt
#  run debconf-set-selections preseed.txt
#  RUN newlist mailman
#  RUN groupadd mailman
#  RUN useradd -c "GNU Mailman" -s /no/shell -d /no/home -g mailman mailman
#  RUN mkdir -p /usr/local/mailman
#  RUN cd /usr/local/mailman; chgrp mailman .; chmod a+rx,g+ws .
#  RUN wget https://launchpad.net/mailman/3.0/3.0.0b3/+download/mailman-3.0.0b3.tar.gz
#  RUN tar -xzf mailman-3.0.0b3.tar.gz
#  RUN sudo su mailman -c "cd /usr/local/mailman/mailman-3.0.0b3; ./configure; man install"
EXPOSE 25/tcp 80/tcp
CMD ["sh", "-c", "service", "syslog-ng", "start", ";", "service", "postfix", "start", ";", "/etc/init.d/supervisor", "start", ";", "/usr/lib/mailman/bin/mailmanctl", "start", ";", "tail", "-F", "/var/log/mailman/*"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
