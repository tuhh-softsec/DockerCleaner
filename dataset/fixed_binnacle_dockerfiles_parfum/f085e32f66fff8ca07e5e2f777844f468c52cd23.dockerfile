FROM debian:9.6-slim
#  install package
RUN apt-get update -y \
 && apt-get install --no-install-recommends build-essential python-setuptools wget cron sudo locales git supervisor nginx gettext-base libssl1.0-dev -y \
 && apt-get autoremove --purge \
 && apt-get clean
#  [work around] for  "cmd": "chsh frappe -s $(which bash)", "stderr": "Password: chsh: PAM: Authentication failure"
#  caused by > bench/playbooks/create_user.yml > shell: "chsh {{ frappe_user }} -s $(which bash)"
RUN sed -i 's/auth required pam_shells.so/auth sufficient pam_shells.so/' /etc/pam.d/chsh
#  set locales
RUN sed -i -e 's/# en_US.UTF-8 UTF-8/en_US.UTF-8 UTF-8/' /etc/locale.gen \
 && locale-gen
ENV LC_ALL="en_US.UTF-8" \
    LC_CTYPE="en_US.UTF-8" \
    LANG="en_US.UTF-8"
#  manually install mariadb
RUN apt-get update \
 && apt-get install --no-install-recommends software-properties-common dirmngr -y \
 && apt-key adv --no-tty --recv-keys --keyserver hkp://keyserver.ubuntu.com:80 0xF1656F24C74CD1D8 \
 && add-apt-repository 'deb [arch=amd64,i386,ppc64el] http://nyc2.mirrors.digitalocean.com/mariadb/repo/10.3/debian stretch main' \
 && export DEBIAN_FRONTEND=noninteractive \
 && apt-get update \
 && apt-get install --no-install-recommends mariadb-server mariadb-client mariadb-common libmariadbclient18 python-mysqldb python3-mysqldb -y
#  add users without sudo password
ENV systemUser="frappe"
RUN adduser --disabled-password --gecos "" $systemUser \
 && usermod -aG sudo $systemUser \
 && echo "%sudo ALL=(ALL) NOPASSWD: ALL" > /etc/sudoers.d/sudoers
#  set user and workdir
USER $systemUser
WORKDIR /home/$systemUser
COPY ./mariadb.cnf /etc/mysql/conf.d/mariadb.cnf
#  install prerequisite for bench with easy install script
ENV easyinstallRepo="https://raw.githubusercontent.com/frappe/bench/master/playbooks/install.py" \
    benchPath="bench-repo" \
    benchBranch="master" \
    benchFolderName="bench" \
    benchRepo="https://github.com/frappe/bench" \
    frappeRepo="https://github.com/frappe/frappe" \
    erpnextRepo="https://github.com/frappe/erpnext" \
    siteName="site1.local" \
    adminPass="12345" \
    mysqlPass="travis"
#  for python 2 use = python
#  for python 3 use = python3 or python3.6 for centos
ARG pythonVersion=python
ARG appBranch=master
RUN sudo service mysql start \
 && mysql --user="root" --execute="ALTER USER 'root'@'localhost' IDENTIFIED BY 'travis';" \
 && git clone $benchRepo /tmp/.bench --depth 1 --branch $benchBranch \
 && wget $easyinstallRepo \
 && sed -i '/mariadb/d' /tmp/.bench/playbooks/site.yml \
 && python install.py --without-bench-setup \
 && rm -rf bench \
 && git clone --branch $benchBranch --depth 1 --origin upstream $benchRepo $benchPath \
 && sudo pip install -e $benchPath \
 && bench init $benchFolderName --frappe-path $frappeRepo --frappe-branch $appBranch --python $pythonVersion \
 && cd $benchFolderName \
 && bench get-app erpnext $erpnextRepo --branch $appBranch \
 && bench update --patch \
 && rm -rf apps/frappe_io apps/foundation \
 && sed -i '/foundation\|frappe_io/d' sites/apps.txt \
 && sudo rm -rf /tmp/* \
 && sudo apt-get autoremove --purge -y \
 && sudo apt-get clean \
 && sudo service mysql start \
 && bench new-site $siteName --mariadb-root-password $mysqlPass --admin-password $adminPass \
 && bench --site $siteName install-app erpnext
#  [work around] change back config for work around for  "cmd": "chsh frappe -s $(which bash)", "stderr": "Password: chsh: PAM: Authentication failure"
RUN sudo sed -i 's/auth sufficient pam_shells.so/auth required pam_shells.so/' /etc/pam.d/chsh
#  set user and workdir
USER $systemUser
WORKDIR /home/$systemUser/$benchFolderName
#  copy production config
COPY production_setup/conf/frappe-docker-conf /home/$systemUser/production_config
#  run start mysql service and start bench when container start
COPY entrypoint.sh /usr/local/bin/
#  fix for [docker Error response from daemon OCI runtime create failed starting container process caused "permission denied" unknown]
RUN sudo chmod +x /home/$systemUser/production_config/entrypoint_prd.sh \
 && sudo chmod +x /usr/local/bin/entrypoint.sh
#  image entrypoint script
CMD ["/usr/local/bin/entrypoint.sh"]
#  expose port
EXPOSE 8000-8005 9000-9005 3306-3307
