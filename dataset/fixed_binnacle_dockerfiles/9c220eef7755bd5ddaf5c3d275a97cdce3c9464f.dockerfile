#   vim:set ft=dockerfile:
FROM debian:stretch
MAINTAINER "cytopia" <cytopia@everythingcli.org>
RUN set -eux \
 && apt-get update \
 && apt-get install --no-install-recommends python-apt=1.4.3 python-dev=2.7.13-2 python-jmespath=0.9.0-2 python-pip=9.0.1-2+deb9u2 python-setuptools=33.1.1-1 sudo=1.8.19p1-2.1+deb9u3 --no-install-suggests -y \
 && rm -rf /var/lib/apt/lists/* \
 && apt-get purge -y --autoremove
RUN set -eux \
 && pip install wheel==0.40.0 \
 && pip install ansible==7.4.0
#   Add user with password-less sudo
RUN set -eux \
 && useradd -m -s /bin/bash cytopia \
 && echo "cytopia ALL=(ALL) NOPASSWD: ALL" >> /etc/sudoers.d/cytopia
#   Copy files
COPY ./ /home/cytopia/ansible
RUN set -eux \
 && chown -R cytopia:cytopia /home/cytopia/ansible
#   Switch to user
USER cytopia
#   Change working directory
WORKDIR /home/cytopia/ansible
#   Systemd cannot be checked inside Docker, so replace it with a dummy role
RUN set -eux \
 && mkdir roles/dummy \
 && sed -i'' 's/systemd-meta/dummy/g' playbook.yml
#   Randomize roles to install each time the container is build (each travis run)
RUN set -eux \
 && ROLES_INSTALL="$( for d in $( /bin/ls roles/ ;); do if [ -d roles/${d} ] ; then echo $d ; fi ; done | grep -vE '*-meta$' | sort -R ;)" \
 && ROLES_REMOVE="$( for d in $( /bin/ls roles/ ;); do if [ -d roles/${d} ] ; then echo $d ; fi ; done | grep -vE '*-meta$' | sort -R ;)" \
 && (echo "#!/bin/sh -eux" ;echo ;echo "# Ansible verbosity" ;echo "if ! set | grep '^verbose=' >/dev/null 2>&1; then" ;echo " verbose=\"\"" ;echo "else" ;echo " if [ \"${verbose}\" = \"1\" ]; then" ;echo " verbose=\"-v\"" ;echo " elif [ \"${verbose}\" = \"2\" ]; then" ;echo " verbose=\"-vv\"" ;echo " elif [ \"${verbose}\" = \"3\" ]; then" ;echo " verbose=\"-vvv\"" ;echo " else" ;echo " verbose=\"\"" ;echo " fi" ;echo "fi" ;echo ;echo "# Ansible tagged role (only run a specific tag)" ;echo "if ! set | grep '^tag=' >/dev/null 2>&1; then" ;echo " tag=\"\"" ;echo "fi" ;echo ;echo "# Ansible random order (run roles in random order)" ;echo "if ! set | grep '^random=' >/dev/null 2>&1; then" ;echo " random=\"0\"" ;echo "fi" ;echo ;echo ;echo "if [ \"${tag}\" != \"\" ]; then" ;echo " role=\"$( echo \"${tag}\" | sed 's/-/_/g' ;)\"" ;echo " # [install] (only tag)" ;echo " ansible-playbook -i inventory playbook.yml --limit ${MY_HOST} ${verbose} --diff -t bootstrap-system-apt-repo" ;echo " ansible-playbook -i inventory playbook.yml --limit ${MY_HOST} ${verbose} --diff -t ${tag}" ;echo ;echo "else" ;echo ;echo " # Random" ;echo " if [ \"${random}\" = \"1\" ]; then" ;echo " # [INSTALL] Bootstrap roles" ;echo " if ! ansible-playbook -i inventory playbook.yml -t bootstrap-system --limit ${MY_HOST} ${verbose} --diff; then" ;echo " ansible-playbook -i inventory playbook.yml -t bootstrap-system --limit ${MY_HOST} ${verbose} --diff" ;echo " fi" ;echo " if ! ansible-playbook -i inventory playbook.yml -t bootstrap-python --limit ${MY_HOST} ${verbose} --diff; then" ;echo " ansible-playbook -i inventory playbook.yml -t bootstrap-python --limit ${MY_HOST} ${verbose} --diff" ;echo " fi" ;echo ;echo " # [INSTALL] Pre-defined roles (randomized)" ;for r in ${ROLES_INSTALL}; do echo " if ! ansible-playbook -i inventory playbook.yml -t ${r} --limit ${MY_HOST} ${verbose} --diff; then" ;echo " ansible-playbook -i inventory playbook.yml -t ${r} --limit ${MY_HOST} ${verbose} --diff" ;echo " fi" ; done ;echo ;echo " # [INSTALL] Custom apt packages" ;echo " if ! ansible-playbook -i inventory playbook.yml -t apt --limit ${MY_HOST} ${verbose} --diff; then" ;echo " ansible-playbook -i inventory playbook.yml -t apt --limit ${MY_HOST} ${verbose} --diff" ;echo " fi" ;echo ;echo " # [INSTALL] Default applications" ;echo " if ! ansible-playbook -i inventory playbook.yml -t xdg --limit ${MY_HOST} ${verbose} --diff; then" ;echo " ansible-playbook -i inventory playbook.yml -t xdg --limit ${MY_HOST} ${verbose} --diff" ;echo " fi" ;echo ;echo " # In order" ;echo " else" ;echo ;echo " # [INSTALL] Normal playbook" ;echo " if ! ansible-playbook -i inventory playbook.yml --limit ${MY_HOST} ${verbose} --diff; then" ;echo " ansible-playbook -i inventory playbook.yml --limit ${MY_HOST} ${verbose} --diff" ;echo " fi" ;echo " fi" ;echo ;echo " apt list --installed > install1.txt" ;echo ;echo " # Full install 2nd round" ;echo " if ! ansible-playbook -i inventory playbook.yml --limit ${MY_HOST} ${verbose} --diff; then" ;echo " ansible-playbook -i inventory playbook.yml --limit ${MY_HOST} ${verbose} --diff" ;echo " fi" ;echo ;echo " apt list --installed > install2.txt" ;echo ;echo " # Validate" ;echo " diff install1.txt install2.txt" ;echo ;echo " # [REMOVE] Pre-defined roles (randomized)" ;for r in ${ROLES_REMOVE}; do del="$( echo $r | sed 's/-/_/g' ;)=remove" ;echo " if ! ansible-playbook -i inventory playbook.yml -t ${r} --limit ${MY_HOST} ${verbose} -e ${del} --diff; then" ;echo " ansible-playbook -i inventory playbook.yml -t ${r} --limit ${MY_HOST} ${verbose} -e ${del} --diff" ;echo " fi" ;echo " if ! ansible-playbook -i inventory playbook.yml -t ${r} --limit ${MY_HOST} ${verbose} -e ${del} --diff; then" ;echo " ansible-playbook -i inventory playbook.yml -t ${r} --limit ${MY_HOST} ${verbose} -e ${del} --diff" ;echo " fi" ; done ;echo ;echo " # [REMOVE] Custom apt packages" ;echo " if ! ansible-playbook -i inventory playbook.yml -t apt --limit ${MY_HOST} ${verbose} -e apt_state=absent --diff; then" ;echo " ansible-playbook -i inventory playbook.yml -t apt --limit ${MY_HOST} ${verbose} -e apt_state=absent --diff" ;echo " fi" ;echo "fi" ) > run-tests.sh \
 && chmod +x run-tests.sh
ENTRYPOINT ["./run-tests.sh"]
# Please add your HEALTHCHECK here!!!
