FROM debian:stretch
MAINTAINER contact@tracim.fr
ARG TAG=""
ARG BRANCH=""
ENV START_WEBDAV="1"
ENV START_CALDAV="1"
ENV LANG="C.UTF-8"
ENV LANGUAGE="C.UTF-8"
ENV LC_ALL="C.UTF-8"
RUN if ([ "$TAG" != "" ] \
 && [ "$BRANCH" != "" ] ) ; then echo "Its not possible to use ARG TAG and ARG BRANCH in same time." \
 && exit 1 ; fi
RUN get_source_from_github() { if ([ "$TAG" != "" ] \
 && [ "$BRANCH" = "" ] ) ; then git clone -b $TAG --single-branch https://github.com/tracim/tracim.git tracim ; elif ([ "$TAG" = "" ] \
 && [ "$BRANCH" != "" ] ) ; then git clone https://github.com/tracim/tracim.git tracim \
 && cd tracim/ \
 && git checkout $BRANCH \
 && cd - ; else git clone https://github.com/tracim/tracim.git tracim ; fi ; } \
 && apt-get update \
 && apt-get upgrade -qy \
 && apt-get install --no-install-recommends apache2=2.4.25-3+deb9u13 build-essential=12.3 curl=7.52.1-5+deb9u16 ghostscript=9.26a~dfsg-0+deb9u9 git=1:2.11.0-3+deb9u7 gnupg=2.1.18-8~deb9u4 imagemagick=8:6.9.7.4+dfsg-11+deb9u14 inkscape=0.92.1-1 libapache2-mod-wsgi-py3=4.5.11-1 libfile-mimeinfo-perl=0.27-1 libimage-exiftool-perl=10.40-1+deb9u1 libjpeg-dev=1:1.5.1-2+deb9u2 libmagickwand-dev=8:6.9.7.4+dfsg-11+deb9u14 libpq-dev=9.6.24-0+deb9u1 libreoffice=1:5.2.7-1+deb9u11 locales=2.24-11+deb9u4 mysql-client=5.5.9999+default poppler-utils=0.48.0-2+deb9u4 postgresql-client=9.6+181+deb9u3 python3=3.5.3-1 python3-dev=3.5.3-1 python3-pip=9.0.1-2+deb9u2 python3-venv=3.5.3-1 qpdf=6.0.0-2 redis-server=3:3.2.6-3+deb9u9 supervisor=3.3.1-1+deb9u1 uwsgi=2.0.14+20161117-3+deb9u5 uwsgi-plugin-python3=2.0.14+20161117-3+deb9u5 vim=2:8.0.0197-4+deb9u7 zlib1g-dev=1:1.2.8.dfsg-5+deb9u1 -qy \
 && curl -sL https://deb.nodesource.com/setup_8.x | bash - \
 && apt-get update \
 && apt-get install --no-install-recommends nodejs=4.8.2~dfsg-1 -qy \
 && get_source_from_github $TAG $BRANCH \
 && cd /tracim/ \
 && ./install_frontend_dependencies.sh root \
 && ./build_full_frontend.sh \
 && cd backend \
 && pip3 install -r requirements.txt \
 && pip3 install -e ".[postgresql]" \
 && pip3 install -e ".[mysql]" \
 && npm install i18next-conv@13.1.1 -g \
 && ./update_i18n_json_file.sh \
 && cd ../../ \
 && mv /tracim/frontend/dist/assets/images/logo-tracim.png /tracim/frontend/dist/assets/images/logo-tracim.png.default \
 && rm -rf /tracim/frontend/node_modules \
 && rm -rf /tracim/frontend_app_admin_workspace_user/node_modules \
 && rm -rf /tracim/frontend_app_agenda/node_modules \
 && rm -rf /tracim/frontend_app_file/node_modules \
 && rm -rf /tracim/frontend_app_folder_advanced/node_modules \
 && rm -rf /tracim/frontend_app_html-document/node_modules \
 && rm -rf /tracim/frontend_app_thread/node_modules \
 && rm -rf /tracim/frontend_app_workspace/node_modules \
 && rm -rf /tracim/frontend_app_workspace_advanced/node_modules \
 && rm -rf /tracim/frontend_lib/node_modules \
 && rm /etc/apache2/sites-enabled/000-default.conf \
 && apt-get purge -yq build-essential curl git libjpeg-dev libmagickwand-dev nodejs python3-dev zlib1g-dev \
 && apt-get autoremove -qy \
 && apt-get clean -qy \
 && rm -rf /var/lib/apt/list/*
VOLUME ["/etc/tracim", "/var/tracim"]
EXPOSE 80/tcp
CMD ["/bin/bash", "/tracim/tools_docker/Debian_Uwsgi/entrypoint.sh"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
