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
 && apt-get install -qy apache2 build-essential curl ghostscript git gnupg imagemagick inkscape libapache2-mod-wsgi-py3 libfile-mimeinfo-perl libimage-exiftool-perl libjpeg-dev libmagickwand-dev libpq-dev libreoffice locales mysql-client poppler-utils postgresql-client python3 python3-dev python3-pip python3-venv qpdf redis-server supervisor uwsgi uwsgi-plugin-python3 vim zlib1g-dev \
 && curl -sL https://deb.nodesource.com/setup_8.x | bash - \
 && apt-get update \
 && apt-get install -qy nodejs \
 && get_source_from_github $TAG $BRANCH \
 && cd /tracim/ \
 && ./install_frontend_dependencies.sh root \
 && ./build_full_frontend.sh \
 && cd backend \
 && pip3 install -r requirements.txt \
 && pip3 install -e ".[postgresql]" \
 && pip3 install -e ".[mysql]" \
 && npm install i18next-conv -g \
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
