FROM continuumio/miniconda3
#  ##############
#   ENVIRONMENT #
#  ##############
# A secret has been removed here. Please do not provide secrets from the Dockerfile as these will leak into the metadata of the resulting docker image. To provide secrets the --secret flag of the docker build command can be used (https://docs.docker.com/develop/develop-images/build_enhancements/#new-docker-build-secret-information).
ENV TETHYS_HOME="/usr/lib/tethys" \
    TETHYS_PORT="8000" \
    TETHYS_PUBLIC_HOST="127.0.0.1" \
    TETHYS_DB_USERNAME="tethys_default" \
    TETHYS_DB_HOST="db" \
    TETHYS_DB_PORT="5432" \
    TETHYS_SUPER_USER="" \
    TETHYS_SUPER_USER_EMAIL="" \
    TETHYS_SUPER_USER_PASS=""
#   Misc
ENV ALLOWED_HOSTS="\"['localhost', '127.0.0.1']\"" \
    BASH_PROFILE=".bashrc" \
    CONDA_HOME="/opt/conda" \
    CONDA_ENV_NAME="tethys" \
    ASGI_PROCESSES="4" \
    CLIENT_MAX_BODY_SIZE="75M"
#  ########
#   SETUP #
#  ########
RUN mkdir -p "${TETHYS_HOME}/src"
WORKDIR ${TETHYS_HOME}
#   Speed up APT installs
RUN echo "force-unsafe-io" > /etc/dpkg/dpkg.cfg.d/02apt-speedup; echo "Acquire::http {No-Cache=True;};" > /etc/apt/apt.conf.d/no-cache
#   Install APT packages
RUN apt-get update \
 && apt-get install --no-install-recommends wget=1.21.3-1ubuntu1 gnupg2=2.2.40-1ubuntu2 -y \
 && wget -O - https://repo.saltstack.com/apt/debian/9/amd64/latest/SALTSTACK-GPG-KEY.pub | apt-key add - \
 && echo "deb http://repo.saltstack.com/apt/debian/9/amd64/latest stretch main" > /etc/apt/sources.list.d/saltstack.list
RUN apt-get update \
 && apt-get install --no-install-recommends bzip2=1.0.8-5build1 git=1:2.39.2-1ubuntu1 nginx=1.22.0-1ubuntu3 supervisor=4.2.1-1ubuntu1 gcc=4:12.2.0-3ubuntu1 salt-minion procps=2:4.0.3-1ubuntu1 pv=1.6.20-1 -y
RUN rm -f /etc/nginx/sites-enabled/default
#   Setup Conda Environment
COPY environment.yml ${TETHYS_HOME}/src/
WORKDIR ${TETHYS_HOME}/src
RUN ${CONDA_HOME}/bin/conda env create -n "${CONDA_ENV_NAME}" -f "environment.yml"
#  ##########
#   INSTALL #
#  ##########
#  Setup Nginx User:
RUN groupadd www ; useradd -r -u 1011 -g www www ; sed -i 's/^user.*/user www www;/' /etc/nginx/nginx.conf
#   ADD files from repo
COPY --chown=www:www resources ${TETHYS_HOME}/src/resources/
COPY --chown=www:www templates ${TETHYS_HOME}/src/templates/
COPY --chown=www:www tethys_apps ${TETHYS_HOME}/src/tethys_apps/
COPY --chown=www:www tethys_compute ${TETHYS_HOME}/src/tethys_compute/
COPY --chown=www:www tethys_config ${TETHYS_HOME}/src/tethys_config/
COPY --chown=www:www tethys_gizmos ${TETHYS_HOME}/src/tethys_gizmos/
COPY --chown=www:www tethys_portal ${TETHYS_HOME}/src/tethys_portal/
COPY --chown=www:www tethys_quotas ${TETHYS_HOME}/src/tethys_quotas/
COPY --chown=www:www tethys_sdk ${TETHYS_HOME}/src/tethys_sdk/
COPY --chown=www:www tethys_services ${TETHYS_HOME}/src/tethys_services/
COPY --chown=www:www README.rst ${TETHYS_HOME}/src/
COPY --chown=www:www *.py ${TETHYS_HOME}/src/
#   Remove any apps that may have been installed in tethysapp
RUN rm -rf ${TETHYS_HOME}/src/tethys_apps/tethysapp ; mkdir -p ${TETHYS_HOME}/src/tethys_apps/tethysapp
COPY --chown=www:www tethys_apps/tethysapp/__init__.py ${TETHYS_HOME}/src/tethys_apps/tethysapp/
#   Run Installer
RUN /bin/bash -c '. ${CONDA_HOME}/bin/activate ${CONDA_ENV_NAME} ; python setup.py develop'
RUN mkdir ${TETHYS_HOME}/workspaces ${TETHYS_HOME}/apps ${TETHYS_HOME}/static
#   Add static files
COPY --chown=www:www static ${TETHYS_HOME}/src/static/
#   Generate Inital Settings Files
RUN /bin/bash -c '. ${CONDA_HOME}/bin/activate ${CONDA_ENV_NAME} ; tethys gen settings --production --allowed-host "${ALLOWED_HOSTS}" --db-username ${TETHYS_DB_USERNAME} --db-password ${TETHYS_DB_PASSWORD} --db-port ${TETHYS_DB_PORT} --overwrite ; sed -i -e "s:#TETHYS_WORKSPACES_ROOT = .*$:TETHYS_WORKSPACES_ROOT = \"/usr/lib/tethys/workspaces\":" ${TETHYS_HOME}/src/tethys_portal/settings.py ; tethys gen nginx --overwrite ; tethys gen nginx_service --overwrite ; tethys gen asgi_service --overwrite ; tethys manage collectstatic'
#  ###########
#   CLEAN UP #
#  ###########
RUN apt-get -y remove wget gcc gnupg2 ; apt-get -y autoremove ; apt-get -y clean
#  ########################
#   CONFIGURE  ENVIRONMENT#
#  ########################
ENV PATH="${CONDA_HOME}/miniconda/envs/tethys/bin:$PATH "
VOLUME ["${TETHYS_HOME}/workspaces", "${TETHYS_HOME}/keys"]
EXPOSE 80/tcp
#  ##############*
#   COPY IN SALT #
#  ##############*
COPY docker/salt/ /srv/salt/
COPY docker/run.sh ${TETHYS_HOME}/
#  #######
#   RUN! #
#  #######
WORKDIR ${TETHYS_HOME}
#   Create Salt configuration based on ENVs
CMD bash run.sh
HEALTHCHECK --start-period=240s CMD ps $( cat $( grep 'pidfile=.*' /etc/supervisor/supervisord.conf | awk -F'=' '{print $2}' | awk '{print $1}' ;) ;) > /dev/null
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
