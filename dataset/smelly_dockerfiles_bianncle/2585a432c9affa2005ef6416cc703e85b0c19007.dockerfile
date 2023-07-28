# ####################
#   Building Stage   #
# ####################
FROM gitlab/gitlab-ce:{IMAGE_TAG} AS builder
ENV GITLAB_DIR="/opt/gitlab/embedded/service/gitlab-rails"
ENV GITLAB_GIT_ZH="https://gitlab.com/xhang/gitlab.git"
#  Reference:
#  * https://gitlab.com/gitlab-org/omnibus-gitlab/blob/master/config/software/gitlab-rails.rb
#  * https://gitlab.com/gitlab-org/gitlab-ce/blob/master/.gitlab-ci.yml
RUN set -xe \
 && echo " # Preparing ..." \
 && export DEBIAN_FRONTEND=noninteractive \
 && export SSL_CERT_DIR=/etc/ssl/certs/ \
 && export GIT_SSL_CAPATH=/etc/ssl/certs/ \
 && curl -sL https://deb.nodesource.com/setup_8.x | bash - \
 && curl -sS https://dl.yarnpkg.com/debian/pubkey.gpg | apt-key add - \
 && echo "deb https://dl.yarnpkg.com/debian/ stable main" | tee /etc/apt/sources.list.d/yarn.list \
 && apt-get update \
 && apt-get install lsb-release patch nodejs python build-essential yarn cmake -yqq
RUN set -xe \
 && echo " # Generating translation patch ..." \
 && cd /tmp \
 && git clone ${GITLAB_GIT_ZH} gitlab \
 && cd gitlab \
 && export IGNORE_DIRS=':!qa :!spec :!features :!.gitignore :!locale :!app/assets/ :!vendor/assets/' \
 && git diff --diff-filter=d {COMMIT_UPSTREAM}..origin/{COMMIT_ZH} -- . ${IGNORE_DIRS} > ../zh_CN.diff \
 && echo " # Patching ..." \
 && patch -d ${GITLAB_DIR} -p1 < ../zh_CN.diff \
 && echo " # Copy assets files ..." \
 && git checkout {COMMIT_ZH} \
 && cp -R locale ${GITLAB_DIR}/ \
 && mkdir -p ${GITLAB_DIR}/app \
 && cp -R app/assets ${GITLAB_DIR}/app/ \
 && mkdir -p ${GITLAB_DIR}/vendor \
 && cp -R vendor/assets ${GITLAB_DIR}/vendor/
RUN set -xe \
 && echo " # Install missing Gem packages ..." \
 && cd ${GITLAB_DIR} \
 && bundle install
RUN set -xe \
 && echo " # Regenerating the assets" \
 && cd ${GITLAB_DIR} \
 && cp config/gitlab.yml.example config/gitlab.yml \
 && cp config/database.yml.postgresql config/database.yml \
 && cp config/secrets.yml.example config/secrets.yml \
 && rm -rf public/assets \
 && export REVISION=$( cat ${GITLAB_DIR}/REVISION ;) \
 && export NODE_ENV=production \
 && export RAILS_ENV=production \
 && export SETUP_DB=false \
 && export USE_DB=false \
 && export SKIP_STORAGE_VALIDATION=true \
 && export WEBPACK_REPORT=true \
 && export NO_COMPRESSION=true \
 && export NO_PRIVILEGE_DROP=true \
 && export NODE_OPTIONS=--max-old-space-size=4086 \
 && yarn install \
 && bundle exec rake gettext:compile \
 && bundle exec rake gitlab:assets:compile
RUN set -xe \
 && echo " # Cleaning ..." \
 && yarn cache clean \
 && rm -rf log tmp config/gitlab.yml config/database.yml config/secrets.yml .secret .gitlab_shell_secret .gitlab_workhorse_secret app/assets node_modules \
 && find /usr/lib/ -name __pycache__ | xargs rm -rf \
 && rm -rf /tmp/gitlab /tmp/*.diff /root/.cache /var/lib/apt/lists/*
# #####################
#   Production Stage  #
# #####################
FROM gitlab/gitlab-ce:{IMAGE_TAG} AS production
RUN set -xe \
 && export DEBIAN_FRONTEND=noninteractive \
 && apt-get update \
 && apt-get install locales tzdata -yqq \
 && locale-gen en_US.UTF-8 \
 && rm -rf /var/lib/apt/lists/*
ENV LANG="en_US.UTF-8" \
    LANGUAGE="en_US:en" \
    LC_ALL="en_US.UTF-8"
ENV TZ="Asia/Shanghai"
ENV GITLAB_VERSION="{COMMIT_UPSTREAM}"
ENV GITLAB_DIR="/opt/gitlab/embedded/service/gitlab-rails"
ENV GITLAB_GIT_ZH="https://gitlab.com/xhang/gitlab.git"
ENV GITLAB_GIT_COMMIT_UPSTREAM="{COMMIT_UPSTREAM}"
ENV GITLAB_GIT_COMMIT_ZH="{COMMIT_ZH}"
COPY --from=builder ${GITLAB_DIR}/app ${GITLAB_DIR}/app
COPY --from=builder ${GITLAB_DIR}/public ${GITLAB_DIR}/public
COPY --from=builder ${GITLAB_DIR}/config/application.rb ${GITLAB_DIR}/config/application.rb
COPY --from=builder ${GITLAB_DIR}/config/initializers ${GITLAB_DIR}/config/initializers
COPY --from=builder ${GITLAB_DIR}/config/locales ${GITLAB_DIR}/config/locales
COPY --from=builder ${GITLAB_DIR}/lib/gitlab ${GITLAB_DIR}/lib/gitlab
COPY --from=builder ${GITLAB_DIR}/locale ${GITLAB_DIR}/locale
