FROM php:7.3-fpm-stretch
LABEL authors="Julien Neuhart <j.neuhart@thecodingmachine.com>, David Négrier <d.negrier@thecodingmachine.com>"
#  |--------------------------------------------------------------------------
#  | Main PHP extensions
#  |--------------------------------------------------------------------------
#  |
#  | Installs the main PHP extensions
#  |
COPY extensions/ /usr/local/lib/thecodingmachine-php/extensions
RUN ln -s 7.3 /usr/local/lib/thecodingmachine-php/extensions/current
#  Install php extensions
RUN apt-get update \
 && apt-get install --no-install-recommends git nano sudo iproute2 openssh-client procps unzip -y
RUN cd /usr/local/lib/thecodingmachine-php/extensions/current/zip \
 && ./install.sh
#  RUN echo 'extension=zip.so' > /usr/local/etc/php/conf.d/generated_conf.ini
#  |--------------------------------------------------------------------------
#  | User
#  |--------------------------------------------------------------------------
#  |
#  | Define a default user with sudo rights.
#  |
RUN useradd -ms /bin/bash docker \
 && adduser docker sudo
#  Users in the sudoers group can sudo as root without password.
RUN echo '%sudo ALL=(ALL) NOPASSWD:ALL' >> /etc/sudoers
#  |--------------------------------------------------------------------------
#  | Default php.ini file
#  |--------------------------------------------------------------------------
#  |
#  | Let's download php.ini for prod and development
#  |
COPY https://raw.githubusercontent.com/php/php-src/php-${PHP_VERSION}/php.ini-production /usr/local/etc/php/php.ini-production
COPY https://raw.githubusercontent.com/php/php-src/php-${PHP_VERSION}/php.ini-development /usr/local/etc/php/php.ini-development
RUN chmod 644 /usr/local/etc/php/php.ini-*
ENV TEMPLATE_PHP_INI="development"
#  |--------------------------------------------------------------------------
#  | Composer
#  |--------------------------------------------------------------------------
#  |
#  | Installs Composer to easily manage your PHP dependencies.
#  |
# ENV COMPOSER_ALLOW_SUPERUSER 1
RUN curl -sS https://getcomposer.org/installer | php -- --install-dir=/usr/local/bin --filename=real_composer \
 && chmod +x /usr/local/bin/real_composer
#  TODO: utils.php in /usr/local/bin... bof!
COPY utils/utils.php /usr/local/bin/utils.php
COPY utils/composer_proxy.sh /usr/local/bin/composer
COPY utils/generate_conf.php /usr/local/bin/generate_conf.php
#  |--------------------------------------------------------------------------
#  | prestissimo
#  |--------------------------------------------------------------------------
#  |
#  | Installs Prestissimo to improve Composer download performance.
#  |
USER docker
RUN composer global require hirak/prestissimo \
 && composer global require bamarni/symfony-console-autocomplete \
 && rm -rf $HOME.composer
USER root
RUN chown docker:docker /var/www/html
#  |--------------------------------------------------------------------------
#  | PATH updating
#  |--------------------------------------------------------------------------
#  |
#  | Let's add ./vendor/bin to the PATH (utility function to use Composer bin easily)
#  |
ENV PATH="$PATH:./vendor/bin:~/.composer/vendor/bin"
RUN sed -i 's#/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin#/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:./vendor/bin:~/.composer/vendor/bin#g' /etc/sudoers
USER docker
#  |--------------------------------------------------------------------------
#  | SSH client
#  |--------------------------------------------------------------------------
#  |
#  | Let's set-up the SSH client (for connections to private git repositories)
#  | We create an empty known_host file and we launch the ssh-agent
#  |
RUN mkdir ~/.ssh \
 && touch ~/.ssh/known_hosts \
 && chmod 644 ~/.ssh/known_hosts \
 && eval $( ssh-agent -s ;)
#  |--------------------------------------------------------------------------
#  | .bashrc updating
#  |--------------------------------------------------------------------------
#  |
#  | Let's update the .bashrc to add nice aliases
#  |
RUN echo 'eval "$(symfony-autocomplete)"' > ~/.bash_profile
RUN { echo "alias ls='ls --color=auto'" ;echo "alias ll='ls --color=auto -alF'" ;echo "alias la='ls --color=auto -A'" ;echo "alias l='ls --color=auto -CF'" ; } >> ~/.bashrc
USER root
#  |--------------------------------------------------------------------------
#  | NodeJS
#  |--------------------------------------------------------------------------
#  |
#  | NodeJS path registration (if we install NodeJS, this is useful).
#  |
ENV PATH="$PATH:./node_modules/.bin"
RUN sed -i 's#/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin#/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:./node_modules/.bin#g' /etc/sudoers
#  |--------------------------------------------------------------------------
#  | Entrypoint
#  |--------------------------------------------------------------------------
#  |
#  | Defines the entrypoint.
#  |
ENV IMAGE_VARIANT="fpm"
#  Add Tini (to be able to stop the container with ctrl-c.
#  See: https://github.com/krallin/tini
ENV TINI_VERSION="v0.16.1"
COPY https://github.com/krallin/tini/releases/download/${TINI_VERSION}/tini /tini
RUN chmod +x /tini
COPY utils/generate_cron.php /usr/local/bin/generate_cron.php
COPY utils/startup_commands.php /usr/local/bin/startup_commands.php
COPY utils/docker-entrypoint.sh /usr/local/bin/docker-entrypoint.sh
COPY utils/docker-entrypoint-as-root.sh /usr/local/bin/docker-entrypoint-as-root.sh
ENTRYPOINT ["/usr/local/bin/docker-entrypoint.sh"]
CMD ["php-fpm"]
USER docker
COPY utils/install_selected_extensions.php /usr/local/bin/install_selected_extensions.php
ONBUILD ARG PHP_EXTENSIONS
ONBUILD ENV PHP_EXTENSIONS="$PHP_EXTENSIONS"
ONBUILD RUN sudo -E PHP_EXTENSIONS="$PHP_EXTENSIONS" php /usr/local/bin/install_selected_extensions.php
#  |--------------------------------------------------------------------------
#  | Supercronic
#  |--------------------------------------------------------------------------
#  |
#  | Supercronic is a drop-in replacement for cron (for containers).
#  |
ONBUILD ARG INSTALL_CRON
ONBUILD RUN if [ -n "$INSTALL_CRON" ] ; then SUPERCRONIC_URL=https://github.com/aptible/supercronic/releases/download/v0.1.5/supercronic-linux-amd64 \
 && SUPERCRONIC=supercronic-linux-amd64 \
 && SUPERCRONIC_SHA1SUM=9aeb41e00cc7b71d30d33c57a2333f2c2581a201 \
 && curl -fsSLO "$SUPERCRONIC_URL" \
 && echo "${SUPERCRONIC_SHA1SUM} ${SUPERCRONIC}" | sha1sum -c - \
 && chmod +x "$SUPERCRONIC" \
 && sudo mv "$SUPERCRONIC" "/usr/local/bin/${SUPERCRONIC}" \
 && sudo ln -s "/usr/local/bin/${SUPERCRONIC}" /usr/local/bin/supercronic ; fi
#  |--------------------------------------------------------------------------
#  | NodeJS
#  |--------------------------------------------------------------------------
#  |
#  | Installs NodeJS and npm. The later will allow you to easily manage
#  | your frontend dependencies.
#  | Also installs yarn. It provides some nice improvements over npm.
#  |
ONBUILD ARG NODE_VERSION
ONBUILD RUN if [ -n "$NODE_VERSION" ] ; then sudo apt-get update \
 && sudo apt-get install -y --no-install-recommends gnupg \
 && curl -sL https://deb.nodesource.com/setup_${NODE_VERSION}.x | sudo bash - \
 && sudo apt-get update \
 && sudo apt-get install -y --no-install-recommends nodejs \
 && curl -sS https://dl.yarnpkg.com/debian/pubkey.gpg | sudo apt-key add - \
 && echo "deb https://dl.yarnpkg.com/debian/ stable main" | sudo tee /etc/apt/sources.list.d/yarn.list \
 && sudo apt-get update \
 && sudo apt-get install -y --no-install-recommends yarn ; fi
