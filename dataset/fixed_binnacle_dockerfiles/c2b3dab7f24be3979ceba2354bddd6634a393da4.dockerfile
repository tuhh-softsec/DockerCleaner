FROM ubuntu:16.04
MAINTAINER pastakhov@yandex.ru
#   Install requered packages
RUN set -x ; apt-get update \
 && apt-get install --no-install-recommends ca-certificates=20210119~16.04.1 apache2=2.4.18-2ubuntu3.17 libapache2-mod-php=1:7.0+35ubuntu6.1 php=1:7.0+35ubuntu6.1 php-mysql=1:7.0+35ubuntu6.1 php-cli=1:7.0+35ubuntu6.1 php-gd=1:7.0+35ubuntu6.1 php-curl=1:7.0+35ubuntu6.1 php-mbstring=1:7.0+35ubuntu6.1 php-xml=1:7.0+35ubuntu6.1 php-apcu=5.1.3+4.0.10-1build1 php-intl=1:7.0+35ubuntu6.1 php-zip=1:7.0+35ubuntu6.1 php-memcached=2.2.0-51-ge573a6e+2.2.0-2build2 php-pear=1:1.10.1+submodules+notgz-6ubuntu0.3 python-pygments=2.1+dfsg-1ubuntu0.2 imagemagick=8:6.8.9.9-7ubuntu5.16 netcat=1.10-41 git=1:2.7.4-0ubuntu1.10 composer=1.0.0~beta2-1 unzip=6.0-20ubuntu1.1 mysql-client=5.7.33-0ubuntu0.16.04.1 wget=1.17.1-1ubuntu1.5 -y \
 && pear install mail net_smtp \
 && rm -rf /var/lib/apt/lists/* \
 && rm -rf /var/cache/apt/archives/* \
 && a2enmod rewrite \
 && rm /var/www/html/index.html \
 && rm -rf /etc/apache2/sites-enabled/*
ENV MW_VERSION="REL1_29" \
    MW_HOME="/var/www/html/w" \
    MW_VOLUME="/mediawiki" \
    WWW_USER="www-data" \
    WWW_GROUP="www-data" \
    APACHE_LOG_DIR="/var/log/apache2"
#   logs should go to stdout / stderr
RUN set -ex \
 && ln -sfT /dev/stderr "$APACHE_LOG_DIR/error.log" \
 && ln -sfT /dev/stdout "$APACHE_LOG_DIR/access.log" \
 && ln -sfT /dev/stdout "$APACHE_LOG_DIR/other_vhosts_access.log"
#  #### MediaWiki Core setup
RUN set -x ; mkdir -p $MW_HOME \
 && git clone --depth 1 -b $MW_VERSION https://gerrit.wikimedia.org/r/p/mediawiki/core.git $MW_HOME \
 && cd $MW_HOME \
 && composer install --no-dev \
 && chown -R $WWW_USER:$WWW_GROUP images/
#  #### Bundled skins, see https://www.mediawiki.org/wiki/Bundled_extensions
RUN set -x ; cd $MW_HOME/skins \
 && git clone --depth 1 -b $MW_VERSION https://gerrit.wikimedia.org/r/mediawiki/skins/Vector \
 && git clone --depth 1 -b $MW_VERSION https://gerrit.wikimedia.org/r/mediawiki/skins/Modern \
 && git clone --depth 1 -b $MW_VERSION https://gerrit.wikimedia.org/r/mediawiki/skins/MonoBook \
 && git clone --depth 1 -b $MW_VERSION https://gerrit.wikimedia.org/r/mediawiki/skins/CologneBlue
#  #### Bundled extensions, see https://www.mediawiki.org/wiki/Bundled_extensions
RUN set -x ; cd $MW_HOME/extensions \
 && git clone --depth 1 -b $MW_VERSION https://gerrit.wikimedia.org/r/p/mediawiki/extensions/ConfirmEdit \
 && git clone --depth 1 -b $MW_VERSION https://gerrit.wikimedia.org/r/p/mediawiki/extensions/Gadgets \
 && git clone --depth 1 -b $MW_VERSION https://gerrit.wikimedia.org/r/p/mediawiki/extensions/Nuke \
 && git clone --depth 1 -b $MW_VERSION https://gerrit.wikimedia.org/r/p/mediawiki/extensions/ParserFunctions \
 && git clone --depth 1 -b $MW_VERSION https://gerrit.wikimedia.org/r/p/mediawiki/extensions/Renameuser \
 && git clone --depth 1 -b $MW_VERSION https://gerrit.wikimedia.org/r/p/mediawiki/extensions/WikiEditor \
 && git clone --depth 1 -b $MW_VERSION https://gerrit.wikimedia.org/r/p/mediawiki/extensions/Cite \
 && git clone --depth 1 -b $MW_VERSION https://gerrit.wikimedia.org/r/p/mediawiki/extensions/ImageMap \
 && git clone --depth 1 -b $MW_VERSION https://gerrit.wikimedia.org/r/p/mediawiki/extensions/InputBox \
 && git clone --depth 1 -b $MW_VERSION https://gerrit.wikimedia.org/r/p/mediawiki/extensions/Interwiki \
 && git clone --depth 1 -b $MW_VERSION https://gerrit.wikimedia.org/r/p/mediawiki/extensions/LocalisationUpdate \
 && git clone --depth 1 -b $MW_VERSION https://gerrit.wikimedia.org/r/p/mediawiki/extensions/PdfHandler \
 && git clone --depth 1 -b $MW_VERSION https://gerrit.wikimedia.org/r/p/mediawiki/extensions/Poem \
 && git clone --depth 1 -b $MW_VERSION https://gerrit.wikimedia.org/r/p/mediawiki/extensions/SpamBlacklist \
 && git clone --depth 1 -b $MW_VERSION https://gerrit.wikimedia.org/r/p/mediawiki/extensions/TitleBlacklist \
 && git clone --depth 1 -b $MW_VERSION https://gerrit.wikimedia.org/r/p/mediawiki/extensions/CiteThisPage
#   SyntaxHighlight_GeSHi
RUN set -x ; cd $MW_HOME/extensions \
 && git clone --depth 1 -b $MW_VERSION https://gerrit.wikimedia.org/r/p/mediawiki/extensions/SyntaxHighlight_GeSHi \
 && cd SyntaxHighlight_GeSHi \
 && composer install --no-dev \
 && chmod a+x pygments/pygmentize \
 && cd ..
#  #### Commonly used extensions
RUN set -x ; cd $MW_HOME/extensions \
 && git clone --depth 1 -b $MW_VERSION https://gerrit.wikimedia.org/r/p/mediawiki/extensions/Echo \
 && git clone --depth 1 -b $MW_VERSION https://gerrit.wikimedia.org/r/p/mediawiki/extensions/Thanks \
 && git clone --depth 1 -b $MW_VERSION https://gerrit.wikimedia.org/r/p/mediawiki/extensions/CheckUser
#   Flow extension
RUN set -x ; cd $MW_HOME/extensions \
 && git clone --depth 1 -b $MW_VERSION https://gerrit.wikimedia.org/r/p/mediawiki/extensions/Flow \
 && cd Flow \
 && composer install --no-dev \
 && cd ..
#  ## MediaWiki Language Extension Bundle
#   Translate
RUN set -x ; cd $MW_HOME/extensions \
 && git clone --depth 1 -b $MW_VERSION https://gerrit.wikimedia.org/r/p/mediawiki/extensions/Babel \
 && git clone --depth 1 -b $MW_VERSION https://gerrit.wikimedia.org/r/p/mediawiki/extensions/cldr \
 && git clone --depth 1 -b $MW_VERSION https://gerrit.wikimedia.org/r/p/mediawiki/extensions/CleanChanges \
 && git clone --depth 1 -b $MW_VERSION https://gerrit.wikimedia.org/r/p/mediawiki/extensions/UniversalLanguageSelector
#  #### VisualEditor extension
RUN set -x ; cd $MW_HOME/extensions \
 && git clone --depth 1 -b $MW_VERSION https://gerrit.wikimedia.org/r/p/mediawiki/extensions/VisualEditor \
 && cd VisualEditor \
 && git submodule update --init
#  #### ElasticSearch extensions
RUN set -x ; cd $MW_HOME/extensions \
 && git clone --depth 1 -b $MW_VERSION https://gerrit.wikimedia.org/r/p/mediawiki/extensions/CirrusSearch \
 && git clone --depth 1 -b $MW_VERSION https://gerrit.wikimedia.org/r/p/mediawiki/extensions/Elastica \
 && cd Elastica \
 && composer install --no-dev \
 && cd ..
#  #### MultimediaViewer extension
RUN set -x ; cd $MW_HOME/extensions \
 && git clone --depth 1 -b $MW_VERSION https://gerrit.wikimedia.org/r/p/mediawiki/extensions/MultimediaViewer
#  #### MobileFrontend extension
RUN set -x ; cd $MW_HOME/extensions \
 && git clone --depth 1 -b $MW_VERSION https://gerrit.wikimedia.org/r/p/mediawiki/extensions/MobileFrontend
#  #### ElectronPdfService extension
RUN set -x ; cd $MW_HOME/extensions \
 && git clone --depth 1 -b $MW_VERSION https://gerrit.wikimedia.org/r/p/mediawiki/extensions/ElectronPdfService
#  ### Run maintenance sripts
#   Increase value for run maintenance script before web service started
ENV MW_AUTOUPDATE="true" \
    MW_MAINTENANCE_UPDATE="3" \
    MW_MAINTENANCE_CIRRUSSEARCH_UPDATECONFIG="1" \
    MW_MAINTENANCE_CIRRUSSEARCH_FORCEINDEX="1" \
    MW_MAINTENANCE_ULS_INDEXER="1" \
    MW_SCRIPT_CLDR_REBUILD="1" \
    MW_SITE_NAME="My MediaWiki Site" \
    MW_SITE_LANG="en" \
    MW_DEFAULT_SKIN="vector" \
    MW_MAIN_CACHE_TYPE="CACHE_ACCEL" \
    MW_REST_DOMAIN="localhost" \
    MW_REST_RESTBASE_PROXY_PATH="/api/rest_" \
    MW_REST_RESTBASE_PORT="7231" \
    MW_SHOW_EXCEPTION_DETAILS="true" \
    PHP_LOG_ERRORS="On" \
    PHP_ERROR_REPORTING="E_ALL"
EXPOSE 80/tcp
COPY php.ini /etc/php/7.0/apache2/conf.d/mediawiki.ini
COPY mediawiki.conf /etc/apache2/sites-available/000-mediawiki.conf
RUN set -x ; ln -s /etc/apache2/sites-available/000-mediawiki.conf /etc/apache2/sites-enabled/000-mediawiki.conf
COPY run-apache.sh /run-apache.sh
RUN chmod -v +x /run-apache.sh
COPY DockerSettings.php $MW_HOME/DockerSettings.php
COPY favicon.ico $MW_HOME/favicon.ico
COPY logo.png $MW_HOME/logo.png
COPY CustomSettings.php $MW_HOME/CustomSettings.php
CMD ["/run-apache.sh"]
VOLUME ["$MW_HOME/images", "$MW_VOLUME"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
