FROM ubuntu:18.04
ARG OUR_IMAGE_VERSION=v2.2.6
ARG OUR_IMAGE_TAG=${OUR_IMAGE_TAG:-$OUR_IMAGE_VERSION}
#   flag for apt-get - affects only build time
ARG DEBIAN_FRONTEND=noninteractive
ARG DOCKRUN_PREFIX="dockrun_"
ARG hack_OUR_IMAGE="t3docs/render-documentation:${OUR_IMAGE_TAG}"
ARG hack_OUR_IMAGE_SHORT="t3rd"
ARG OUR_IMAGE_SLOGAN="t3rd - TYPO3 render documentation"
ENV LC_ALL="C.UTF-8" \
    LANG="C.UTF-8" \
    HOME="/ALL/userhome" \
    TOOLCHAIN_VERSION="2.6.1" \
    TOOLCHAIN_UNPACKED="Toolchain_RenderDocumentation-2.6.1" \
    TOOLCHAIN_URL="https://github.com/marble/Toolchain_RenderDocumentation/archive/v2.6.1.zip" \
    TYPOSCRIPT_PY_VERSION="v2.2.4" \
    TYPOSCRIPT_PY_URL="https://raw.githubusercontent.com/TYPO3-Documentation/Pygments-TypoScript-Lexer/v2.2.4/typoscript.py" \
    OUR_IMAGE="$hack_OUR_IMAGE" \
    OUR_IMAGE_SHORT="$hack_OUR_IMAGE_SHORT" \
    THEME_VERSION="3.6.16" \
    THEME_MTIME="1530870718"
LABEL Maintainer="TYPO3 Documentation Team" \
      Description="This image renders TYPO3 documentation." \
      Vendor="t3docs" \
      Version="$OUR_IMAGE_VERSION"
COPY ALL-for-build /ALL
WORKDIR /ALL/venv
RUN true "Create executable COMMENT as a workaround to allow commenting here" \
 && cp /bin/true /bin/COMMENT \
 && COMMENT "Garantee folders" \
 && mkdir /PROJECT \
 && mkdir /RESULT \
 && COMMENT "Avoid GIT bug" \
 && cp /ALL/global-gitconfig.cfg /root/.gitconfig \
 && cp /ALL/global-gitconfig.cfg /.gitconfig \
 && chmod 666 /.gitconfig \
 && COMMENT "Make sure other users can write" \
 && chmod -R o+w /ALL/Makedir /ALL/dummy_webroot /RESULT \
 && COMMENT "Install and upgrade system packages" \
 && apt-get update \
 && apt-get upgrade -qy \
 && apt-get install --no-install-recommends python-pip=9.0.1-2.3~ubuntu1.18.04.8 -yq \
 && COMMENT "What the toolchains needs" \
 && apt-get install --no-install-recommends moreutils=0.60-1 pandoc=1.19.2.4~dfsg-1build4 rsync=3.1.2-2.1ubuntu1.6 tidy=1:5.2.0-2 unzip=6.0-21ubuntu1.2 wget=1.19.4-1ubuntu2.2 -yq \
 && COMMENT "What we need - convenience tools" \
 && apt-get install --no-install-recommends less=487-0.1 nano=2.9.3-2 ncdu=1.12-1 -yq \
 && COMMENT "Try extra cleaning besides /etc/apt/apt.conf.d/docker-clean" \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/* \
 && COMMENT "Python stuff" \
 && /usr/bin/pip install --upgrade pip \
 && apt-get remove python-pip -y \
 && /usr/local/bin/pip install --upgrade pipenv \
 && COMMENT "Disable /ALL/venv/Pipfile.lock - it didn't work reliably" \
 && rm -f Pipfile.lock.DISABLED \
 && if [ -f "Pipfile.lock" ] ; then mv Pipfile.lock Pipfile.lock.DISABLED ; fi \
 && COMMENT "Install from /ALL/venv/Pipfile" \
 && pipenv install \
 && echo source $( pipenv --venv ;)/bin/activate >> $HOME/.bashrc \
 && COMMENT "Provide some special files" \
 && wget https://raw.githubusercontent.com/TYPO3-Documentation/typo3-docs-typo3-org-resources/master/userroot/scripts/bin/check_include_files.py --quiet --output-document /usr/local/bin/check_include_files.py \
 && wget https://raw.githubusercontent.com/TYPO3-Documentation/typo3-docs-typo3-org-resources/master/userroot/scripts/bin/conf-2017-09.py --quiet --output-document /ALL/Makedir/conf-2017-09.py \
 && wget https://raw.githubusercontent.com/TYPO3-Documentation/typo3-docs-typo3-org-resources/master/userroot/scripts/config/_htaccess-2016-08.txt --quiet --output-document /ALL/Makedir/_htaccess \
 && wget https://github.com/etobi/Typo3ExtensionUtils/raw/master/bin/t3xutils.phar --quiet --output-document /usr/local/bin/t3xutils.phar \
 && chmod +x /usr/local/bin/t3xutils.phar \
 && COMMENT "All files of the theme of a given theme version should have the" \
 && COMMENT "same mtime (last commit) to not turn off Sphinx caching" \
 && python=$( pipenv --venv ;)/bin/python \
 && destdir=$( dirname $( $python -c "import t3SphinxThemeRtd; print t3SphinxThemeRtd.__file__" ;) ;) \
 && find $destdir -exec touch --no-create --time=mtime --date="$( date --rfc-2822 --date=@$THEME_MTIME ;)" {}
ENTRYPOINT ["/ALL/Menu/mainmenu.sh"]
CMD []
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
