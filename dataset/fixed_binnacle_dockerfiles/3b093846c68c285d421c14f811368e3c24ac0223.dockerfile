FROM debian:stretch
ENV PYTHONIOENCODING="UTF-8"
#   Pypy is installed from a package manager because it takes so long to build.
RUN apt-get update \
 && apt-get install --no-install-recommends build-essential=12.3 libcurl4-openssl-dev=7.52.1-5+deb9u16 libffi-dev=3.2.1-6 tk-dev=8.6.0+9 xz-utils=5.2.2-1.2+deb9u1 curl=7.52.1-5+deb9u16 lsb-release=9.20161125 git=1:2.11.0-3+deb9u7 libmemcached-dev=1.0.18-4.1 make=4.1-9.1 liblzma-dev=5.2.2-1.2+deb9u1 libreadline-dev=7.0-3 libbz2-dev=1.0.6-8.1 llvm=1:3.8-36 libncurses5-dev=6.0+20161126-1+deb9u2 libsqlite3-dev=3.16.2-5+deb9u3 wget=1.18-5+deb9u3 pypy=5.6.0+dfsg-4 python-openssl=16.2.0-1 libncursesw5-dev=6.0+20161126-1+deb9u2 zlib1g-dev=1:1.2.8.dfsg-5+deb9u1 pkg-config=0.29-4+b1 libssl1.0-dev=1.0.2u-1~deb9u7 -y
#   Setup variables. Even though changing these may cause unnecessary invalidation of
#   unrelated elements, grouping them together makes the Dockerfile read better.
ENV PROVISIONING="/provisioning"
ARG CELERY_USER=developer
#   Check for mandatory build arguments
RUN : "${CELERY_USER:?CELERY_USER build argument needs to be set and non-empty.}"
ENV HOME="/home/$CELERY_USER"
ENV PATH="$HOME/.pyenv/bin:$PATH"
#   Copy and run setup scripts
WORKDIR $PROVISIONING
COPY docker/scripts/install-couchbase.sh .
#   Scripts will lose thier executable flags on copy. To avoid the extra instructions
#   we call the shell directly.
RUN sh install-couchbase.sh
COPY docker/scripts/create-linux-user.sh .
RUN sh create-linux-user.sh
#   Swap to the celery user so packages and celery are not installed as root.
USER $CELERY_USER
COPY docker/scripts/install-pyenv.sh .
RUN sh install-pyenv.sh
#   Install celery
WORKDIR $HOME
COPY --chown=1000:1000 requirements $HOME/requirements
COPY --chown=1000:1000 docker/entrypoint /entrypoint
RUN chmod gu+x /entrypoint
#   Define the local pyenvs
RUN pyenv local python3.6 python3.5 python3.4 python2.7 python3.7
RUN pyenv exec python2.7 -m pip install --upgrade pip setuptools \
 && pyenv exec python3.4 -m pip install --upgrade pip setuptools \
 && pyenv exec python3.5 -m pip install --upgrade pip setuptools \
 && pyenv exec python3.6 -m pip install --upgrade pip setuptools \
 && pyenv exec python3.7 -m pip install --upgrade pip setuptools
#   Setup one celery environment for basic development use
RUN pyenv exec python3.7 -m pip install -r requirements/default.txt -r requirements/test.txt -r requirements/test-ci-default.txt -r requirements/docs.txt -r requirements/test-integration.txt -r requirements/pkgutils.txt \
 && pyenv exec python3.6 -m pip install -r requirements/default.txt -r requirements/test.txt -r requirements/test-ci-default.txt -r requirements/docs.txt -r requirements/test-integration.txt -r requirements/pkgutils.txt \
 && pyenv exec python3.5 -m pip install -r requirements/default.txt -r requirements/test.txt -r requirements/test-ci-default.txt -r requirements/docs.txt -r requirements/test-integration.txt -r requirements/pkgutils.txt \
 && pyenv exec python3.4 -m pip install -r requirements/default.txt -r requirements/test.txt -r requirements/test-ci-default.txt -r requirements/docs.txt -r requirements/test-integration.txt -r requirements/pkgutils.txt \
 && pyenv exec python2.7 -m pip install -r requirements/default.txt -r requirements/test.txt -r requirements/test-ci-default.txt -r requirements/docs.txt -r requirements/test-integration.txt -r requirements/pkgutils.txt
COPY --chown=1000:1000 . $HOME/celery
WORKDIR $HOME/celery
#   Setup the entrypoint, this ensures pyenv is initialized when a container is started
#   and that any compiled files from earlier steps or from moutns are removed to avoid
#   py.test failing with an ImportMismatchError
ENTRYPOINT ["/entrypoint"]
# Please add your HEALTHCHECK here!!!
