#
#   See the top level Makefile in https://github.com/docker/docker for usage.
#
FROM docs/base:latest
MAINTAINER Sven Dowideit <SvenDowideit@docker.com> (@SvenDowideit)
#   This section ensures we pull the correct version of each
#   sub project
ENV COMPOSE_BRANCH="release"
ENV SWARM_BRANCH="v0.2.0"
ENV MACHINE_BRANCH="docs"
ENV DISTRIB_BRANCH="docs"
ENV KITEMATIC_BRANCH="master"
#   TODO: need the full repo source to get the git version info
COPY . /src
#   Reset the /docs dir so we can replace the theme meta with the new repo's git info
#   RUN git reset --hard
#   Then copy the desired docs into the /docs/sources/ dir
COPY ./sources/ /docs/sources
COPY ./VERSION VERSION
#   adding the image spec will require Docker 1.5 and `docker build -f docs/Dockerfile .`
#  COPY ./image/spec/v1.md /docs/sources/reference/image-spec-v1.md
#   TODO: don't do this - look at merging the yml file in build.sh
COPY ./mkdocs.yml ./s3_website.json ./release.sh ./
#  ######################
#   Docker Distribution
#  #######################
#  ADD https://raw.githubusercontent.com/docker/distribution/${DISTRIB_BRANCH}/docs/mkdocs.yml /docs/mkdocs-distribution.yml
RUN which wget &> /dev/null || apt-get install --no-install-recommends wget=1.20.3 ; wget --no-verbose --output-document /docs/sources/registry/images/ https://raw.githubusercontent.com/docker/distribution/${DISTRIB_BRANCH}/docs/images/notifications.png
RUN which wget &> /dev/null || apt-get install --no-install-recommends wget=1.20.3 ; wget --no-verbose --output-document /docs/sources/registry/images/ https://raw.githubusercontent.com/docker/distribution/${DISTRIB_BRANCH}/docs/images/registry.png
RUN which wget &> /dev/null || apt-get install --no-install-recommends wget=1.20.3 ; wget --no-verbose --output-document /docs/sources/registry/ https://raw.githubusercontent.com/docker/distribution/${DISTRIB_BRANCH}/docs/index.md
RUN which wget &> /dev/null || apt-get install --no-install-recommends wget=1.20.3 ; wget --no-verbose --output-document /docs/sources/registry/ https://raw.githubusercontent.com/docker/distribution/${DISTRIB_BRANCH}/docs/deploying.md
RUN which wget &> /dev/null || apt-get install --no-install-recommends wget=1.20.3 ; wget --no-verbose --output-document /docs/sources/registry/ https://raw.githubusercontent.com/docker/distribution/${DISTRIB_BRANCH}/docs/configuration.md
RUN which wget &> /dev/null || apt-get install --no-install-recommends wget=1.20.3 ; wget --no-verbose --output-document /docs/sources/registry/ https://raw.githubusercontent.com/docker/distribution/${DISTRIB_BRANCH}/docs/storagedrivers.md
RUN which wget &> /dev/null || apt-get install --no-install-recommends wget=1.20.3 ; wget --no-verbose --output-document /docs/sources/registry/ https://raw.githubusercontent.com/docker/distribution/${DISTRIB_BRANCH}/docs/notifications.md
RUN which wget &> /dev/null || apt-get install --no-install-recommends wget=1.20.3 ; wget --no-verbose --output-document /docs/sources/registry/spec/ https://raw.githubusercontent.com/docker/distribution/${DISTRIB_BRANCH}/docs/spec/api.md
RUN which wget &> /dev/null || apt-get install --no-install-recommends wget=1.20.3 ; wget --no-verbose --output-document /docs/sources/registry/spec/ https://raw.githubusercontent.com/docker/distribution/${DISTRIB_BRANCH}/docs/spec/json.md
RUN which wget &> /dev/null || apt-get install --no-install-recommends wget=1.20.3 ; wget --no-verbose --output-document /docs/sources/registry/storage-drivers/ https://raw.githubusercontent.com/docker/distribution/${DISTRIB_BRANCH}/docs/storage-drivers/s3.md
RUN which wget &> /dev/null || apt-get install --no-install-recommends wget=1.20.3 ; wget --no-verbose --output-document /docs/sources/registry/storage-drivers/ https://raw.githubusercontent.com/docker/distribution/${DISTRIB_BRANCH}/docs/storage-drivers/azure.md
RUN which wget &> /dev/null || apt-get install --no-install-recommends wget=1.20.3 ; wget --no-verbose --output-document /docs/sources/registry/storage-drivers/ https://raw.githubusercontent.com/docker/distribution/${DISTRIB_BRANCH}/docs/storage-drivers/filesystem.md
RUN which wget &> /dev/null || apt-get install --no-install-recommends wget=1.20.3 ; wget --no-verbose --output-document /docs/sources/registry/storage-drivers/ https://raw.githubusercontent.com/docker/distribution/${DISTRIB_BRANCH}/docs/storage-drivers/inmemory.md
RUN which wget &> /dev/null || apt-get install --no-install-recommends wget=1.20.3 ; wget --no-verbose --output-document /docs/sources/registry/spec/auth/token.md https://raw.githubusercontent.com/docker/distribution/${DISTRIB_BRANCH}/docs/spec/auth/token.md
RUN sed -i.old '1s;^;no_version_dropdown: true;' /docs/sources/registry/*.md /docs/sources/registry/spec/*.md /docs/sources/registry/spec/auth/*.md /docs/sources/registry/storage-drivers/*.md
RUN sed -i.old -e '/^<!--GITHUB/g' -e '/^IGNORES-->/g' /docs/sources/registry/*.md /docs/sources/registry/spec/*.md /docs/sources/registry/spec/auth/*.md /docs/sources/registry/storage-drivers/*.md
#  ######################
#   Docker Swarm
#  ######################
#  ADD https://raw.githubusercontent.com/docker/swarm/${SWARM_BRANCH}/docs/mkdocs.yml /docs/mkdocs-swarm.yml
RUN which wget &> /dev/null || apt-get install --no-install-recommends wget=1.20.3 ; wget --no-verbose --output-document /docs/sources/swarm/index.md https://raw.githubusercontent.com/docker/swarm/${SWARM_BRANCH}/docs/index.md
RUN which wget &> /dev/null || apt-get install --no-install-recommends wget=1.20.3 ; wget --no-verbose --output-document /docs/sources/swarm/discovery.md https://raw.githubusercontent.com/docker/swarm/${SWARM_BRANCH}/discovery/README.md
RUN which wget &> /dev/null || apt-get install --no-install-recommends wget=1.20.3 ; wget --no-verbose --output-document /docs/sources/swarm/API.md https://raw.githubusercontent.com/docker/swarm/${SWARM_BRANCH}/api/README.md
RUN which wget &> /dev/null || apt-get install --no-install-recommends wget=1.20.3 ; wget --no-verbose --output-document /docs/sources/swarm/scheduler/filter.md https://raw.githubusercontent.com/docker/swarm/${SWARM_BRANCH}/scheduler/filter/README.md
RUN which wget &> /dev/null || apt-get install --no-install-recommends wget=1.20.3 ; wget --no-verbose --output-document /docs/sources/swarm/scheduler/strategy.md https://raw.githubusercontent.com/docker/swarm/${SWARM_BRANCH}/scheduler/strategy/README.md
RUN sed -i.old '1s;^;no_version_dropdown: true;' /docs/sources/swarm/*.md /docs/sources/swarm/scheduler/*.md
#  ######################
#   Docker Machine
#  ######################
#  ADD https://raw.githubusercontent.com/docker/machine/${MACHINE_BRANCH}/docs/mkdocs.yml /docs/mkdocs-machine.yml
RUN which wget &> /dev/null || apt-get install --no-install-recommends wget=1.20.3 ; wget --no-verbose --output-document /docs/sources/machine/index.md https://raw.githubusercontent.com/docker/machine/${MACHINE_BRANCH}/docs/index.md
RUN sed -i.old '1s;^;no_version_dropdown: true;' /docs/sources/machine/index.md
#  ######################
#   Docker Compose
#  ######################
#  ADD https://raw.githubusercontent.com/docker/compose/${COMPOSE_BRANCH}/docs/mkdocs.yml /docs/mkdocs-compose.yml
RUN which wget &> /dev/null || apt-get install --no-install-recommends wget=1.20.3 ; wget --no-verbose --output-document /docs/sources/compose/ https://raw.githubusercontent.com/docker/compose/${COMPOSE_BRANCH}/docs/index.md
RUN which wget &> /dev/null || apt-get install --no-install-recommends wget=1.20.3 ; wget --no-verbose --output-document /docs/sources/compose/ https://raw.githubusercontent.com/docker/compose/${COMPOSE_BRANCH}/docs/install.md
RUN which wget &> /dev/null || apt-get install --no-install-recommends wget=1.20.3 ; wget --no-verbose --output-document /docs/sources/compose/ https://raw.githubusercontent.com/docker/compose/${COMPOSE_BRANCH}/docs/cli.md
RUN which wget &> /dev/null || apt-get install --no-install-recommends wget=1.20.3 ; wget --no-verbose --output-document /docs/sources/compose/ https://raw.githubusercontent.com/docker/compose/${COMPOSE_BRANCH}/docs/yml.md
RUN which wget &> /dev/null || apt-get install --no-install-recommends wget=1.20.3 ; wget --no-verbose --output-document /docs/sources/compose/ https://raw.githubusercontent.com/docker/compose/${COMPOSE_BRANCH}/docs/env.md
RUN which wget &> /dev/null || apt-get install --no-install-recommends wget=1.20.3 ; wget --no-verbose --output-document /docs/sources/compose/ https://raw.githubusercontent.com/docker/compose/${COMPOSE_BRANCH}/docs/completion.md
RUN which wget &> /dev/null || apt-get install --no-install-recommends wget=1.20.3 ; wget --no-verbose --output-document /docs/sources/compose/ https://raw.githubusercontent.com/docker/compose/${COMPOSE_BRANCH}/docs/django.md
RUN which wget &> /dev/null || apt-get install --no-install-recommends wget=1.20.3 ; wget --no-verbose --output-document /docs/sources/compose/ https://raw.githubusercontent.com/docker/compose/${COMPOSE_BRANCH}/docs/rails.md
RUN which wget &> /dev/null || apt-get install --no-install-recommends wget=1.20.3 ; wget --no-verbose --output-document /docs/sources/compose/ https://raw.githubusercontent.com/docker/compose/${COMPOSE_BRANCH}/docs/wordpress.md
RUN which wget &> /dev/null || apt-get install --no-install-recommends wget=1.20.3 ; wget --no-verbose --output-document /docs/sources/compose/ https://raw.githubusercontent.com/docker/compose/${COMPOSE_BRANCH}/docs/extends.md
RUN which wget &> /dev/null || apt-get install --no-install-recommends wget=1.20.3 ; wget --no-verbose --output-document /docs/sources/compose/ https://raw.githubusercontent.com/docker/compose/${COMPOSE_BRANCH}/docs/production.md
RUN sed -i.old '1s;^;no_version_dropdown: true;' /docs/sources/compose/*.md
#  ######################
#   Kitematic
#  ######################
RUN which wget &> /dev/null || apt-get install --no-install-recommends wget=1.20.3 ; wget --no-verbose --output-document /docs/sources/kitematic/ https://raw.githubusercontent.com/kitematic/kitematic/${KITEMATIC_BRANCH}/docs/faq.md
RUN which wget &> /dev/null || apt-get install --no-install-recommends wget=1.20.3 ; wget --no-verbose --output-document /docs/sources/kitematic/ https://raw.githubusercontent.com/kitematic/kitematic/${KITEMATIC_BRANCH}/docs/index.md
RUN which wget &> /dev/null || apt-get install --no-install-recommends wget=1.20.3 ; wget --no-verbose --output-document /docs/sources/kitematic/ https://raw.githubusercontent.com/kitematic/kitematic/${KITEMATIC_BRANCH}/docs/known-issues.md
RUN which wget &> /dev/null || apt-get install --no-install-recommends wget=1.20.3 ; wget --no-verbose --output-document /docs/sources/kitematic/ https://raw.githubusercontent.com/kitematic/kitematic/${KITEMATIC_BRANCH}/docs/minecraft-server.md
RUN which wget &> /dev/null || apt-get install --no-install-recommends wget=1.20.3 ; wget --no-verbose --output-document /docs/sources/kitematic/ https://raw.githubusercontent.com/kitematic/kitematic/${KITEMATIC_BRANCH}/docs/nginx-web-server.md
RUN which wget &> /dev/null || apt-get install --no-install-recommends wget=1.20.3 ; wget --no-verbose --output-document /docs/sources/kitematic/ https://raw.githubusercontent.com/kitematic/kitematic/${KITEMATIC_BRANCH}/docs/rethinkdb-dev-database.md
RUN which wget &> /dev/null || apt-get install --no-install-recommends wget=1.20.3 ; wget --no-verbose --output-document /docs/sources/kitematic/ https://raw.githubusercontent.com/kitematic/kitematic/${KITEMATIC_BRANCH}/docs/userguide.md
RUN sed -i.old '1s;^;no_version_dropdown: true;' /docs/sources/kitematic/*.md
RUN which wget &> /dev/null || apt-get install --no-install-recommends wget=1.20.3 ; wget --no-verbose --output-document /docs/sources/kitematic/assets/ https://raw.githubusercontent.com/kitematic/kitematic/${KITEMATIC_BRANCH}/docs/assets/browse-images.png
RUN which wget &> /dev/null || apt-get install --no-install-recommends wget=1.20.3 ; wget --no-verbose --output-document /docs/sources/kitematic/assets/ https://raw.githubusercontent.com/kitematic/kitematic/${KITEMATIC_BRANCH}/docs/assets/change-folder.png
RUN which wget &> /dev/null || apt-get install --no-install-recommends wget=1.20.3 ; wget --no-verbose --output-document /docs/sources/kitematic/assets/ https://raw.githubusercontent.com/kitematic/kitematic/${KITEMATIC_BRANCH}/docs/assets/cli-access-button.png
RUN which wget &> /dev/null || apt-get install --no-install-recommends wget=1.20.3 ; wget --no-verbose --output-document /docs/sources/kitematic/assets/ https://raw.githubusercontent.com/kitematic/kitematic/${KITEMATIC_BRANCH}/docs/assets/cli-redis-container.png
RUN which wget &> /dev/null || apt-get install --no-install-recommends wget=1.20.3 ; wget --no-verbose --output-document /docs/sources/kitematic/assets/ https://raw.githubusercontent.com/kitematic/kitematic/${KITEMATIC_BRANCH}/docs/assets/cli-terminal.png
RUN which wget &> /dev/null || apt-get install --no-install-recommends wget=1.20.3 ; wget --no-verbose --output-document /docs/sources/kitematic/assets/ https://raw.githubusercontent.com/kitematic/kitematic/${KITEMATIC_BRANCH}/docs/assets/containers.png
RUN which wget &> /dev/null || apt-get install --no-install-recommends wget=1.20.3 ; wget --no-verbose --output-document /docs/sources/kitematic/assets/ https://raw.githubusercontent.com/kitematic/kitematic/${KITEMATIC_BRANCH}/docs/assets/installing.png
RUN which wget &> /dev/null || apt-get install --no-install-recommends wget=1.20.3 ; wget --no-verbose --output-document /docs/sources/kitematic/assets/ https://raw.githubusercontent.com/kitematic/kitematic/${KITEMATIC_BRANCH}/docs/assets/minecraft-add-server.png
RUN which wget &> /dev/null || apt-get install --no-install-recommends wget=1.20.3 ; wget --no-verbose --output-document /docs/sources/kitematic/assets/ https://raw.githubusercontent.com/kitematic/kitematic/${KITEMATIC_BRANCH}/docs/assets/minecraft-create.png
RUN which wget &> /dev/null || apt-get install --no-install-recommends wget=1.20.3 ; wget --no-verbose --output-document /docs/sources/kitematic/assets/ https://raw.githubusercontent.com/kitematic/kitematic/${KITEMATIC_BRANCH}/docs/assets/minecraft-data-volume.png
RUN which wget &> /dev/null || apt-get install --no-install-recommends wget=1.20.3 ; wget --no-verbose --output-document /docs/sources/kitematic/assets/ https://raw.githubusercontent.com/kitematic/kitematic/${KITEMATIC_BRANCH}/docs/assets/minecraft-login.png
RUN which wget &> /dev/null || apt-get install --no-install-recommends wget=1.20.3 ; wget --no-verbose --output-document /docs/sources/kitematic/assets/ https://raw.githubusercontent.com/kitematic/kitematic/${KITEMATIC_BRANCH}/docs/assets/minecraft-map.png
RUN which wget &> /dev/null || apt-get install --no-install-recommends wget=1.20.3 ; wget --no-verbose --output-document /docs/sources/kitematic/assets/ https://raw.githubusercontent.com/kitematic/kitematic/${KITEMATIC_BRANCH}/docs/assets/minecraft-port.png
RUN which wget &> /dev/null || apt-get install --no-install-recommends wget=1.20.3 ; wget --no-verbose --output-document /docs/sources/kitematic/assets/ https://raw.githubusercontent.com/kitematic/kitematic/${KITEMATIC_BRANCH}/docs/assets/minecraft-restart.png
RUN which wget &> /dev/null || apt-get install --no-install-recommends wget=1.20.3 ; wget --no-verbose --output-document /docs/sources/kitematic/assets/ https://raw.githubusercontent.com/kitematic/kitematic/${KITEMATIC_BRANCH}/docs/assets/minecraft-server-address.png
RUN which wget &> /dev/null || apt-get install --no-install-recommends wget=1.20.3 ; wget --no-verbose --output-document /docs/sources/kitematic/assets/ https://raw.githubusercontent.com/kitematic/kitematic/${KITEMATIC_BRANCH}/docs/assets/nginx-2048-files.png
RUN which wget &> /dev/null || apt-get install --no-install-recommends wget=1.20.3 ; wget --no-verbose --output-document /docs/sources/kitematic/assets/ https://raw.githubusercontent.com/kitematic/kitematic/${KITEMATIC_BRANCH}/docs/assets/nginx-2048.png
RUN which wget &> /dev/null || apt-get install --no-install-recommends wget=1.20.3 ; wget --no-verbose --output-document /docs/sources/kitematic/assets/ https://raw.githubusercontent.com/kitematic/kitematic/${KITEMATIC_BRANCH}/docs/assets/nginx-create.png
RUN which wget &> /dev/null || apt-get install --no-install-recommends wget=1.20.3 ; wget --no-verbose --output-document /docs/sources/kitematic/assets/ https://raw.githubusercontent.com/kitematic/kitematic/${KITEMATIC_BRANCH}/docs/assets/nginx-data-folder.png
RUN which wget &> /dev/null || apt-get install --no-install-recommends wget=1.20.3 ; wget --no-verbose --output-document /docs/sources/kitematic/assets/ https://raw.githubusercontent.com/kitematic/kitematic/${KITEMATIC_BRANCH}/docs/assets/nginx-data-volume.png
RUN which wget &> /dev/null || apt-get install --no-install-recommends wget=1.20.3 ; wget --no-verbose --output-document /docs/sources/kitematic/assets/ https://raw.githubusercontent.com/kitematic/kitematic/${KITEMATIC_BRANCH}/docs/assets/nginx-hello-world.png
RUN which wget &> /dev/null || apt-get install --no-install-recommends wget=1.20.3 ; wget --no-verbose --output-document /docs/sources/kitematic/assets/ https://raw.githubusercontent.com/kitematic/kitematic/${KITEMATIC_BRANCH}/docs/assets/nginx-preview.png
RUN which wget &> /dev/null || apt-get install --no-install-recommends wget=1.20.3 ; wget --no-verbose --output-document /docs/sources/kitematic/assets/ https://raw.githubusercontent.com/kitematic/kitematic/${KITEMATIC_BRANCH}/docs/assets/nginx-serving-2048.png
RUN which wget &> /dev/null || apt-get install --no-install-recommends wget=1.20.3 ; wget --no-verbose --output-document /docs/sources/kitematic/assets/ https://raw.githubusercontent.com/kitematic/kitematic/${KITEMATIC_BRANCH}/docs/assets/rethink-container.png
RUN which wget &> /dev/null || apt-get install --no-install-recommends wget=1.20.3 ; wget --no-verbose --output-document /docs/sources/kitematic/assets/ https://raw.githubusercontent.com/kitematic/kitematic/${KITEMATIC_BRANCH}/docs/assets/rethink-create.png
RUN which wget &> /dev/null || apt-get install --no-install-recommends wget=1.20.3 ; wget --no-verbose --output-document /docs/sources/kitematic/assets/ https://raw.githubusercontent.com/kitematic/kitematic/${KITEMATIC_BRANCH}/docs/assets/rethink-ports.png
RUN which wget &> /dev/null || apt-get install --no-install-recommends wget=1.20.3 ; wget --no-verbose --output-document /docs/sources/kitematic/assets/ https://raw.githubusercontent.com/kitematic/kitematic/${KITEMATIC_BRANCH}/docs/assets/rethinkdb-preview.png
RUN which wget &> /dev/null || apt-get install --no-install-recommends wget=1.20.3 ; wget --no-verbose --output-document /docs/sources/kitematic/assets/ https://raw.githubusercontent.com/kitematic/kitematic/${KITEMATIC_BRANCH}/docs/assets/volumes-dir.png
#   Then build everything together, ready for mkdocs
RUN /docs/build.sh
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
