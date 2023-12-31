FROM node:8-alpine
ARG NAME=styleguide
ARG DESCRIPTION="Reaction Components Style Guide"
ARG URL=https://github.com/reactioncommerce/reaction-component-library
ARG DOC_URL=https://github.com/reactioncommerce/reaction-component-library
ARG VCS_URL=https://github.com/reactioncommerce/reaction-component-library
ARG VCS_REF
ARG VENDOR
ARG BUILD_DATE
ARG BUILD_COMPARE_URL
ARG BUILD_ENV=test
ARG BUILD_NUMBER
ARG BUILD_PLATFORM
ARG BUILD_PLATFORM_PROJECT_USERNAME
ARG BUILD_PLATFORM_PROJECT_REPONAME
ARG BUILD_PULL_REQUESTS
ARG BUILD_TRIGGERED_BY_TAG
ARG BUILD_URL
ARG CIRCLE_WORKSPACE_ID
ARG CIRCLE_WORKFLOW_ID
ARG CIRCLE_WORKFLOW_JOB_ID
ARG CIRCLE_WORKFLOW_UPSTREAM_JOB_IDS
ARG CIRCLE_WORKSPACE_ID
ARG GIT_REPOSITORY_URL
ARG GIT_SHA1
ARG LICENSE
ENV APP_SOURCE_DIR="/usr/local/src/reaction-app" \
    PATH="$PATH:/usr/local/src/reaction-app/node_modules/.bin"
LABEL maintainer="Reaction Commerce <engineering@reactioncommerce.com>" \
      com.reactioncommerce.build-date="$BUILD_DATE" \
      com.reactioncommerce.name="$NAME" \
      com.reactioncommerce.description="$DESCRIPTION" \
      com.reactioncommerce.url="$URL" \
      com.reactioncommerce.vcs-url="$VCS_URL" \
      com.reactioncommerce.vcs-ref="$VCS_REF" \
      com.reactioncommerce.vendor="$VENDOR" \
      com.reactioncommerce.docker.build.compare-url="$BUILD_COMPARE_URL" \
      com.reactioncommerce.docker.build.number="$BUILD_NUMBER" \
      com.reactioncommerce.docker.build.platform="$BUILD_PLATFORM" \
      com.reactioncommerce.docker.build.platform.project.username="$BUILD_PLATFORM_PROJECT_USERNAME" \
      com.reactioncommerce.docker.build.platform.project.reponame="$BUILD_PLATFORM_PROJECT_REPONAME" \
      com.reactioncommerce.docker.build.pull-requests="$BUILD_PULL_REQUESTS" \
      com.reactioncommerce.docker.build.triggered-by-tag="$BUILD_TRIGGERED_BY_TAG" \
      com.reactioncommerce.docker.build.url="$BUILD_URL" \
      com.reactioncommerce.docker.build.circle.workflow.id="$CIRCLE_WORKFLOW_ID" \
      com.reactioncommerce.docker.build.circle.workflow.job.id="$CIRCLE_WORKFLOW_JOB_ID" \
      com.reactioncommerce.docker.build.circle.workflow.upstream.job.ids="$CIRCLE_WORKFLOW_UPSTREAM_JOB_IDS" \
      com.reactioncommerce.docker.build.circle.workflow.url="https://circleci.com/workflow-run/$CIRCLE_WORKFLOW_ID" \
      com.reactioncommerce.docker.build.circle.workspace.id="$CIRCLE_WORKSPACE_ID" \
      com.reactioncommerce.docker.git.repository.url="$GIT_REPOSITORY_URL" \
      com.reactioncommerce.docker.git.sha1="$GIT_SHA1" \
      com.reactioncommerce.docker.license="$LICENSE"
#   Because Docker Compose uses a volume for node_modules and volumes are owned
#   by root by default, we have to initially create node_modules here with correct owner.
#   Without this Yarn cannot write packages into node_modules later, when running in a container.
RUN mkdir -p "/usr/local/src/reaction-app/node_modules"
RUN mkdir -p "/usr/local/src/reaction-app/package/node_modules"
RUN chown node "/usr/local/src"
RUN chown node "/usr/local/src/reaction-app"
RUN chown node "/usr/local/src/reaction-app/node_modules"
RUN chown node "/usr/local/src/reaction-app/package/node_modules"
WORKDIR $APP_SOURCE_DIR
#   Note that the two node_modules directories will not be copied during
#   this due to being listed in the `.dockerignore` file.
COPY --chown=node . $APP_SOURCE_DIR
#   Build the dependencies into the Docker image in a cacheable way. Dependencies
#   are only rebuilt when package.json or yarn.lock is modified.
#
#   The project directory will be mounted during development. Therefore, we'll
#   install dependencies into an external directory (one level up.) This works
#   because Node traverses up the fs to find node_modules.
RUN set -ex ; if [ "$BUILD_ENV" = "production" ] ; then yarn install --frozen-lockfile --ignore-scripts --no-cache ;cd package \
 && yarn install --frozen-lockfile --ignore-scripts --no-cache ; elif [ "$BUILD_ENV" = "test" ] ; then yarn install --frozen-lockfile --ignore-scripts --no-cache ;cd package \
 && yarn install --frozen-lockfile --ignore-scripts --no-cache ; fi
#   For development, we will yarn install on each container start.
#   This ensures that we use our Docker development .yarnrc config
#   and get any add/changed dependencies without needing a full rebuild.
#
#   We are copying in a .yarnrc file that is specific to running within Docker.
#   Thus, we don't want it in the main repo because it breaks yarn on the host
#   machine. We also don't want it in APP_SOURCE_DIR where docker-compose will
#   link in host files, so we create it as the user-level config.
#   Note that this will be copied in for a prod build, too, but since
#   we already ran yarn install above, it doesn't matter.
COPY --chown=node ./.reaction/yarnrc-docker.template /home/node/.yarnrc
#   Important: Make sure we're the "node" user before we begin doing things because
#   our tools use "/home/node" as the HOME dir.
USER node
RUN yarn run build
CMD ["yarn", "start"]
HEALTHCHECK CMD curl --fail http://127.0.0.1:3000 || exit 1
