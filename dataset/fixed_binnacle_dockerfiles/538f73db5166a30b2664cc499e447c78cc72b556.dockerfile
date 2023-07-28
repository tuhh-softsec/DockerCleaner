#   Development Application
FROM wunsh/alpine-elixir-elm AS app
MAINTAINER Docdog Developers <dev@docdog.io>
ARG BUILD_ENV=prod
ARG PORT=4000
ENV APP_NAME="docdog" \
    MIX_ENV="$BUILD_ENV" \
    PORT="$PORT" \
    MIX_HOME="/opt/mix" \
    HEX_HOME="/opt/hex" \
    HOME="/opt/app"
EXPOSE ${PORT}
RUN echo "Build Docker image for Mix env: ${MIX_ENV}." \
 && mkdir -p ${HOME} \
 && chmod -R 777 ${HOME} \
 && apk update \
 && apk add g++=12.2.1_git20220924-r4 inotify-tools=3.22.6.0-r0 make=4.3-r1 python yarn=1.22.19-r0 --no-cache --update \
 && update-ca-certificates --fresh \
 && rm -rf /var/cache/apk/* \
 && mix local.hex --force \
 && mix local.rebar --force
#   Cache Mix dependencies
WORKDIR ${HOME}
COPY mix.exs mix.lock ./
RUN mix do deps.get --only=${MIX_ENV}, deps.compile
#   Cache Yarn dependencies
WORKDIR assets
COPY assets/package.json assets/yarn.lock ./
RUN yarn install
#   Cache Elm dependencies
WORKDIR elm
COPY assets/elm/elm-package.json ./
RUN elm package install -y
WORKDIR ${HOME}
COPY . .
WORKDIR assets/elm
RUN elm-make Main.elm --output=../js/main.js
WORKDIR assets
RUN if [ ${MIX_ENV} = "prod" ] ; then yarn run deploy ; else yarn run build ; fi
WORKDIR ${HOME}
RUN mix do compile, phx.digest
RUN if [ ${MIX_ENV} = "prod" ] ; then mix release --env=${MIX_ENV} ;RELEASE_DIR=`ls -d _build/${MIX_ENV}/rel/${APP_NAME}/releases/*/ ` \
 && mkdir /release \
 && tar -xf "${RELEASE_DIR}${APP_NAME}.tar.gz" -C /release ; fi
CMD ["mix", "phx.server"]
#   Release Application
FROM bitwalker/alpine-erlang:21.0.3
MAINTAINER Docdog Developers <dev@docdog.io>
ARG BUILD_ENV=prod
ARG PORT=4000
ENV APP_NAME="docdog" \
    HOME="/opt/app" \
    MIX_ENV="$BUILD_ENV" \
    PORT="$PORT" \
    REPLACE_OS_VARS="true"
EXPOSE ${PORT}
COPY --from=app /release/ .
RUN apk update \
 && apk add inotify-tools=3.22.6.0-r0
ENTRYPOINT ["/opt/app/bin/docdog"]
CMD ["foreground"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
