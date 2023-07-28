FROM debian:9.9-slim
#  Default google-fluentd to the latest stable.
#  Use `docker build --build-arg REPO_SUFFIX=<CUSTOM_REPO_SUFFIX>` to override
#  the version.
ARG REPO_SUFFIX=20190525-1
COPY entrypoint.sh Dockerfile /
#  TODO: This may be a moving target, figure out how to pin.
RUN apt-get update \
 && apt-get install --no-install-recommends curl gnupg2 lsb-release build-essential procps systemd ca-certificates adduser -y \
 && curl -sS https://dl.google.com/cloudagents/install-logging-agent.sh | REPO_SUFFIX="$REPO_SUFFIX" DO_NOT_INSTALL_CATCH_ALL_CONFIG=true bash \
 && dpkg -s google-fluentd | sed -nE 's/^Version: (.*)(-[^-]+)$/VERSION=\1\nGOOGLE_FLUENTD_VERSION=\1\2/p' >> /VERSION \
 && echo REPO_SUFFIX=$REPO_SUFFIX >> /VERSION \
 && /opt/google-fluentd/embedded/bin/gem list fluent-plugin-google-cloud | sed 's/^fluent-plugin-google-cloud (\(.*\))$/FLUENT_PLUGIN_GOOGLE_CLOUD_VERSION=\1/' >> /VERSION \
 && apt-get purge -y curl lsb-release build-essential gtk2.0 libkrb5-3 \
 && apt-get autoremove -y --purge \
 && rm -rf /usr/share/doc /usr/share/man /opt/google-fluentd/embedded/lib/ruby/gems/*/doc /opt/google-fluentd/embedded/share/doc /opt/google-fluentd/embedded/share/gtk-doc /opt/google-fluentd/embedded/share/man /opt/google-fluentd/embedded/share/terminfo \
 && /opt/google-fluentd/embedded/bin/gem uninstall -ax --force td td-client td-logger fluent-plugin-td fluent-plugin-td-monitoring hirb parallel ohai mixlib-cli mixlib-config mixlib-log mixlib-shellout systemu fluent-plugin-mongo mongo \
 && /opt/google-fluentd/embedded/bin/gem uninstall httpclient -v 2.7.2 \
 && rm -rf /var/cache /usr/bin/apt-* /var/lib/apt/lists/* /usr/bin/dpkg* /var/lib/dpkg /tmp/* /usr/lib/x86_64-linux-gnu/libldap* /usr/lib/x86_64-linux-gnu/gconv/IBM* /usr/lib/x86_64-linux-gnu/gconv/EBCDIC* /opt/google-fluentd/embedded/bin/ecpg /opt/google-fluentd/embedded/bin/openssl /usr/bin/openssl /opt/google-fluentd/embedded/bin/pg_* /opt/google-fluentd/embedded/bin/postgre* /opt/google-fluentd/embedded/share/postgre* /opt/google-fluentd/embedded/lib/postgre* /opt/google-fluentd/embedded/bin/psql /opt/google-fluentd/embedded/share/libtool /opt/google-fluentd/embedded/bin/libtool /opt/google-fluentd/embedded/include /opt/google-fluentd/embedded/lib/*.a \
 && find /var/log -name "*.log" -type f -delete \
 && (find /opt/google-fluentd/embedded/ -name '*.c' -o -name '*.cc' -o -name '*.h' | xargs rm ) \
 && (find /opt/google-fluentd/embedded/lib/ruby/gems/*/gems/google-api-client-*/generated/google/apis -mindepth 1 -maxdepth 1 ! -name 'logging*' | xargs rm -rf ) \
 && sed -i "s/num_threads 8/num_threads 8\n detect_json true\n # Enable metadata agent lookups.\n enable_metadata_agent true\n metadata_agent_url \"http:\/\/local-metadata-agent.stackdriver.com:8000\"/" "/etc/google-fluentd/google-fluentd.conf"
ENV LD_PRELOAD="/opt/google-fluentd/embedded/lib/libjemalloc.so"
ENTRYPOINT ["/entrypoint.sh"]
CMD ["/usr/sbin/google-fluentd"]
