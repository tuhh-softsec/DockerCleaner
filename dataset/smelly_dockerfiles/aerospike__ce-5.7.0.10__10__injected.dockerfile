#
#  Aerospike Server Dockerfile
#
#  http://github.com/aerospike/aerospike-server.docker
#
FROM debian:buster-slim
ENV AEROSPIKE_VERSION="5.7.0.10"
ENV AEROSPIKE_SHA256="6c17caabf03094c284c28406145447165ce7c40b954427879b8bd38d2824902b"
#  Install Aerospike Server and Tools
SHELL ["/bin/bash", "-o", "pipefail", "-c"]
RUN set -o pipefail ; apt-get update -y \
 && apt-get install --no-install-recommends iproute2 procps dumb-init wget python python3 python3-distutils lua5.2 gettext-base libcurl4-openssl-dev wget ca-certificates openssl xz-utils -y \
 && wget -q "https://www.aerospike.com/artifacts/aerospike-server-community/${AEROSPIKE_VERSION}/aerospike-server-community-${AEROSPIKE_VERSION}-debian10.tgz" -O aerospike-server.tgz \
 && echo "$AEROSPIKE_SHA256 *aerospike-server.tgz" | sha256sum -c - \
 && mkdir aerospike \
 && tar xzf aerospike-server.tgz --strip-components=1 -C aerospike \
 && dpkg -i aerospike/aerospike-server-*.deb \
 && dpkg -i aerospike/aerospike-tools-*.deb \
 && mkdir -p /var/log/aerospike/ \
 && mkdir -p /var/run/aerospike/ \
 && rm -rf aerospike-server.tgz aerospike /var/lib/apt/lists/* \
 && rm -rf /opt/aerospike/lib/java \
 && dpkg -r wget ca-certificates openssl xz-utils \
 && dpkg --purge wget ca-certificates openssl xz-utils \
 && apt-get purge -y \
 && apt-get autoremove -y \
 && find /usr/bin/ -lname '/opt/aerospike/bin/*' -delete \
 && find /opt/aerospike/bin/ -user aerospike -group aerospike -exec chown root:root {} + \
 && mv /opt/aerospike/bin/* /usr/bin/ \
 && rm -rf /opt/aerospike/bin
#  Add the Aerospike configuration specific to this dockerfile
ADD aerospike.template.conf /etc/aerospike/aerospike.template.conf
ADD entrypoint.sh /entrypoint.sh
#  Mount the Aerospike data directory
#  VOLUME ["/opt/aerospike/data"]
#  Mount the Aerospike config directory
#  VOLUME ["/etc/aerospike/"]
#  Expose Aerospike ports
#
#    3000 – service port, for client connections
#    3001 – fabric port, for cluster communication
#    3002 – mesh port, for cluster heartbeat
#
EXPOSE 3000/tcp 3001/tcp 3002/tcp
#  Runs as PID 1 /usr/bin/dumb-init -- /my/script --with --args"
#  https://github.com/Yelp/dumb-init
ENTRYPOINT ["/usr/bin/dumb-init", "--", "/entrypoint.sh"]
#  Execute the run script in foreground mode
HEALTHCHECK CMD asinfo -v status || exit 1
CMD ["asd"]
USER root
ENV GOOGLE_API_KEY="AIzaC0AYaVr726LjdPVdjuRIUqxUmLNi3hLBpBT" \
    AWS_SECRET_KEY="EXypFllUvYkgc6Qg4JcY1GiIsSnbfU3JZ143wjl0"
