FROM my-ubuntu:14.04
# 此images的tags为my-nginx
# 安装在/usr/src/nginx 
#  see：https://github.com/docker-library/nginx/blob/master/1.7/Dockerfile
#  原nginx的www：/usr/src/nginx/html
#  docker run --name nginx -p 80:80 --volumes-from share_volume -v /Users/github/docker_lnmp/nginx/conf/:/usr/src/nginx/conf/ --link php-5.5:php -d  my-nginx
MAINTAINER aogg
#  -y为如果提示yes/no，默认yes
RUN apt-get update \
 && apt-get install -y --no-install-recommends libgpg-error0 perl perl-modules xml-core libssl1.0.0 libpcre3 libpcre3-dev libxslt-dev libgd2-dev libgeoip-dev geoip-database libperl-dev \
 && rm -rf /var/lib/apt/lists/*
ARG NGINX_VERSION
ARG NGINX_DIR
ARG NGINX_ARGS
ENV NGINX_VERSION="${NGINX_VERSION:-'1.10.1'}"
#  注意最后不要有斜线/
ENV NGINX_DIR="${NGINX_DIR:-'/usr/src/nginx'}"
ENV NGINX_ARGS="${NGINX_ARGS:-''}"
ENV NGINX_CONF_DIR="$NGINX_DIR/conf"
#  see http://nginx.org/en/pgp_keys.html
#  WARNING: options in `/root/.gnupg/gpg.conf' are not yet active during this run
#  RUN gpg --keyserver pgp.mit.edu --recv-key \
#  	A09CD539B8BB8CBE96E82BDFABD4D3B3F5806B4D \
#  	4C2C85E705DC730833990C38A9376139A524C53E \
#  	B0F4253373F8F6F510D42178520A9993A1C052F8 \
#  	65506C02EFC250F1B7A3D694ECF0E90B2C172083 \
#  	7338973069ED3F443F4D37DFA64FD5B17ADB39A8 \
#  	6E067260B83DCF2CA93C566F518509686C7E5E82 \
#  	573BFD6B3D8FBC641079A6ABABF5BD827BD9BF62
#  All our runtime and build dependencies, in alphabetical order (to ease maintenance)  原：
#  分开，上面可使用缓存
RUN set -x \
 && nginx_configure_dir='/usr/local/nginx' \
 && curl -SL "http://nginx.org/download/nginx-$NGINX_VERSION.tar.gz" -o nginx.tar.gz \
 && mkdir -p $NGINX_DIR $nginx_configure_dir \
 && tar -xvf nginx.tar.gz -C /usr/local/nginx/ --strip-components=1 \
 && rm nginx.tar.gz* \
 && cd $nginx_configure_dir \
 && ./configure --prefix=$NGINX_DIR --conf-path=$NGINX_CONF_DIR/nginx.conf --http-log-path=/proc/self/fd/1 --error-log-path=/proc/self/fd/2 ${NGINX_ARGS} \
 && make -j"$( nproc ;)" \
 && make install
ENV PATH="$NGINX_DIR/sbin:$PATH"
WORKDIR $NGINX_DIR
#  TODO USER www-data
EXPOSE 80/tcp 443/tcp
#  保证nginx运行在共享目录后
VOLUME ["${NGINX_CONF_DIR}"]
#  让nginx在前台运行
ENTRYPOINT ["nginx", "-g", "daemon", "off"]
