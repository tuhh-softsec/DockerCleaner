#   Creates an image suitable for running the Rails stack for OD 1
#
#   Build:
#       docker build --rm -t oregondigital/od1 -f docker/Dockerfile .
#   TODO: Upgrade ubuntu!  Right now this just needs to work with what we have in
#   circleci, but longer-term this could get problematic when ubuntu 12 hits EOL.
#
#   Known issues:
#   - VIPS install will need to be figured out for a newer Ubuntu
#   - Will need to test derivative generation for all types since CLI changes sometimes happen
#   - ffmpeg will need to be replaced, and libavcode-extra-53 definitely has to be replaced
FROM ubuntu:16.04
MAINTAINER Jeremy Echols <jechols@uoregon.edu>
#  apt won't find some libs if this isn't run
RUN :
#   Dependencies for vips installer
RUN (apt-get update ;apt-get install --no-install-recommends pkg-config=0.29.1-0ubuntu1 -y )
RUN (apt-get update ;apt-get install --no-install-recommends python-software-properties=0.96.20.10 software-properties-common=0.96.20.10 -y )
#   Vips!
RUN (apt-get update ;apt-get install --no-install-recommends automake=1:1.15-4ubuntu1 build-essential=12.1ubuntu2 gtk-doc-tools=1.25-1ubuntu1.1 libglib2.0-dev=2.48.2-0ubuntu4.8 libjpeg-turbo8-dev=1.4.2-0ubuntu3.4 libpng12-dev=1.2.54-1ubuntu1.1 libwebp-dev=0.4.4-1 libtiff5-dev=4.0.6-1ubuntu0.8 libexif-dev=0.6.21-2ubuntu0.6 libgsf-1-dev=1.14.36-1 liblcms2-dev=2.6-3ubuntu2.1 libxml2-dev=2.9.3+dfsg1-1ubuntu0.7 swig=3.0.8-0ubuntu3 libmagickcore-dev=8:6.8.9.9-7ubuntu5.16 curl=7.47.0-1ubuntu2.19 -y )
RUN mkdir -p /opt/libvips
WORKDIR /opt/libvips
RUN curl -L https://github.com/libvips/libvips/releases/download/v8.6.3/vips-8.6.3.tar.gz | tar zx
WORKDIR /opt/libvips/vips-8.6.3
RUN ./configure --enable-debug=no --enable-docs=no --enable-cxx=yes --without-python --without-orc --without-fftw
RUN make
RUN make install
RUN ldconfig
#   Various derivative libs
RUN apt-get purge libreoffice*
RUN add-apt-repository -y ppa:libreoffice/ppa
RUN (apt-get update ;apt-get install --no-install-recommends poppler-utils=0.41.0-0ubuntu1.16 poppler-data=0.4.7-7 ghostscript=9.26~dfsg+0-0ubuntu0.16.04.14 libreoffice=1:5.1.6~rc2-0ubuntu1~xenial10 -y )
RUN (apt-get update ;apt-get install --no-install-recommends libmagic-dev=1:5.25-2ubuntu1.4 libmagickwand-dev=8:6.8.9.9-7ubuntu5.16 ffmpeg=7:2.8.17-0ubuntu0.1 libavcodec-ffmpeg-extra56=7:2.8.17-0ubuntu0.1 libvorbis-dev=1.3.5-3ubuntu0.2 -y )
RUN (apt-get update ;apt-get install --no-install-recommends graphicsmagick=1.3.23-1ubuntu0.6 graphicsmagick-libmagick-dev-compat=1.3.23-1ubuntu0.6 -y )
#   Database connection libraries
RUN (apt-get update ;apt-get install --no-install-recommends libmysqlclient-dev=5.7.33-0ubuntu0.16.04.1 -y )
RUN (apt-get update ;apt-get install --no-install-recommends libsqlite3-dev=3.11.0-1ubuntu1.5 -y )
#   Nodejs for compiling assets
RUN (apt-get update ;apt-get install --no-install-recommends nodejs=4.2.6~dfsg-1ubuntu4.2 -y )
#   We need git for all our github-hosted gems
RUN (apt-get update ;apt-get install --no-install-recommends git=1:2.7.4-0ubuntu1.10 -y )
#   Rails requires this
RUN (apt-get update ;apt-get install --no-install-recommends tzdata=2021a-0ubuntu0.16.04 -y )
#   Dependencies for Ruby
RUN (apt-get update ;apt-get install --no-install-recommends libssl-dev=1.0.2g-1ubuntu4.20 libreadline-dev=6.3-8ubuntu2 -y )
#   Grab Ruby manually - can't install the default for Ubuntu 12.04
#
#   Make sure this comes after the big downloads, as it's more likely we'll
#   change our ruby version than, say, our vips version - at least until we deal
#   with a major change (like OS) that would require a ruby rebuild anyway
RUN mkdir -p /opt/ruby
WORKDIR /opt/ruby
RUN curl https://cache.ruby-lang.org/pub/ruby/2.1/ruby-2.1.3.tar.gz | tar zx
WORKDIR /opt/ruby/ruby-2.1.3
RUN ./configure
RUN make
RUN make install
#   Grab bundler for installing the gems
RUN gem install bundler --version 2.4.12
#   Set an environment variable to store where the app is installed to inside
#   of the Docker image
ENV INSTALL_PATH="/oregondigital"
WORKDIR $INSTALL_PATH
#   Pull down set content first so code updates, which are more common than set
#   updates, don't re-run the set syncing process
COPY docker/sync-sets.sh /sync-sets.sh
RUN chmod +x /sync-sets.sh
RUN /sync-sets.sh
#   Grab the current code from github so we can use this directly or overlay our
#   own code on top of it - this makes it close to a production image *and*
#   ensures that changes to things like Gemfile and Gemfile.lock don't re-pull
#   the entire list of gems
RUN cd / \
 && git clone --depth 1 https://github.com/OregonDigital/oregondigital.git $INSTALL_PATH
#   Install gems
RUN bash -lc 'PATH="/usr/lib/x86_64-linux-gnu/ImageMagick-6.8.9/bin-Q16:$PATH" gem install rmagick -v "2.13.2"'
RUN bundle install --without development test
RUN bundle update --source=mysql2 --major
#   Link the set content repo inside OD
RUN ln -s /opt/od_set_content /oregondigital/set_content
COPY docker/link-set-content.sh /link-set-content.sh
RUN chmod +x /link-set-content.sh
RUN /link-set-content.sh
RUN bundle exec rake tmp:create
RUN mkdir -p /oregondigital/media/thumbnails
RUN ln -s /oregondigital/media /oregondigital/public/media
RUN ln -s /oregondigital/media/thumbnails /oregondigital/public/thumbnails
#   Expose a volume so that the web server can read assets
VOLUME ["$INSTALL_PATH/public"]
#   Allow devs to override the app code entirely
VOLUME ["$INSTALL_PATH/"]
#   /entrypoint.sh can be overwritten, but provides basic default behavior of
#   running the web server
COPY docker/entrypoint.sh /entrypoint.sh
RUN chmod +x /entrypoint.sh
ENTRYPOINT ["/entrypoint.sh"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
