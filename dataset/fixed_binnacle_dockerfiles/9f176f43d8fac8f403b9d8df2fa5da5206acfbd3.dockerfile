FROM nginx
RUN apt-get update \
 && apt-get install --no-install-recommends perl=5.32.1-4+deb11u2 cpanminus=1.7044-2 libaspell-dev=0.60.8-3 make=4.3-4.1 libdbd-mysql-perl=4.050-3+b1 libdigest-perl-md5-perl=1.9-1.1 libxml-simple-perl=2.25-1 libmodule-install-perl=1.19-1 gcc=4:10.2.1-1 libperl-dev=5.32.1-4+deb11u2 libmysql++-dev=3.2.5-2 libpng-dev=1.6.37-3 build-essential=12.9 libgd-dev=2.3.0-2 mariadb-client=1:10.5.18-0+deb11u1 -y
RUN cpanm --notest --force Algorithm::Permute App::Cmd Archive::Any Archive::Tar Archive::Zip Business::OnlinePayment Business::OnlinePayment::AuthorizeNet Business::PayPal::API Business::Tax::VAT::Validation CHI CSS::Minifier::XS CSS::Packer Cache::FastMmap Capture::Tiny Class::C3 Class::InsideOut Clone Color::Calc Compress::Zlib Config::JSON DBD::mysql DBI Data::ICal DateTime DateTime::Event::ICal DateTime::Format::HTTP DateTime::Format::Mail DateTime::Format::Strptime Devel::StackTrace Devel::StackTrace::WithLexicals Digest::MD5 Digest::SHA Email::Valid Exception::Class Facebook::Graph File::Path Finance::Quote GD GD::Graph Geo::Coder::Googlev3 HTML::Form HTML::Highlight HTML::Packer HTML::Parser HTML::TagCloud HTML::TagFilter HTML::Template HTML::Template::Expr HTTP::BrowserDetect HTTP::Exception HTTP::Headers HTTP::Request IO::File::WithPath IO::Interactive::Tiny IO::Socket::SSL IO::Zlib Image::ExifTool Imager Imager::File::PNG JSON JSON::Any JSON::PP JavaScript::Minifier::XS JavaScript::Packer Kwargs LWP LWP::Protocol::https List::MoreUtils Locales Log::Log4perl MIME::Tools Module::Find Monkey::Patch Moose MooseX::NonMoose MooseX::Storage MooseX::Storage::Format::JSON Net::CIDR::Lite Net::DNS Net::LDAP Net::POP3 Net::SMTP Net::Twitter Number::Format POE POE::Component::Client::HTTP POE::Component::IKC::Server POE::Component::IKC Package::Stash Params::Validate Path::Class PerlIO::eol Plack Plack::Middleware::Debug Plack::Middleware::Status Plack::Request Plack::Response Pod::Coverage Readonly Scope::Guard Search::QueryParser Storable Template Test::Class Test::Deep Test::Differences Test::Exception Test::Harness Test::Log::Dispatch Test::LongString Test::MockObject Test::MockTime Test::More Test::Tester Test::WWW::Mechanize::PSGI Text::Aspell Text::Balanced Text::CSV_XS Tie::CPHash Tie::IxHash Time::HiRes Try::Tiny URI::Escape UUID::Tiny Weather::Com::Finder XML::FeedPP XML::FeedPP::MediaRSS XML::Simple common::sense namespace::autoclean
COPY lib /WebGUI/lib
COPY sbin /WebGUI/sbin
COPY share /WebGUI/share
COPY www/extras /WebGUI/www/extras
COPY www/maintenance.html /WebGUI/www/maintenance.html
COPY www/uploads /WebGUI/uploads
COPY etc /WebGUI/etc
COPY app.psgi /WebGUI/app.psgi
COPY share/entrypoint /entrypoint
COPY share/nginx-main /etc/nginx/nginx.conf
COPY share/nginx-default-server /etc/nginx/conf.d/default.conf
COPY share/upstream-allium.conf /etc/nginx/streams.d/upstream-allium.conf
RUN useradd --home=/WebGUI webgui ; chown -R webgui: /WebGUI ; chmod 755 /entrypoint ; apt-get remove -y cpanminus make gcc libperl-dev ; rm -rf /root/.cpanm ; chown webgui /var/cache/nginx
USER webgui
WORKDIR /WebGUI
EXPOSE 8080/tcp
CMD ["/entrypoint"]
HEALTHCHECK CMD curl --fail http://127.0.0.1:80 || exit 1
