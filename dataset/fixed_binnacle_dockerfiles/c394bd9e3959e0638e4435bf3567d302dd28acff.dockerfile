FROM debian:jessie
MAINTAINER MacArthur Lab
#   install commmon utilities
RUN apt-get update \
 && apt-get install --no-install-recommends apt-utils bzip2 curl emacs git htop less nano wget xterm -y --fix-missing
#   install python
RUN apt-get install --no-install-recommends python-dev -y \
 && wget https://bootstrap.pypa.io/get-pip.py \
 && python get-pip.py \
 && pip install pip==23.1 setuptools==67.6.1 --upgrade
#   install dependencies of the HaploPainter.pl script used to generate static pedigree images
RUN apt-get update \
 && apt-get install --no-install-recommends perl build-essential libcairo2-dev libglib2.0-bin libglib2.0-0 libgtk2.0-dev libpango1.0-dev -y
RUN wget https://raw.github.com/miyagawa/cpanminus/master/cpanm -O /usr/bin/cpanm \
 && chmod +x /usr/bin/cpanm \
 && cpanm --notest Cairo DBI Gtk2 Tk Sort::Naturally
#   install dev dependencies for react, javascript development. These are not needed at runtime.
RUN apt-get update \
 && curl -sL https://deb.nodesource.com/setup_8.x | bash - \
 && apt-get install --no-install-recommends nodejs -y
#   install database clients for debugging
RUN apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv 0C49F3730359A14518585931BC711F9BA15703C6 \
 && echo "deb http://repo.mongodb.org/apt/debian jessie/mongodb-org/3.4 main" | tee /etc/apt/sources.list.d/mongodb-org-3.4.list \
 && apt-get update \
 && apt-get install --no-install-recommends mongodb-org-tools mongodb-org-shell -y
RUN echo 'deb http://apt.postgresql.org/pub/repos/apt/ jessie-pgdg main' >> /etc/apt/sources.list.d/postgresql.list \
 && wget --no-check-certificate -q https://www.postgresql.org/media/keys/ACCC4CF8.asc -O- | apt-key add - \
 && apt-get update \
 && apt-get install --no-install-recommends postgresql postgresql-client -y
RUN apt-get update \
 && apt-get install --no-install-recommends redis-tools -y
#   install gcloud tools
RUN CLOUDSDK_CORE_DISABLE_PROMPTS=1 \
 && curl https://sdk.cloud.google.com | bash \
 && apt-get update \
 && apt-get install --no-install-recommends gcc python-dev python-setuptools libffi-dev libssl-dev -y \
 && pip install gsutil==5.23
RUN CLOUDSDK_CORE_DISABLE_PROMPTS=1 \
 && CLOUD_SDK_REPO="cloud-sdk-$( lsb_release -c -s ;)" \
 && echo "deb http://packages.cloud.google.com/apt $CLOUD_SDK_REPO main" > /etc/apt/sources.list.d/google-cloud-sdk.list \
 && curl https://packages.cloud.google.com/apt/doc/apt-key.gpg | apt-key add - \
 && apt-get update \
 && apt-get install --no-install-recommends google-cloud-sdk -y
RUN curl -LO https://storage.googleapis.com/kubernetes-release/release/$( curl -s https://storage.googleapis.com/kubernetes-release/release/stable.txt ;)/bin/linux/amd64/kubectl \
 && chmod +x ./kubectl \
 && mv ./kubectl /usr/local/bin/kubectl
#   DISABLE_CACHE work-around to force git pull on every docker build, based on https://github.com/docker/docker/issues/1996
ARG DISABLE_CACHE=1
#   update seqr repo
RUN git clone -q https://github.com/macarthur-lab/seqr
WORKDIR /seqr
#   install seqr dependencies
RUN pip install -r requirements.txt
ARG SEQR_SERVICE_PORT
ENV SEQR_SERVICE_PORT="$SEQR_SERVICE_PORT"
EXPOSE $SEQR_SERVICE_PORT
ENV PYTHONPATH="/seqr:/seqr_settings"
ENV TERM="xterm"
RUN mkdir /seqr_settings
COPY readiness_probe /
COPY bin/*.sh /usr/local/bin/
COPY gitconfig /root/.gitconfig
COPY config/*.py /seqr_settings/
COPY bashrc /root/.bashrc
COPY entrypoint.sh /
WORKDIR /seqr
CMD ["/entrypoint.sh"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
