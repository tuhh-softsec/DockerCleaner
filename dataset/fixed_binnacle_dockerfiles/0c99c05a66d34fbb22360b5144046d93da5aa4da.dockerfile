ARG BUILD_FROM
FROM $BUILD_FROM
LABEL maintainer="Michael Hansen <hansen.mike@gmail.com>"
ARG BUILD_ARCH
ARG CPU_ARCH
ENV LANG="C.UTF-8"
ARG MAKE_THREADS=4
WORKDIR /
COPY etc/qemu-arm-static /usr/bin/
COPY etc/qemu-aarch64-static /usr/bin/
RUN apt-get update \
 && apt-get install --no-install-recommends bash=5.2.15-2ubuntu1 jq=1.6-2.1ubuntu3 unzip=6.0-27ubuntu1 python3=3.11.2-1 python3-pip=23.0.1+dfsg-1 python3-dev=3.11.2-1 build-essential=12.9ubuntu3 portaudio19-dev=19.6.0-1.2 swig=4.1.0-0.2 libfst-dev=1.7.9-5 libfst-tools=1.7.9-5 libatlas-base-dev=3.10.3-13ubuntu1 sox=14.4.2+git20190427-3.4ubuntu1 espeak=1.48.15+dfsg-3 flite=2.2-5 alsa-utils=1.2.8-1ubuntu1 git=1:2.39.2-1ubuntu1 curl=7.88.1-7ubuntu1 autoconf=2.71-3 libtool=2.4.7-5 automake=1:1.16.5-1.3 bison=2:3.8.2+dfsg-1build1 sphinxbase-utils=0.8+5prealpha+1-15build1 sphinxtrain=1.0.8+5prealpha+1-5 -y
COPY download/phonetisaurus-2019_${BUILD_ARCH}.deb /phonetisaurus.deb
RUN dpkg -i /phonetisaurus.deb \
 && rm /phonetisaurus.deb
#   Install Opengrm
COPY download/opengrm-ngram-1.3.3.tar.gz /
RUN cd / \
 && tar -xf opengrm-ngram-1.3.3.tar.gz \
 && cd opengrm-ngram-1.3.3 \
 && ./configure \
 && make -j $MAKE_THREADS \
 && make install \
 && ldconfig \
 && rm -rf /opengrm*
#   Install Python dependencies
RUN python3 -m pip install --no-cache-dir wheel
COPY download/jsgf2fst-0.1.0.tar.gz /download/
RUN apt-get install --no-install-recommends libfreetype6-dev=2.12.1+dfsg-4 libpng-dev=1.6.39-2 pkg-config=1.8.1-1ubuntu2 libffi-dev=3.4.4-1 libssl-dev=3.0.8-1ubuntu1 -y
COPY requirements.txt /requirements.txt
RUN if [ "$BUILD_ARCH" != "amd64" ] ; then grep -v flair /requirements.txt > /requirements-noflair.txt;mv /requirements-noflair.txt /requirements.txt ; fi
RUN python3 -m pip install --no-cache-dir -r /requirements.txt
#   Install Pocketsphinx Python module with no sound
COPY download/pocketsphinx-python.tar.gz /
RUN python3 -m pip install --no-cache-dir /pocketsphinx-python.tar.gz \
 && rm -rf /pocketsphinx-python*
#   Install snowboy
COPY download/snowboy-1.3.0.tar.gz /
RUN if [ "$BUILD_ARCH" != "aarch64" ] ; then pip3 install --no-cache-dir /snowboy-1.3.0.tar.gz ; fi
#   Install Mycroft Precise
COPY download/precise-engine_0.3.0_${CPU_ARCH}.tar.gz /precise-engine.tar.gz
RUN if [ "$BUILD_ARCH" != "aarch64" ] ; then cd / \
 && tar -xzf /precise-engine.tar.gz \
 && ln -s /precise-engine/precise-engine /usr/bin/precise-engine \
 && rm /precise-engine.tar.gz ; fi
RUN apt-get install --no-install-recommends flite=2.2-5 libttspico-utils=1.0+git20130326-13 -y
COPY download/kaldi_${BUILD_ARCH}.tar.gz /kaldi.tar.gz
RUN mkdir -p /opt \
 && tar -C /opt -xzf /kaldi.tar.gz \
 && rm /kaldi.tar.gz
RUN ldconfig
#   Copy bw and mllr_solve to /usr/bin
RUN find / -name bw -exec cp '{}' /usr/bin/
RUN find / -name mllr_solve -exec cp '{}' /usr/bin/
ENV RHASSPY_APP="/usr/share/rhasspy"
#   Copy script to run
COPY docker/run.sh /run.sh
RUN chmod +x /run.sh
COPY profiles/zh/profile.json profiles/zh/custom_words.txt profiles/zh/espeak_phonemes.txt profiles/zh/phoneme_examples.txt profiles/zh/frequent_words.txt profiles/zh/sentences.ini profiles/zh/stop_words.txt ${RHASSPY_APP}/profiles/zh/
COPY profiles/hi/ profiles/hi/profile.json profiles/hi/custom_words.txt profiles/hi/espeak_phonemes.txt profiles/hi/phoneme_examples.txt profiles/hi/frequent_words.txt profiles/hi/sentences.ini profiles/hi/stop_words.txt ${RHASSPY_APP}/profiles/hi/
COPY profiles/el/profile.json profiles/el/custom_words.txt profiles/el/espeak_phonemes.txt profiles/el/phoneme_examples.txt profiles/el/frequent_words.txt profiles/el/sentences.ini profiles/el/stop_words.txt ${RHASSPY_APP}/profiles/el/
COPY profiles/de/profile.json profiles/de/custom_words.txt profiles/de/espeak_phonemes.txt profiles/de/phoneme_examples.txt profiles/de/frequent_words.txt profiles/de/sentences.ini profiles/de/stop_words.txt ${RHASSPY_APP}/profiles/de/
COPY profiles/it/profile.json profiles/it/custom_words.txt profiles/it/espeak_phonemes.txt profiles/it/phoneme_examples.txt profiles/it/frequent_words.txt profiles/it/sentences.ini profiles/it/stop_words.txt ${RHASSPY_APP}/profiles/it/
COPY profiles/es/profile.json profiles/es/custom_words.txt profiles/es/espeak_phonemes.txt profiles/es/phoneme_examples.txt profiles/es/frequent_words.txt profiles/es/sentences.ini profiles/es/stop_words.txt ${RHASSPY_APP}/profiles/es/
COPY profiles/fr/profile.json profiles/fr/custom_words.txt profiles/fr/espeak_phonemes.txt profiles/fr/phoneme_examples.txt profiles/fr/frequent_words.txt profiles/fr/sentences.ini profiles/fr/stop_words.txt ${RHASSPY_APP}/profiles/fr/
COPY profiles/ru/profile.json profiles/ru/custom_words.txt profiles/ru/espeak_phonemes.txt profiles/ru/phoneme_examples.txt profiles/ru/frequent_words.txt profiles/ru/sentences.ini profiles/ru/stop_words.txt ${RHASSPY_APP}/profiles/ru/
COPY profiles/nl/profile.json profiles/nl/custom_words.txt profiles/nl/espeak_phonemes.txt profiles/nl/phoneme_examples.txt profiles/nl/frequent_words.txt profiles/nl/sentences.ini profiles/nl/stop_words.txt ${RHASSPY_APP}/profiles/nl/
COPY profiles/vi/profile.json profiles/vi/custom_words.txt profiles/vi/espeak_phonemes.txt profiles/vi/phoneme_examples.txt profiles/vi/frequent_words.txt profiles/vi/sentences.ini profiles/vi/stop_words.txt ${RHASSPY_APP}/profiles/vi/
COPY profiles/pt/profile.json profiles/pt/custom_words.txt profiles/pt/espeak_phonemes.txt profiles/pt/phoneme_examples.txt profiles/pt/frequent_words.txt profiles/pt/sentences.ini profiles/pt/stop_words.txt ${RHASSPY_APP}/profiles/pt/
COPY profiles/en/profile.json profiles/en/custom_words.txt profiles/en/espeak_phonemes.txt profiles/en/phoneme_examples.txt profiles/en/frequent_words.txt profiles/en/sentences.ini profiles/en/stop_words.txt ${RHASSPY_APP}/profiles/en/
COPY profiles/defaults.json ${RHASSPY_APP}/profiles/
COPY docker/rhasspy ${RHASSPY_APP}/bin/
COPY dist/ ${RHASSPY_APP}/dist/
COPY etc/wav/* ${RHASSPY_APP}/etc/wav/
COPY rhasspy/profile_schema.json ${RHASSPY_APP}/rhasspy/
COPY *.py ${RHASSPY_APP}/
COPY rhasspy/*.py ${RHASSPY_APP}/rhasspy/
ENV CONFIG_PATH="/data/options.json"
ENTRYPOINT ["/run.sh"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
