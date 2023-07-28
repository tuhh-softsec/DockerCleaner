ARG BASE_TAG=5.3.0
FROM gcr.io/kaggle-images/python-tensorflow-whl:1.13.1-py36-2 AS tensorflow_whl
FROM continuumio/anaconda3:${BASE_TAG}
ADD clean-layer.sh /tmp/clean-layer.sh
ADD patches/nbconvert-extensions.tpl /opt/kaggle/nbconvert-extensions.tpl
#  This is necessary for apt to access HTTPS sources
RUN apt-get update \
 && apt-get install apt-transport-https \
 && /tmp/clean-layer.sh
#  Use a fixed apt-get repo to stop intermittent failures due to flaky httpredir connections,
#  as described by Lionel Chan at http://stackoverflow.com/a/37426929/5881346
RUN sed -i "s/httpredir.debian.org/debian.uchicago.edu/" /etc/apt/sources.list \
 && apt-get update \
 && apt-get install build-essential unzip cmake -y \
 && conda install -y python=3.6.6 \
 && pip install pip --upgrade \
 && /tmp/clean-layer.sh
#  The anaconda base image includes outdated versions of these packages. Update them to include the latest version.
RUN pip install seaborn python-dateutil dask \
 && pip install pyyaml joblib pytagcloud husl geopy ml_metrics mne pyshp \
 && pip install spacy \
 && python -m spacy download en \
 && python -m spacy download en_core_web_lg \
 && apt-get install dbus fontconfig fontconfig-config fonts-dejavu-core fonts-droid-fallback ghostscript gsfonts hicolor-icon-theme libavahi-client3 libavahi-common-data libavahi-common3 libcairo2 libcap-ng0 libcroco3 libcups2 libcupsfilters1 libcupsimage2 libdatrie1 libdbus-1-3 libdjvulibre-text libdjvulibre21 libfftw3-double3 libfontconfig1 libfreetype6 libgdk-pixbuf2.0-0 libgdk-pixbuf2.0-common libgomp1 libgraphite2-3 libgs9 libgs9-common libharfbuzz0b libijs-0.35 libilmbase12 libjbig0 libjbig2dec0 libjpeg62-turbo liblcms2-2 liblqr-1-0 libltdl7 libmagickcore-6.q16-3 libmagickcore-6.q16-3-extra libmagickwand-6.q16-3 libnetpbm10 libopenexr22 libpango-1.0-0 libpangocairo-1.0-0 libpangoft2-1.0-0 libpaper-utils libpaper1 libpixman-1-0 libpng16-16 librsvg2-2 librsvg2-common libthai-data libthai0 libtiff5 libwmf0.2-7 libxcb-render0 libxcb-shm0 netpbm poppler-data p7zip-full -y \
 && cd /usr/local/src \
 && wget --no-verbose https://imagemagick.org/download/ImageMagick.tar.gz \
 && tar xzf ImageMagick.tar.gz \
 && cd `ls -d ImageMagick-* ` \
 && pwd \
 && ls -al \
 && ./configure \
 && make -j $( nproc ;) \
 && make install \
 && /tmp/clean-layer.sh
#  Install tensorflow from a pre-built wheel
COPY --from=tensorflow_whl /tmp/tensorflow_cpu/*.whl /tmp/tensorflow_cpu/
RUN pip install /tmp/tensorflow_cpu/tensorflow*.whl \
 && rm -rf /tmp/tensorflow_cpu \
 && /tmp/clean-layer.sh
RUN apt-get install libfreetype6-dev -y \
 && apt-get install libglib2.0-0 libxext6 libsm6 libxrender1 libfontconfig1 -y --fix-missing \
 && pip install gensim \
 && pip install textblob \
 && pip install wordcloud \
 && conda install -y -c conda-forge python-igraph \
 && pip install xgboost \
 && pip install lightgbm \
 && pip install git+git://github.com/Lasagne/Lasagne.git \
 && pip install keras \
 && pip install keras-rl \
 && pip install git+https://github.com/broadinstitute/keras-rcnn \
 && pip install flake8==3.6.0 \
 && cd /usr/local/src \
 && git clone --depth 1 https://github.com/NervanaSystems/neon.git \
 && cd neon \
 && pip install . \
 && cd /usr/local/src \
 && mkdir nolearn \
 && cd nolearn \
 && git clone --depth 1 https://github.com/dnouri/nolearn.git \
 && cd nolearn \
 && echo "x" > README.rst \
 && echo "x" > CHANGES.rst \
 && python setup.py install \
 && pip install git+git://github.com/Theano/Theano.git --upgrade --no-deps \
 && printf "[global]\nbase_compiledir = /tmp/.theano\n" > /.theanorc \
 && cd /usr/local/src \
 && git clone --depth 1 https://github.com/pybrain/pybrain \
 && cd pybrain \
 && python setup.py install \
 && apt-get install libatlas-base-dev -y \
 && cd /usr/local/src \
 && git clone --depth 1 https://github.com/ztane/python-Levenshtein \
 && cd python-Levenshtein \
 && python setup.py install \
 && pip install hep_ml \
 && pip install chainer \
 && mkdir -p /usr/share/nltk_data \
 && yes | python -m nltk.downloader -d /usr/share/nltk_data abc alpino averaged_perceptron_tagger basque_grammars biocreative_ppi bllip_wsj_no_aux book_grammars brown brown_tei cess_cat cess_esp chat80 city_database cmudict comtrans conll2000 conll2002 conll2007 crubadan dependency_treebank europarl_raw floresta gazetteers genesis gutenberg ieer inaugural indian jeita kimmo knbc large_grammars lin_thesaurus mac_morpho machado masc_tagged maxent_ne_chunker maxent_treebank_pos_tagger moses_sample movie_reviews mte_teip5 names nps_chat omw opinion_lexicon paradigms pil pl196x porter_test ppattach problem_reports product_reviews_1 product_reviews_2 propbank pros_cons ptb punkt qc reuters rslp rte sample_grammars semcor senseval sentence_polarity sentiwordnet shakespeare sinica_treebank smultron snowball_data spanish_grammars state_union stopwords subjectivity swadesh switchboard tagsets timit toolbox treebank twitter_samples udhr2 udhr unicode_samples universal_tagset universal_treebanks_v20 vader_lexicon verbnet webtext word2vec_sample wordnet wordnet_ic words ycoe \
 && pip install stop-words \
 && pip install scikit-image --upgrade \
 && /tmp/clean-layer.sh
#  Make sure the dynamic linker finds the right libstdc++
ENV LD_LIBRARY_PATH="/opt/conda/lib"
RUN apt-get install zlib1g-dev liblcms2-dev libwebp-dev libgeos-dev -y \
 && pip install matplotlib \
 && pip install pyshp \
 && pip install pyproj \
 && conda install basemap \
 && apt-get install libsasl2-dev -y \
 && apt-get install libpq-dev -y \
 && pip install ibis-framework \
 && yes | conda install proj4 \
 && pip install packaging \
 && pip install shapely \
 && pip install cartopy \
 && pip install mxnet \
 && pip install numpy --upgrade \
 && pip install gluonnlp \
 && pip install gluoncv \
 && apt-get install default-jdk -y \
 && cd /usr/local/src \
 && mkdir h2o \
 && cd h2o \
 && wget --no-verbose http://h2o-release.s3.amazonaws.com/h2o/latest_stable -O latest \
 && wget --no-verbose --no-check-certificate -i latest -O h2o.zip \
 && rm latest \
 && unzip h2o.zip \
 && rm h2o.zip \
 && cp h2o-*/h2o.jar . \
 && pip install `find . -name "*whl" ` \
 && python -c "from keras.models import Sequential" \
 && sed -i 's/theano/tensorflow/' /root/.keras/keras.json \
 && python -c "from keras.models import Sequential; from keras import backend; print(backend._BACKEND)" \
 && mkdir -p /tmp/.keras \
 && cp /root/.keras/keras.json /tmp/.keras \
 && /tmp/clean-layer.sh
#  b/128333086: Set PROJ_LIB to points to the proj4 cartographic library.
ENV PROJ_LIB="/opt/conda/share/proj"
#  scikit-learn dependencies
RUN pip install scipy \
 && pip install scikit-learn \
 && pip install h5py \
 && pip install biopython \
 && pip install pudb \
 && pip install imbalanced-learn \
 && pip install line_profiler \
 && pip install orderedmultidict \
 && pip install smhasher \
 && pip install bokeh \
 && pip install numba==0.38.0 \
 && pip install datashader \
 && pip install Boruta \
 && cd /usr/local/src \
 && git clone git://github.com/nicolashennetier/pyeconometrics.git \
 && cd pyeconometrics \
 && python setup.py install \
 && apt-get install graphviz -y \
 && pip install graphviz \
 && apt-get install pandoc -y \
 && pip install git+git://github.com/scikit-learn-contrib/py-earth.git@issue191 \
 && pip install essentia \
 && export CXXFLAGS="-std=c++11" \
 && export CFLAGS="-std=c99" \
 && conda install -y pytorch-cpu torchvision-cpu -c pytorch \
 && apt-get install sox libsox-dev libsox-fmt-all -y \
 && pip install cffi \
 && pip install git+git://github.com/pytorch/audio.git \
 && /tmp/clean-layer.sh
#  vtk with dependencies
RUN apt-get install libgl1-mesa-glx -y \
 && pip install vtk \
 && apt-get install xvfb -y \
 && pip install xvfbwrapper \
 && /tmp/clean-layer.sh
RUN pip install mpld3 \
 && pip install mplleaflet \
 && pip install gpxpy \
 && pip install arrow \
 && pip install nilearn \
 && pip install nibabel \
 && pip install pronouncing \
 && pip install markovify \
 && pip install rf_perm_feat_import \
 && pip install imgaug \
 && pip install preprocessing \
 && pip install Baker \
 && pip install path.py \
 && pip install Geohash \
 && sed -i -- 's/geohash/.geohash/g' /opt/conda/lib/python3.6/site-packages/Geohash/__init__.py \
 && pip install deap \
 && pip install tpot \
 && pip install scikit-optimize \
 && pip install haversine \
 && pip install toolz cytoolz \
 && pip install sacred \
 && pip install plotly \
 && pip install git+https://github.com/nicta/dora.git \
 && pip install git+https://github.com/hyperopt/hyperopt.git \
 && pip install git+https://github.com/tflearn/tflearn.git \
 && pip install fitter \
 && pip install langid \
 && pip install delorean \
 && pip install trueskill \
 && pip install heamy \
 && pip install vida \
 && pip install missingno \
 && pip install pandas-profiling \
 && pip install s2sphere \
 && pip install git+https://github.com/fmfn/BayesianOptimization.git \
 && pip install matplotlib-venn \
 && pip install pyldavis \
 && pip install mlxtend \
 && pip install altair \
 && pip install pystan \
 && pip install ImageHash \
 && pip install ecos \
 && pip install CVXcanon \
 && pip install fancyimpute \
 && pip install git+https://github.com/pymc-devs/pymc3 \
 && pip install tifffile \
 && pip install spectral \
 && pip install descartes \
 && pip install geojson \
 && pip install pysal \
 && pip install pyflux \
 && pip install terminalplot \
 && pip install raccoon \
 && pip install pydicom \
 && pip install wavio \
 && pip install SimpleITK \
 && pip install hmmlearn \
 && pip install bayespy \
 && pip install gplearn \
 && pip install PyAstronomy \
 && pip install squarify \
 && pip install fuzzywuzzy \
 && pip install python-louvain \
 && pip install pyexcel-ods \
 && pip install sklearn-pandas \
 && pip install stemming \
 && pip install fbprophet \
 && pip install holoviews \
 && pip install geoviews==1.6.1 \
 && pip install hypertools \
 && pip install py_stringsimjoin \
 && pip install speedml \
 && pip install nibabel \
 && pip install mlens \
 && pip install scikit-multilearn \
 && pip install cleverhans \
 && pip install leven \
 && pip install catboost \
 && pip install fastFM \
 && pip install lightfm \
 && pip install paramnb \
 && pip install folium \
 && pip install scikit-plot \
 && pip install fury dipy \
 && pip install plotnine==0.4.0 \
 && pip install git+https://github.com/dvaida/hallucinate.git \
 && pip install scikit-surprise \
 && pip install pymongo \
 && pip install edward \
 && pip install geoplot \
 && pip install eli5 \
 && pip install implicit \
 && pip install dask-ml[xgboost] \
 && /tmp/clean-layer.sh
RUN pip install kmeans-smote --no-dependencies \
 && cd /opt/ \
 && git clone https://github.com/PAIR-code/facets \
 && cd facets/ \
 && jupyter nbextension install facets-dist/ --user \
 && export PYTHONPATH=$PYTHONPATH:/opt/facets/facets_overview/python/ \
 && pip install ethnicolr --no-dependencies \
 && pip install tensorpack \
 && pip install pycountry \
 && pip install iso3166 \
 && pip install pydash \
 && pip install kmodes --no-dependencies \
 && pip install librosa \
 && pip install polyglot \
 && pip install mmh3 \
 && pip install fbpca \
 && pip install sentencepiece \
 && pip install cufflinks \
 && pip install glmnet_py \
 && pip install lime \
 && pip install memory_profiler \
 && /tmp/clean-layer.sh
#  install cython & cysignals before pyfasttext
RUN pip install cython --upgrade \
 && pip install cysignals --upgrade \
 && pip install pyfasttext \
 && pip install ktext \
 && pip install git+git://github.com/facebookresearch/fastText.git \
 && apt-get install libhunspell-dev -y \
 && pip install hunspell \
 && pip install annoy \
 && pip install category_encoders \
 && pip install google-cloud-bigquery==1.12.1 \
 && pip install google-cloud-storage \
 && pip install ortools \
 && pip install scattertext \
 && pip install pandas-datareader \
 && pip install pykoko \
 && pip install wordsegment \
 && pip install pyahocorasick \
 && pip install wordbatch \
 && pip install emoji \
 && pip install janome \
 && pip install wfdb \
 && pip install vecstack \
 && pip install sklearn-contrib-lightning \
 && pip install yellowbrick \
 && pip install mlcrate \
 && pip install vega3 \
 && jupyter nbextension install --sys-prefix --py vega3 \
 && /tmp/clean-layer.sh
#  Fast.ai and dependencies
RUN pip install bcolz \
 && pip install bleach \
 && pip install certifi \
 && pip install cycler \
 && pip install decorator \
 && pip install entrypoints \
 && pip install html5lib \
 && pip install ipykernel \
 && pip install ipython \
 && pip install ipython-genutils \
 && pip install ipywidgets \
 && pip install isoweek \
 && pip install jedi \
 && pip install Jinja2 \
 && pip install jsonschema \
 && pip install jupyter \
 && pip install jupyter-client \
 && pip install jupyter-console \
 && pip install jupyter-core \
 && pip install MarkupSafe \
 && pip install mistune \
 && pip install nbconvert \
 && pip install nbformat \
 && pip install notebook==5.5.0 \
 && pip install olefile \
 && pip install opencv-python \
 && pip install pandas==0.23.4 \
 && pip install pandas_summary \
 && pip install pandocfilters \
 && pip install pexpect \
 && pip install pickleshare \
 && pip install Pillow \
 && pip install ptyprocess \
 && pip install Pygments \
 && pip install pyparsing \
 && pip install pytz \
 && pip install PyYAML \
 && pip install pyzmq \
 && pip install qtconsole \
 && pip install simplegeneric \
 && pip install six \
 && pip install terminado \
 && pip install testpath \
 && pip install tornado==5.0.2 \
 && pip install tqdm \
 && pip install traitlets \
 && pip install wcwidth \
 && pip install webencodings \
 && pip install widgetsnbextension \
 && pip install pyarrow==0.10.0 \
 && pip install feather-format \
 && pip install fastai \
 && pip install torchtext \
 && /tmp/clean-layer.sh
#  allennlp and dependencies
#  TODO: install deps when underlying dependency is fixed. https://github.com/Kaggle/docker-python/issues/548
RUN pip install jsonnet overrides tensorboardX \
 && pip install flask flask-cors gevent > =1.0.2 > =3.0.7 > =1.3.6 \
 && pip install unidecode parsimonious sqlparse word2number > =0.8.0 > =0.2.4 > =1.1 \
 && pip install pytorch-pretrained-bert jsonpickle > =0.6.0 \
 && pip install requests editdistance conllu==0.11 > =2.18 \
 && pip install allennlp --no-dependencies \
 && /tmp/clean-layer.sh
# ##########
#
#       NEW CONTRIBUTORS:
#  Please add new pip/apt installs in this block. Don't forget a "&& \" at the end
#  of all non-final lines. Thanks!
#
# ##########
RUN pip install flashtext \
 && pip install marisa-trie \
 && pip install pyemd \
 && pip install pyupset \
 && pip install pympler \
 && pip install s3fs \
 && pip install featuretools \
 && pip install -e git+https://github.com/SohierDane/BigQuery_Helper#egg=bq_helper \
 && pip install hpsklearn \
 && pip install keras-tqdm \
 && pip install git+https://github.com/Kaggle/learntools \
 && pip install kmapper \
 && pip install shap \
 && pip install ray \
 && pip install gym \
 && pip install tensorforce \
 && pip install pyarabic \
 && pip install conx \
 && pip install pandasql \
 && pip install trackml \
 && pip install tensorflow_hub \
 && pip install jieba \
 && pip install git+https://github.com/SauceCat/PDPbox \
 && pip install ggplot \
 && pip install cesium \
 && pip install rgf_python \
 && pip install pytext-nlp==0.1.2 \
 && pip install tsfresh \
 && pip install pymagnitude \
 && pip install pykalman \
 && pip install optuna \
 && pip install chainercv \
 && pip install plotly_express \
 && pip install albumentations \
 && pip install pytorch-ignite \
 && /tmp/clean-layer.sh
#  Tesseract and some associated utility packages
RUN apt-get install tesseract-ocr -y \
 && pip install pytesseract \
 && pip install wand==0.5.3 \
 && pip install pdf2image \
 && pip install PyPDF \
 && pip install pyocr \
 && /tmp/clean-layer.sh
ENV TESSERACT_PATH="/usr/bin/tesseract"
#  Pin Vowpal Wabbit v8.6.0 because 8.6.1 does not build or install successfully
RUN cd /usr/local/src \
 && git clone -b 8.6.0 https://github.com/JohnLangford/vowpal_wabbit.git \
 && ./vowpal_wabbit/python/conda_install.sh \
 && pip install vowpal_wabbit/python \
 && /tmp/clean-layer.sh
#  For Facets
ENV PYTHONPATH="$PYTHONPATH:/opt/facets/facets_overview/python/"
#  For Theano with MKL
ENV MKL_THREADING_LAYER="GNU"
#  Temporary fixes and patches
#  Temporary patch for Dask getting downgraded, which breaks Keras
RUN pip install dask --upgrade \
 && mkdir -p /root/.jupyter \
 && touch /root/.jupyter/jupyter_nbconvert_config.py \
 && touch /root/.jupyter/migrated \
 && mkdir -p /.jupyter \
 && touch /.jupyter/jupyter_nbconvert_config.py \
 && touch /.jupyter/migrated \
 && sed -i "s/^.*Matplotlib is building the font cache using fc-list.*$/# Warning removed by Kaggle/g" /opt/conda/lib/python3.6/site-packages/matplotlib/font_manager.py \
 && mkdir -p /etc/ipython/ \
 && echo "c = get_config(); c.IPKernelApp.matplotlib = 'inline'" > /etc/ipython/ipython_config.py \
 && /tmp/clean-layer.sh
#  Add BigQuery client proxy settings
ENV PYTHONUSERBASE="\"/root/.local\""
ADD patches/kaggle_gcp.py /root/.local/lib/python3.6/site-packages/kaggle_gcp.py
ADD patches/kaggle_secrets.py /root/.local/lib/python3.6/site-packages/kaggle_secrets.py
ADD patches/log.py /root/.local/lib/python3.6/site-packages/log.py
ADD patches/sitecustomize.py /root/.local/lib/python3.6/site-packages/sitecustomize.py
#  TensorBoard Jupyter extension. Should be replaced with TensorBoard's provided magic once we have
#  worker tunneling support in place.
ENV JUPYTER_CONFIG_DIR="\"/root/.jupyter/\""
RUN pip install jupyter_tensorboard \
 && jupyter serverextension enable jupyter_tensorboard \
 && jupyter tensorboard enable
ADD patches/tensorboard/notebook.py /opt/conda/lib/python3.6/site-packages/tensorboard/notebook.py
#  Set backend for matplotlib
ENV MPLBACKEND="\"agg\""
