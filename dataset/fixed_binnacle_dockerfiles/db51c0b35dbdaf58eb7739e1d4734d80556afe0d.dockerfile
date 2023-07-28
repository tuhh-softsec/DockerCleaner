ARG BASE_TAG=5.3.0
FROM gcr.io/kaggle-images/python-tensorflow-whl:1.13.1-py36-2 AS tensorflow_whl
FROM continuumio/anaconda3:${BASE_TAG}
COPY clean-layer.sh /tmp/clean-layer.sh
COPY patches/nbconvert-extensions.tpl /opt/kaggle/nbconvert-extensions.tpl
#   This is necessary for apt to access HTTPS sources
RUN apt-get update \
 && apt-get install --no-install-recommends apt-transport-https \
 && /tmp/clean-layer.sh
#   Use a fixed apt-get repo to stop intermittent failures due to flaky httpredir connections,
#   as described by Lionel Chan at http://stackoverflow.com/a/37426929/5881346
RUN sed -i "s/httpredir.debian.org/debian.uchicago.edu/" /etc/apt/sources.list \
 && apt-get update \
 && apt-get install --no-install-recommends build-essential unzip cmake -y \
 && conda install -y python=3.6.6 \
 && pip install pip==23.1 --upgrade \
 && /tmp/clean-layer.sh
#   The anaconda base image includes outdated versions of these packages. Update them to include the latest version.
RUN pip install seaborn==0.12.2 python-dateutil==2.8.2 dask==2023.4.0 \
 && pip install pyyaml==6.0 joblib==1.2.0 pytagcloud==0.3.5 husl==4.0.3 geopy==2.3.0 ml_metrics==0.1.4 mne==1.3.1 pyshp==2.3.1 \
 && pip install spacy==3.5.2 \
 && python -m spacy download en \
 && python -m spacy download en_core_web_lg \
 && apt-get install --no-install-recommends dbus fontconfig fontconfig-config fonts-dejavu-core fonts-droid-fallback ghostscript gsfonts hicolor-icon-theme libavahi-client3 libavahi-common-data libavahi-common3 libcairo2 libcap-ng0 libcroco3 libcups2 libcupsfilters1 libcupsimage2 libdatrie1 libdbus-1-3 libdjvulibre-text libdjvulibre21 libfftw3-double3 libfontconfig1 libfreetype6 libgdk-pixbuf2.0-0 libgdk-pixbuf2.0-common libgomp1 libgraphite2-3 libgs9 libgs9-common libharfbuzz0b libijs-0.35 libilmbase12 libjbig0 libjbig2dec0 libjpeg62-turbo liblcms2-2 liblqr-1-0 libltdl7 libmagickcore-6.q16-3 libmagickcore-6.q16-3-extra libmagickwand-6.q16-3 libnetpbm10 libopenexr22 libpango-1.0-0 libpangocairo-1.0-0 libpangoft2-1.0-0 libpaper-utils libpaper1 libpixman-1-0 libpng16-16 librsvg2-2 librsvg2-common libthai-data libthai0 libtiff5 libwmf0.2-7 libxcb-render0 libxcb-shm0 netpbm poppler-data p7zip-full -y \
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
#   Install tensorflow from a pre-built wheel
COPY --from=tensorflow_whl /tmp/tensorflow_cpu/*.whl /tmp/tensorflow_cpu/
RUN pip install /tmp/tensorflow_cpu/tensorflow*.whl \
 && rm -rf /tmp/tensorflow_cpu \
 && /tmp/clean-layer.sh
RUN apt-get install --no-install-recommends libfreetype6-dev -y \
 && apt-get install --no-install-recommends libglib2.0-0 libxext6 libsm6 libxrender1 libfontconfig1 -y --fix-missing \
 && pip install gensim==4.3.1 \
 && pip install textblob==0.17.1 \
 && pip install wordcloud==1.8.2.2 \
 && conda install -y -c conda-forge python-igraph \
 && pip install xgboost==1.7.5 \
 && pip install lightgbm==3.3.5 \
 && pip install git+git://github.com/Lasagne/Lasagne.git \
 && pip install keras==2.12.0 \
 && pip install keras-rl==0.4.2 \
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
 && apt-get install --no-install-recommends libatlas-base-dev -y \
 && cd /usr/local/src \
 && git clone --depth 1 https://github.com/ztane/python-Levenshtein \
 && cd python-Levenshtein \
 && python setup.py install \
 && pip install hep_ml==0.7.2 \
 && pip install chainer==7.8.1 \
 && mkdir -p /usr/share/nltk_data \
 && yes | python -m nltk.downloader -d /usr/share/nltk_data abc alpino averaged_perceptron_tagger basque_grammars biocreative_ppi bllip_wsj_no_aux book_grammars brown brown_tei cess_cat cess_esp chat80 city_database cmudict comtrans conll2000 conll2002 conll2007 crubadan dependency_treebank europarl_raw floresta gazetteers genesis gutenberg ieer inaugural indian jeita kimmo knbc large_grammars lin_thesaurus mac_morpho machado masc_tagged maxent_ne_chunker maxent_treebank_pos_tagger moses_sample movie_reviews mte_teip5 names nps_chat omw opinion_lexicon paradigms pil pl196x porter_test ppattach problem_reports product_reviews_1 product_reviews_2 propbank pros_cons ptb punkt qc reuters rslp rte sample_grammars semcor senseval sentence_polarity sentiwordnet shakespeare sinica_treebank smultron snowball_data spanish_grammars state_union stopwords subjectivity swadesh switchboard tagsets timit toolbox treebank twitter_samples udhr2 udhr unicode_samples universal_tagset universal_treebanks_v20 vader_lexicon verbnet webtext word2vec_sample wordnet wordnet_ic words ycoe \
 && pip install stop-words==2018.7.23 \
 && pip install scikit-image==0.20.0 --upgrade \
 && /tmp/clean-layer.sh
#   Make sure the dynamic linker finds the right libstdc++
ENV LD_LIBRARY_PATH="/opt/conda/lib"
RUN apt-get install --no-install-recommends zlib1g-dev liblcms2-dev libwebp-dev libgeos-dev -y \
 && pip install matplotlib==3.7.1 \
 && pip install pyshp==2.3.1 \
 && pip install pyproj==3.5.0 \
 && conda install basemap \
 && apt-get install --no-install-recommends libsasl2-dev -y \
 && apt-get install --no-install-recommends libpq-dev -y \
 && pip install ibis-framework==5.1.0 \
 && yes | conda install proj4 \
 && pip install packaging==23.1 \
 && pip install shapely==2.0.1 \
 && pip install cartopy==0.21.1 \
 && pip install mxnet==1.9.1 \
 && pip install numpy==1.24.2 --upgrade \
 && pip install gluonnlp==0.10.0 \
 && pip install gluoncv==0.10.5.post0 \
 && apt-get install --no-install-recommends default-jdk -y \
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
#   b/128333086: Set PROJ_LIB to points to the proj4 cartographic library.
ENV PROJ_LIB="/opt/conda/share/proj"
#   scikit-learn dependencies
RUN pip install scipy==1.10.1 \
 && pip install scikit-learn==1.2.2 \
 && pip install h5py==3.8.0 \
 && pip install biopython==1.81 \
 && pip install pudb==2022.1.3 \
 && pip install imbalanced-learn==0.10.1 \
 && pip install line_profiler==4.0.3 \
 && pip install orderedmultidict==1.0.1 \
 && pip install smhasher==0.150.1 \
 && pip install bokeh==3.1.0 \
 && pip install numba==0.38.0 \
 && pip install datashader==0.14.4 \
 && pip install Boruta==0.3 \
 && cd /usr/local/src \
 && git clone git://github.com/nicolashennetier/pyeconometrics.git \
 && cd pyeconometrics \
 && python setup.py install \
 && apt-get install --no-install-recommends graphviz -y \
 && pip install graphviz==0.20.1 \
 && apt-get install --no-install-recommends pandoc -y \
 && pip install git+git://github.com/scikit-learn-contrib/py-earth.git@issue191 \
 && pip install essentia==2.1b6.dev871 \
 && export CXXFLAGS="-std=c++11" \
 && export CFLAGS="-std=c99" \
 && conda install -y pytorch-cpu torchvision-cpu -c pytorch \
 && apt-get install --no-install-recommends sox libsox-dev libsox-fmt-all -y \
 && pip install cffi==1.15.1 \
 && pip install git+git://github.com/pytorch/audio.git \
 && /tmp/clean-layer.sh
#   vtk with dependencies
RUN apt-get install --no-install-recommends libgl1-mesa-glx -y \
 && pip install vtk==9.2.6 \
 && apt-get install --no-install-recommends xvfb -y \
 && pip install xvfbwrapper==0.2.9 \
 && /tmp/clean-layer.sh
RUN pip install mpld3==0.5.9 \
 && pip install mplleaflet==0.0.5 \
 && pip install gpxpy==1.5.0 \
 && pip install arrow==1.2.3 \
 && pip install nilearn==0.10.0 \
 && pip install nibabel==5.1.0 \
 && pip install pronouncing==0.2.0 \
 && pip install markovify==0.9.4 \
 && pip install rf_perm_feat_import==0.1 \
 && pip install imgaug==0.4.0 \
 && pip install preprocessing==0.1.13 \
 && pip install Baker==1.3 \
 && pip install path.py==12.5.0 \
 && pip install Geohash==1.0 \
 && sed -i -- 's/geohash/.geohash/g' /opt/conda/lib/python3.6/site-packages/Geohash/__init__.py \
 && pip install deap==1.3.3 \
 && pip install tpot==0.11.7 \
 && pip install scikit-optimize==0.9.0 \
 && pip install haversine==2.8.0 \
 && pip install toolz==0.12.0 cytoolz==0.12.1 \
 && pip install sacred==0.8.4 \
 && pip install plotly==5.14.1 \
 && pip install git+https://github.com/nicta/dora.git \
 && pip install git+https://github.com/hyperopt/hyperopt.git \
 && pip install git+https://github.com/tflearn/tflearn.git \
 && pip install fitter==1.5.2 \
 && pip install langid==1.1.6 \
 && pip install delorean==1.0.0 \
 && pip install trueskill==0.4.5 \
 && pip install heamy==0.0.7 \
 && pip install vida==0.3 \
 && pip install missingno==0.5.2 \
 && pip install pandas-profiling==3.6.6 \
 && pip install s2sphere==0.2.5 \
 && pip install git+https://github.com/fmfn/BayesianOptimization.git \
 && pip install matplotlib-venn==0.11.9 \
 && pip install pyldavis==3.4.0 \
 && pip install mlxtend==0.22.0 \
 && pip install altair==4.2.2 \
 && pip install pystan==3.6.0 \
 && pip install ImageHash==4.3.1 \
 && pip install ecos==2.0.12 \
 && pip install CVXcanon==0.1.2 \
 && pip install fancyimpute==0.7.0 \
 && pip install git+https://github.com/pymc-devs/pymc3 \
 && pip install tifffile==2023.4.12 \
 && pip install spectral==0.23.1 \
 && pip install descartes==1.1.0 \
 && pip install geojson==3.0.1 \
 && pip install pysal==23.1 \
 && pip install pyflux==0.4.15 \
 && pip install terminalplot==0.3.0 \
 && pip install raccoon==3.0.0 \
 && pip install pydicom==2.3.1 \
 && pip install wavio==0.0.7 \
 && pip install SimpleITK==2.2.1 \
 && pip install hmmlearn==0.2.8 \
 && pip install bayespy==0.5.25 \
 && pip install gplearn==0.4.2 \
 && pip install PyAstronomy==0.19.0 \
 && pip install squarify==0.4.3 \
 && pip install fuzzywuzzy==0.18.0 \
 && pip install python-louvain==0.16 \
 && pip install pyexcel-ods==0.6.0 \
 && pip install sklearn-pandas==2.2.0 \
 && pip install stemming==1.0.1 \
 && pip install fbprophet==0.7.1 \
 && pip install holoviews==1.15.4 \
 && pip install geoviews==1.6.1 \
 && pip install hypertools==0.8.0 \
 && pip install py_stringsimjoin==0.3.3 \
 && pip install speedml==0.9.3 \
 && pip install nibabel==5.1.0 \
 && pip install mlens==0.2.3 \
 && pip install scikit-multilearn==0.2.0 \
 && pip install cleverhans==4.0.0 \
 && pip install leven==1.0.4 \
 && pip install catboost==1.1.1 \
 && pip install fastFM==0.2.11 \
 && pip install lightfm==1.17 \
 && pip install paramnb==2.0.4 \
 && pip install folium==0.14.0 \
 && pip install scikit-plot==0.3.7 \
 && pip install fury==0.9.0 dipy==1.6.0 \
 && pip install plotnine==0.4.0 \
 && pip install git+https://github.com/dvaida/hallucinate.git \
 && pip install scikit-surprise==1.1.3 \
 && pip install pymongo==4.3.3 \
 && pip install edward==1.3.5 \
 && pip install geoplot==0.5.1 \
 && pip install eli5==0.13.0 \
 && pip install implicit==0.6.2 \
 && pip install dask-ml[xgboost] \
 && /tmp/clean-layer.sh
RUN pip install kmeans-smote==0.1.2 --no-dependencies \
 && cd /opt/ \
 && git clone https://github.com/PAIR-code/facets \
 && cd facets/ \
 && jupyter nbextension install facets-dist/ --user \
 && export PYTHONPATH=$PYTHONPATH:/opt/facets/facets_overview/python/ \
 && pip install ethnicolr==0.9.3 --no-dependencies \
 && pip install tensorpack==0.11 \
 && pip install pycountry==22.3.5 \
 && pip install iso3166==2.1.1 \
 && pip install pydash==7.0.1 \
 && pip install kmodes==0.12.2 --no-dependencies \
 && pip install librosa==0.10.0.post2 \
 && pip install polyglot==16.7.4 \
 && pip install mmh3==3.1.0 \
 && pip install fbpca==1.0 \
 && pip install sentencepiece==0.1.98 \
 && pip install cufflinks==0.17.3 \
 && pip install glmnet_py==0.1.0b2 \
 && pip install lime==0.2.0.1 \
 && pip install memory_profiler==0.61.0 \
 && /tmp/clean-layer.sh
#   install cython & cysignals before pyfasttext
RUN pip install cython==0.29.34 --upgrade \
 && pip install cysignals==1.11.2 --upgrade \
 && pip install pyfasttext==0.4.6 \
 && pip install ktext==0.40 \
 && pip install git+git://github.com/facebookresearch/fastText.git \
 && apt-get install --no-install-recommends libhunspell-dev -y \
 && pip install hunspell==0.5.5 \
 && pip install annoy==1.17.2 \
 && pip install category_encoders==2.6.0 \
 && pip install google-cloud-bigquery==1.12.1 \
 && pip install google-cloud-storage==2.8.0 \
 && pip install ortools==9.6.2534 \
 && pip install scattertext==0.1.18 \
 && pip install pandas-datareader==0.10.0 \
 && pip install pykoko==0.1.8 \
 && pip install wordsegment==1.3.1 \
 && pip install pyahocorasick==2.0.0 \
 && pip install wordbatch==1.4.9 \
 && pip install emoji==2.2.0 \
 && pip install janome==0.4.2 \
 && pip install wfdb==4.1.0 \
 && pip install vecstack==0.4.0 \
 && pip install sklearn-contrib-lightning==0.6.2.post0 \
 && pip install yellowbrick==1.5 \
 && pip install mlcrate==0.2.0 \
 && pip install vega3==0.13.0 \
 && jupyter nbextension install --sys-prefix --py vega3 \
 && /tmp/clean-layer.sh
#   Fast.ai and dependencies
RUN pip install bcolz==1.2.1 \
 && pip install bleach==6.0.0 \
 && pip install certifi==2022.12.7 \
 && pip install cycler==0.11.0 \
 && pip install decorator==5.1.1 \
 && pip install entrypoints==0.4 \
 && pip install html5lib==1.1 \
 && pip install ipykernel==6.22.0 \
 && pip install ipython==8.12.0 \
 && pip install ipython-genutils==0.2.0 \
 && pip install ipywidgets==8.0.6 \
 && pip install isoweek==1.3.3 \
 && pip install jedi==0.18.2 \
 && pip install Jinja2==3.1.2 \
 && pip install jsonschema==4.17.3 \
 && pip install jupyter==1.0.0 \
 && pip install jupyter-client==8.2.0 \
 && pip install jupyter-console==6.6.3 \
 && pip install jupyter-core==5.3.0 \
 && pip install MarkupSafe==2.1.2 \
 && pip install mistune==2.0.5 \
 && pip install nbconvert==7.3.1 \
 && pip install nbformat==5.8.0 \
 && pip install notebook==5.5.0 \
 && pip install olefile==0.46 \
 && pip install opencv-python==4.7.0.72 \
 && pip install pandas==0.23.4 \
 && pip install pandas_summary==0.2.0 \
 && pip install pandocfilters==1.5.0 \
 && pip install pexpect==4.8.0 \
 && pip install pickleshare==0.7.5 \
 && pip install Pillow==9.5.0 \
 && pip install ptyprocess==0.7.0 \
 && pip install Pygments==2.15.0 \
 && pip install pyparsing==3.0.9 \
 && pip install pytz==2023.3 \
 && pip install PyYAML==6.0 \
 && pip install pyzmq==25.0.2 \
 && pip install qtconsole==5.4.2 \
 && pip install simplegeneric==0.8.1 \
 && pip install six==1.16.0 \
 && pip install terminado==0.17.1 \
 && pip install testpath==0.6.0 \
 && pip install tornado==5.0.2 \
 && pip install tqdm==4.65.0 \
 && pip install traitlets==5.9.0 \
 && pip install wcwidth==0.2.6 \
 && pip install webencodings==0.5.1 \
 && pip install widgetsnbextension==4.0.7 \
 && pip install pyarrow==0.10.0 \
 && pip install feather-format==0.4.1 \
 && pip install fastai==2.7.12 \
 && pip install torchtext==0.15.1 \
 && /tmp/clean-layer.sh
#   allennlp and dependencies
#   TODO: install deps when underlying dependency is fixed. https://github.com/Kaggle/docker-python/issues/548
RUN pip install jsonnet==0.19.1 overrides==7.3.1 tensorboardX==2.6 \
 && pip install flask==2.2.3 flask-cors==3.0.10 gevent==22.10.2 > =1.0.2 > =3.0.7 > =1.3.6 \
 && pip install unidecode==1.3.6 parsimonious==0.10.0 sqlparse==0.4.3 word2number==1.1 > =0.8.0 > =0.2.4 > =1.1 \
 && pip install pytorch-pretrained-bert==0.6.2 jsonpickle==3.0.1 > =0.6.0 \
 && pip install requests==2.28.2 editdistance==0.6.2 conllu==0.11 > =2.18 \
 && pip install allennlp==2.10.1 --no-dependencies \
 && /tmp/clean-layer.sh
#  ##########
#
#        NEW CONTRIBUTORS:
#   Please add new pip/apt installs in this block. Don't forget a "&& \" at the end
#   of all non-final lines. Thanks!
#
#  ##########
RUN pip install flashtext==2.7 \
 && pip install marisa-trie==0.8.0 \
 && pip install pyemd==1.0.0 \
 && pip install pyupset==0.1.1.post7 \
 && pip install pympler==1.0.1 \
 && pip install s3fs==2023.4.0 \
 && pip install featuretools==1.25.0 \
 && pip install -e git+https://github.com/SohierDane/BigQuery_Helper#egg=bq_helper \
 && pip install hpsklearn==0.1.0 \
 && pip install keras-tqdm==2.0.1 \
 && pip install git+https://github.com/Kaggle/learntools \
 && pip install kmapper==2.0.1 \
 && pip install shap==0.41.0 \
 && pip install ray==2.3.1 \
 && pip install gym==0.26.2 \
 && pip install tensorforce==0.6.5 \
 && pip install pyarabic==0.6.15 \
 && pip install conx==3.7.10 \
 && pip install pandasql==0.7.3 \
 && pip install trackml==0.1.12 \
 && pip install tensorflow_hub==0.13.0 \
 && pip install jieba==0.42.1 \
 && pip install git+https://github.com/SauceCat/PDPbox \
 && pip install ggplot==0.11.5 \
 && pip install cesium==0.11.1 \
 && pip install rgf_python==3.12.0 \
 && pip install pytext-nlp==0.1.2 \
 && pip install tsfresh==0.20.0 \
 && pip install pymagnitude==0.1.143 \
 && pip install pykalman==0.9.5 \
 && pip install optuna==3.1.1 \
 && pip install chainercv==0.13.1 \
 && pip install plotly_express==0.4.1 \
 && pip install albumentations==1.3.0 \
 && pip install pytorch-ignite==0.4.11 \
 && /tmp/clean-layer.sh
#   Tesseract and some associated utility packages
RUN apt-get install --no-install-recommends tesseract-ocr -y \
 && pip install pytesseract==0.3.10 \
 && pip install wand==0.5.3 \
 && pip install pdf2image==1.16.3 \
 && pip install PyPDF==3.7.1 \
 && pip install pyocr==0.8.3 \
 && /tmp/clean-layer.sh
ENV TESSERACT_PATH="/usr/bin/tesseract"
#   Pin Vowpal Wabbit v8.6.0 because 8.6.1 does not build or install successfully
RUN cd /usr/local/src \
 && git clone -b 8.6.0 https://github.com/JohnLangford/vowpal_wabbit.git \
 && ./vowpal_wabbit/python/conda_install.sh \
 && pip install vowpal_wabbit/python \
 && /tmp/clean-layer.sh
#   For Facets
ENV PYTHONPATH="$PYTHONPATH:/opt/facets/facets_overview/python/"
#   For Theano with MKL
ENV MKL_THREADING_LAYER="GNU"
#   Temporary fixes and patches
#   Temporary patch for Dask getting downgraded, which breaks Keras
RUN pip install dask==2023.4.0 --upgrade \
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
#   Add BigQuery client proxy settings
ENV PYTHONUSERBASE="\"/root/.local\""
COPY patches/kaggle_gcp.py /root/.local/lib/python3.6/site-packages/kaggle_gcp.py
COPY patches/kaggle_secrets.py /root/.local/lib/python3.6/site-packages/kaggle_secrets.py
COPY patches/log.py /root/.local/lib/python3.6/site-packages/log.py
COPY patches/sitecustomize.py /root/.local/lib/python3.6/site-packages/sitecustomize.py
#   TensorBoard Jupyter extension. Should be replaced with TensorBoard's provided magic once we have
#   worker tunneling support in place.
ENV JUPYTER_CONFIG_DIR="\"/root/.jupyter/\""
RUN pip install jupyter_tensorboard==0.2.0 \
 && jupyter serverextension enable jupyter_tensorboard \
 && jupyter tensorboard enable
COPY patches/tensorboard/notebook.py /opt/conda/lib/python3.6/site-packages/tensorboard/notebook.py
#   Set backend for matplotlib
ENV MPLBACKEND="\"agg\""
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
