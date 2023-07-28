FROM continuumio/anaconda3:5.2.0
#   kaggle
RUN apt-get update \
 && apt-get install --no-install-recommends build-essential unzip -y \
 && conda update -y conda \
 && conda update -y python \
 && pip install pip==23.1 --upgrade \
 && apt-get install --no-install-recommends cmake -y
#  mlcopurse.ai
RUN apt-get install --no-install-recommends apt-utils curl nano -y
#  RUN apt-get install -y python3 python3-dev python-distribute python3-pip git
#  Kaggle
#   Tensorflow doesn't support python 3.7 yet. See https://github.com/tensorflow/tensorflow/issues/20517
#   Fix to install tf 1.10:: Downgrade python 3.7->3.6.6 and downgrade Pandas 0.23.3->0.23.2
RUN conda install -y python=3.6.6
RUN conda upgrade -y notebook
#  Kaggle
#   The anaconda base image includes outdated versions of these packages. Update them to include the latest version.
RUN pip install seaborn==0.12.2 python-dateutil==2.8.2 dask==2023.4.0 --upgrade \
 && pip install pyyaml==6.0 joblib==1.2.0 pytagcloud==0.3.5 husl==4.0.3 geopy==2.3.0 ml_metrics==0.1.4 mne==1.3.1 pyshp==2.3.1
#   && \
#      conda install -y -c conda-forge spacy && python -m spacy download en && \
#      python -m spacy download en_core_web_lg
#  More upgrades to bring in sync with Kaggle
RUN pip install matplotlib==2.2.3 pandas==0.23.4 scikit-learn==0.20.2 numpy==1.16.0
#   main python packages
#  RUN pip3 install --upgrade pip
#  RUN pip3 install --upgrade numpy scipy matplotlib scikit-learn pandas seaborn plotly jupyter statsmodels
#   bug with 5.7.0 jupyter/docker-stacks#721
#  RUN pip3 install notebook==5.6.0
RUN pip install tqdm==4.65.0 pydot==1.4.2 pydotplus==2.0.2 watermark==2.3.1 pillow==9.5.0 mlxtend==0.22.0 ua-parser==0.16.1 user-agents==2.2.0 plotly==5.14.1 --upgrade
#   Graphviz, visualizing trees
RUN apt-get install --no-install-recommends graphviz -y
#  KAggle
#   Make sure the dynamic linker finds the right libstdc++
ENV LD_LIBRARY_PATH="/opt/conda/lib"
#   boost ?
#  RUN apt-get -y install libboost-program-options-dev zlib1g-dev libboost-python-dev
#   JDK ? - this was for h2o
#  RUN apt-get -y install openjdk-8-jdk
#  ENV CPLUS_INCLUDE_PATH=/usr/lib/jvm/java-8-openjdk-amd64/include/linux:/usr/lib/jvm/java-1.8.0-openjdk-amd64/include
#   Vowpal Wabbit
#  RUN git clone -b 8.6.0 https://github.com/VowpalWabbit/vowpal_wabbit.git
#  RUN cd vowpal_wabbit && make && make install
#   python wrapper
#  RUN cd vowpal_wabbit/python && python3 setup.py install
#  RUN pip3 install --upgrade vowpalwabbit
#  kaggle
#  beware that vw -V says 8.5.0 - @datamove
RUN cd /usr/local/src \
 && git clone -b 8.6.0 https://github.com/JohnLangford/vowpal_wabbit.git \
 && ./vowpal_wabbit/python/conda_install.sh \
 && pip install vowpal_wabbit/python \
 && ln -s /usr/local/src/vowpal_wabbit/python/src/vowpalwabbit/vw /opt/conda/bin/vw
#   XGBoost
#  RUN git clone --recursive https://github.com/dmlc/xgboost && \
#      cd xgboost && \
#      make -j4 
#   xgboost python wrapper
#  RUN cd xgboost/python-package; python3 setup.py install && cd ../..
#  RUN apt-get -y install cmake 
#   LightGBM
#  RUN cd /usr/local/src && git clone --recursive --depth 1 https://github.com/Microsoft/LightGBM && \
#      cd LightGBM && mkdir build && cd build && cmake .. && make -j $(nproc) 
#   LightGBM python wrapper
#  RUN cd /usr/local/src/LightGBM/python-package && python3 setup.py install 
#   CatBoost
#  RUN pip3 install --upgrade catboost
#  Kaggle installs with pip
RUN pip install xgboost==1.7.5 \
 && pip install lightgbm==3.3.5 \
 && pip install catboost==1.1.1 --upgrade
#   PyTorch
#  RUN pip3 install http://download.pytorch.org/whl/cpu/torch-0.4.0-cp35-cp35m-linux_x86_64.whl 
#  RUN pip3 install --upgrade torchvision
#   TensorFlow 
#  RUN pip3 install --upgrade tensorflow  
#   Keras with TensorFlow backend
#  RUN pip3 install --upgrade keras
#   Facebook Prophet
RUN pip3 install --upgrade pystan cython
#  Kaggle
#   Latest version of fbprophet fails with incompatibility with PyStan. From the logs:
#   pystan:Something went wrong while unpickling the StanModel. Consider recompiling 
#   See: https://github.com/facebook/prophet/issues/775
RUN pip install fbprophet==0.3.post2
EXPOSE 8888/tcp
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
