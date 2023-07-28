FROM ubuntu:latest
ENV LANGUAGE="en_US.UTF-8"
ENV LANG="en_US.UTF-8"
ENV LC_ALL="en_US.UTF-8"
ENV PYTHONIOENCODING="UTF-8"
ENV NB_USER="ubuntu"
RUN useradd -ms /bin/bash ubuntu
RUN apt-get update -y \
 && apt-get -y dist-upgrade \
 && apt-get -y upgrade \
 && apt-get install --no-install-recommends sudo=1.9.13p1-1ubuntu2 build-essential=12.9ubuntu3 python3-dev=3.11.2-1 python3-pip=23.0.1+dfsg-1 ca-certificates=20230311 curl=7.88.1-7ubuntu1 git=1:2.39.2-1ubuntu1 gfortran=4:12.2.0-3ubuntu1 libblas-dev=3.11.0-2 liblapack-dev=3.11.0-2 libssl-dev=3.0.8-1ubuntu1 libffi-dev=3.4.4-1 libcurl4-openssl-dev=7.88.1-7ubuntu1 libgdal-dev=3.6.2+dfsg-1build1 wget=1.21.3-1ubuntu1 jq=1.6-2.1ubuntu3 language-pack-en=1:23.04+20230317 libcurl4-openssl-dev=7.88.1-7ubuntu1 libffi-dev=3.4.4-1 libzmq3-dev=4.3.4-6 libxml2-dev=2.9.14+dfsg-1.1build2 libxslt-dev python3-lxml=4.9.2-1build1 zlib1g-dev=1:1.2.13.dfsg-1ubuntu4 python3-mysqldb=1.4.6-2build1 -y \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/*
#   install latest version of pip
RUN pip3 install -U pip
#   TODO: Move the Python libraries to a requirements.txt file?
#   install basic Python libraries to run Jupyter
RUN pip3 install -U notebook==5.2.* jupyterhub==0.8.* ipython
#   add libraries used in intro to python exercise
RUN pip3 install -U jellyfish ngram
#   add standard data science libraries
RUN pip3 install -U numpy scipy matplotlib pandas statsmodels scikit-learn
#   add libraries for teaching web APIs
RUN pip3 install -U requests requests_oauthlib Flask slackclient
#   add libraries for NLP
RUN pip3 install -U spacy nltk gensim
#   add libraries for visualization/mapping
RUN pip3 install -U seaborn bokeh folium geopandas geopy
#   add libraries for finance
RUN pip3 install -U googlefinance yahoo-finance quandl
#   misc libraries
RUN pip3 install -U boto boto3 elasticsearch networkx py2neo pymongo selenium tweepy
ARG FILE_PATH
#   Add a notebook profile.
COPY $FILE_PATH/jupyter_notebook_config.py /etc/jupyter/
RUN echo "c.NotebookApp.notebook_dir = '/notebooks'" >> /etc/jupyter/jupyter_notebook_config.py
RUN echo "c.NotebookApp.allow_root = True" >> /etc/jupyter/jupyter_notebook_config.py
RUN echo "$NB_USER ALL=NOPASSWD: ALL" >> /etc/sudoers
WORKDIR /notebooks
RUN ["git", "clone", "--verbose", "https://github.com/ipeirotis/dealing_with_data.git", "/notebooks"]
#   VOLUME /notebooks
WORKDIR /data
RUN ["git", "clone", "--verbose", "https://github.com/ipeirotis/data.git", "/data"]
#   VOLUME /data
RUN pip3 install ipython-sql sql_magic mysqlclient
EXPOSE 8888/tcp
LABEL org.jupyter.service="jupyter"
RUN chmod -R 777 /notebooks
RUN chmod -R 777 /data
CMD ["start-notebook.sh"]
#   Add local files as late as possible to avoid cache busting
COPY $FILE_PATH/start-notebook.sh /usr/local/bin/
USER $NB_USER
# Please add your HEALTHCHECK here!!!
