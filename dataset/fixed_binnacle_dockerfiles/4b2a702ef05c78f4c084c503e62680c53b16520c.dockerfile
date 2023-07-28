FROM 0x0l/base:latest
ENV PYTHONIOENCODING="UTF-8"
RUN apt-get update -qq \
 && apt-get install --no-install-recommends curl=7.88.1-7ubuntu1 ffmpeg=7:5.1.2-3ubuntu1 g++=4:12.2.0-3ubuntu1 gcc=4:12.2.0-3ubuntu1 gfortran=4:12.2.0-3ubuntu1 git=1:2.39.2-1ubuntu1 graphviz=2.42.2-7build3 graphviz-dev less=590-1.2 libblosc-dev=1.21.3+ds-1 libedit-dev=3.1-20221030-2 libfreetype6-dev=2.12.1+dfsg-4 libgmp-dev=2:6.2.1+dfsg1-1.1ubuntu1 libhdf5-dev=1.10.8+repack1-1ubuntu1 libjpeg-dev=8c-2ubuntu11 libnetcdf-dev=1:4.9.0-3 libopenblas-dev=0.3.21+ds-4 libpng3-dev libssl-dev=3.0.8-1ubuntu1 libxml2-dev=2.9.14+dfsg-1.1build2 libxslt-dev libzmq3-dev=4.3.4-6 llvm-3.7-dev pandoc=2.17.1.1-1.1ubuntu1 pkg-config=1.8.1-1ubuntu2 python3=3.11.2-1 python3-dev=3.11.2-1 vim=2:9.0.1000-4ubuntu2 zip=3.0-13 -yqq
RUN curl https://bootstrap.pypa.io/get-pip.py > get-pip.py \
 && python3 get-pip.py \
 && rm get-pip.py
RUN pip install Cython==0.29.34 numpy==1.24.2 -q
RUN LLVM_CONFIG=llvm-config-3.7 pip install -q beautifulsoup4 blosc boto bottleneck cppimport cufflinks cvxpy deap gmpy graphviz h5py html5lib ipyparallel jupyter keras llvmlite lxml matplotlib netCDF4 networkx nose numba numexpr nltk pandas pandas-datareader Pillow plotly https://github.com/quantopian/pyfolio/archive/master.zip pygraphviz pykalman https://github.com/pymc-devs/pymc3/archive/master.zip qgrid quandl requests[security] seaborn scikit-image scikit-learn scipy scoop scrapy sqlalchemy statsmodels sympy tables Theano xarray xlrd xlwt https://github.com/quantopian/zipline/archive/master.zip
ENV THEANORC="/etc/theanorc:~/.theanorc"
COPY theanorc /etc/theanorc
RUN jupyter nbextension enable --py widgetsnbextension
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
