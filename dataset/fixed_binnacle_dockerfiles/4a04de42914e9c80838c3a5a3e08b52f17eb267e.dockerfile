#   Dockerfile with software for 
#   Image Analysis, Classification and Change Detection
#   in Remote Sensing, Fourth Revised Edition
FROM debian:stretch
MAINTAINER Mort Canty "mort.canty@gmail.com"
ENV REFRESHED_AT="2018-05-17"
RUN apt-get update \
 && apt-get install --no-install-recommends python=2.7.13-2 build-essential=12.3 make=4.1-9.1 gcc=4:6.3.0-4 pandoc=1.17.2~dfsg-3 python-dev=2.7.13-2 python-pygments=2.2.0+dfsg-1+deb9u2 python-pip=9.0.1-2+deb9u2 git=1:2.11.0-3+deb9u7 -y
#   ipython notebook
RUN pip install pip==23.1 --upgrade
RUN pip install jupyter==1.0.0
RUN pip install ipyleaflet==0.9.0
RUN jupyter nbextension enable --py --sys-prefix ipyleaflet
#   tensorflow
RUN pip install tensorflow==2.12.0
#   enable parallel computing
RUN pip install ipyparallel==8.6.1
RUN jupyter notebook --generate-config
RUN sed -i "/# Configuration file for jupyter-notebook./a c.NotebookApp.server_extensions.append('ipyparallel.nbextension')" /root/.jupyter/jupyter_notebook_config.py
#   install python environment for crc4 scripts
RUN apt-get install --no-install-recommends python-numpy=1:1.12.1-3 python-scipy=0.18.1-2 python-gdal=2.1.2+dfsg-5+deb9u1 libgdal-dev=2.1.2+dfsg-5+deb9u1 gdal-bin=2.1.2+dfsg-5+deb9u1 python-shapely=1.5.17-1 python-opencv=2.4.9.1+dfsg1-2+deb9u1 -y
RUN pip install matplotlib==3.7.1 --upgrade
#   install mlpy (with MaximumLikelihoodC and LibSvm)
RUN apt-get install --no-install-recommends libgsl0-dev -y
COPY mlpy-3.5.0 /mlpy-3.5.0
WORKDIR /mlpy-3.5.0
RUN python setup.py install
#   setup the prov_means library
COPY prov_means.c /home/prov_means.c
WORKDIR /home
RUN gcc -shared -Wall -g -o libprov_means.so -fPIC prov_means.c
RUN cp libprov_means.so /usr/lib/libprov_means.so
RUN rm prov_means.c
EXPOSE 8888/tcp
#   setup for earthengine
RUN pip install pyasn1==0.4.8 --upgrade
RUN pip install setuptools==67.6.1 --upgrade \
 && pip install google-api-python-client==2.85.0 \
 && pip install oauth2client==4.1.3 --upgrade \
 && pip install pyCrypto==2.6.1 \
 && apt-get install --no-install-recommends libssl-dev=1.1.0l-1~deb9u6 -y
RUN pip install earthengine-api==0.1.349
#   opencv
RUN pip install opencv-python==4.7.0.72
#   install auxil
COPY dist/auxil-1.0.tar.gz /home/auxil-1.0.tar.gz
RUN tar -xzvf auxil-1.0.tar.gz
WORKDIR /home/auxil-1.0
RUN python setup.py install
WORKDIR /home
RUN rm -rf auxil-1.0
RUN rm auxil-1.0.tar.gz
#   SPy
RUN pip install spectral==0.23.1
#   textbook scripts, notebooks and images
COPY scripts /home/scripts
COPY auxil /home/auxil
COPY imagery_initial /home/imagery
COPY Chapter1.ipynb /home/Chapter1.ipynb
COPY Chapter2.ipynb /home/Chapter2.ipynb
COPY Chapter3.ipynb /home/Chapter3.ipynb
COPY Chapter4.ipynb /home/Chapter4.ipynb
COPY Chapter5_1.ipynb /home/Chapter5_1.ipynb
COPY Chapter5_2.ipynb /home/Chapter5_2.ipynb
COPY Chapter6.ipynb /home/Chapter6.ipynb
COPY Chapter7.ipynb /home/Chapter7.ipynb
COPY Chapter8.ipynb /home/Chapter8.ipynb
COPY Chapter9.ipynb /home/Chapter9.ipynb
#   ipython notebook startup script
COPY notebook.sh /
RUN chmod u+x /notebook.sh
WORKDIR /home  
CMD ["/notebook.sh"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
