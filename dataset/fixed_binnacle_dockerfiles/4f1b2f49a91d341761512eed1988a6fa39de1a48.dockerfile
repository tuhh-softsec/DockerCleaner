FROM jupyterhub-user
MAINTAINER Thomas Krijnen <thomas@ifcopenshell.org>
USER root
RUN :
RUN (apt-get update ;apt-get install --no-install-recommends wget=1.21.3-1ubuntu1 git=1:2.39.2-1ubuntu1 cmake=3.25.1-1 build-essential=12.9ubuntu3 libgl1-mesa-dev=23.0.1-1ubuntu1 libfreetype6-dev=2.12.1+dfsg-4 swig=4.1.0-0.2 libglu1-mesa-dev=9.0.2-1.1 libzmq3-dev=4.3.4-6 libsqlite3-dev=3.40.1-1 libboost-all-dev=1.74.0.3ubuntu7 libicu-dev=72.1-3ubuntu1 python3-dev=3.11.2-1 -y )
#   OCE
WORKDIR /opt/build
RUN git clone https://github.com/aothms/oce
RUN mkdir oce/build \
 && mkdir oce/install
WORKDIR /opt/build/oce/build
RUN git checkout copy_headers
ENV CFLAGS="-fPIC"
ENV CXXFLAGS="-fPIC"
RUN cmake -DOCE_TESTING=OFF -DOCE_BUILD_SHARED_LIB=ON -DOCE_VISUALISATION=ON -DOCE_OCAF=OFF -DOCE_INSTALL_PREFIX=/opt/build/install/oce ..
RUN make -j4 install
#   IfcOpenShell
WORKDIR /opt/build
RUN git clone https://github.com/IfcOpenShell/IfcOpenShell
WORKDIR IfcOpenShell/build
RUN git checkout v0.6.0
RUN which wget &> /dev/null || (apt-get update ;apt-get install --no-install-recommends wget=1.20.3 ) ; wget --no-verbose --output-document /tmp/ifcopenshell_version.json https://api.github.com/repos/IfcOpenShell/IfcOpenShell/git/refs/heads/v0.6.0
RUN git pull
RUN cmake -DCOLLADA_SUPPORT=Off -DBUILD_EXAMPLES=Off -DIFCXML_SUPPORT=Off -DOCC_INCLUDE_DIR=/opt/build/install/oce/include/oce -DOCC_LIBRARY_DIR=/opt/build/install/oce/lib -DPYTHON_LIBRARY=/opt/conda/lib/libpython3.6m.so -DPYTHON_INCLUDE_DIR=/opt/conda/include/python3.6m -DPYTHON_EXECUTABLE=/opt/conda/bin/python ../cmake
RUN make -j4 install
#   pyOCC
WORKDIR /opt/build
RUN git clone https://github.com/aothms/pythonocc-core
WORKDIR /opt/build/pythonocc-core/build
RUN git checkout review/jupyter_render_improvements
RUN cmake -DOCE_INCLUDE_PATH=/opt/build/install/oce/include/oce -DOCE_LIB_PATH=/opt/build/install/oce/lib -DPYTHONOCC_WRAP_VISU=ON -DPYTHONOCC_WRAP_OCAF=OFF -DPYTHON_LIBRARY=/opt/conda/lib/libpython3.6m.so -DPYTHON_INCLUDE_DIR=/opt/conda/include/python3.6m -DPYTHON_EXECUTABLE=/opt/conda/bin/python ..
RUN make -j4 install
RUN echo "/opt/build/install/oce/lib" >> /etc/ld.so.conf.d/pyocc.conf
RUN ldconfig
RUN conda install -y matplotlib
RUN conda install -y -c conda-forge ipywidgets
#   pythreejs
WORKDIR /opt/build
RUN git clone https://github.com/aothms/pythreejs
WORKDIR /opt/build/pythreejs
RUN git checkout own_fixes
RUN chown -R jovyan .
USER jovyan
RUN /opt/conda/bin/pip install --user -e .
WORKDIR /opt/build/pythreejs/js
RUN npm run autogen
RUN npm run build:all
USER root
RUN jupyter nbextension install --py --symlink --sys-prefix pythreejs
RUN jupyter nbextension enable pythreejs --py --sys-prefix
#   populate workspace with examples
WORKDIR /opt/build
COPY examples/populate_workspace.sh /opt/build/populate_workspace.sh
RUN chmod +x /opt/build/populate_workspace.sh
RUN sed -e '38i/opt/build/populate_workspace.sh' -i /usr/local/bin/start-singleuser.sh
RUN git clone https://github.com/gatsoulis/py2ipynb
COPY examples/01_visualize.py /opt/examples/01_visualize.py
COPY examples/02_analyze.py /opt/examples/02_analyze.py
COPY examples/ifc_viewer.py /opt/examples/ifc_viewer.py
#   viewer optimizations
#   USER jovyan
#   RUN /opt/conda/bin/pip install --user --upgrade --pre pyzmq
#   USER root
#   # COPY optimize_traitlets.py /home/jovyan/.local/lib/python3.6/site-packages/optimize_traitlets.py
#   WORKDIR /opt/build
#   RUN git clone https://github.com/vidartf/ipytunnel
#   WORKDIR ipytunnel
#   RUN chown -R jovyan .
#   USER jovyan
#   RUN /opt/conda/bin/pip install --user -e .
#   USER root
#   RUN jupyter nbextension enable --py --sys-prefix ipytunnel
USER jovyan
WORKDIR /home/jovyan/work
# Please add your HEALTHCHECK here!!!
