FROM lmoresi/uw2-jupyter-hub-tested:latest
# FROM underworldcode/underworld2:latest
MAINTAINER MAINTAINER https://github.com/underworldcode/
#  Configure environment for singleuser
ENV SHELL="/bin/bash"
ENV NB_USER="jovyan"
ENV NB_UID="1000"
ENV HOME="/home/$NB_USER"
ENV LC_ALL="en_US.UTF-8"
ENV LANG="en_US.UTF-8"
ENV LANGUAGE="en_US.UTF-8"
#  Create jovyan user with UID=1000 and in the 'users' group
RUN useradd -m -s /bin/bash -N -u $NB_UID $NB_USER
USER $NB_USER
#  Setup work directory for backward-compatibility
RUN mkdir /home/$NB_USER/work
USER root
#  Install jupyterhub dependencies, including python3
RUN apt-get update -qq \
 && DEBIAN_FRONTEND=noninteractive apt-get install -yq --no-install-recommends npm nodejs-legacy python3 python3-pip python3-dev
RUN pip3 install 'jupyterhub==0.8.1' jupyterlab ipyparallel
RUN npm install configurable-http-proxy -g
#  Install Python 2 kernel spec globally to avoid permission problems when NB_UID
#  switching at runtime and to allow the notebook server running out of the root
#  environment to find it. Also, activate the python2 environment upon kernel
#  launch.
RUN python -m ipykernel install
#  Setup ipyparallel for mpi profile for the NBUSER?
USER $NB_USER
WORKDIR $HOME
RUN ipython2 profile create --parallel --profile=mpi \
 && echo "c.IPClusterEngines.engine_launcher_class = 'MPIEngineSetLauncher'" >> $HOME/.ipython/profile_mpi/ipcluster_config.py
#  Trust underworld notebooks
RUN find /workspace -name *.ipynb -print0 | xargs -0 jupyter trust
USER root
#  Copy contents of home directory into /workspace so it'll be copied into the
#  user's persistent home directory the first time they start their server.
RUN rsync -au "$HOME"/.ipython "$HOME"/.jupyter /workspace/
#  Reinstate the xvfb entrypoint, required for underworld viz, and use the
#  customised start command to set up the underworld notebooks in the jupyterhub
#  environment.
ENTRYPOINT ["/usr/local/bin/tini", "--", "underworld-entrypoint.sh"]
CMD ["start-notebook.sh"]
#  Add local files as late as possible to avoid cache busting. These are the
#  jupyter(hub) startup scripts and config file.
COPY start.sh /usr/local/bin/
COPY start-notebook.sh /usr/local/bin/
COPY start-singleuser.sh /usr/local/bin/
COPY underworld-entrypoint.sh /usr/local/bin/
COPY jupyter_notebook_config.py /etc/jupyter/
RUN chown -R $NB_USER:users /etc/jupyter/
#  Include a default landing page for the notebook. See the DEFAULT_LANDING
#  environment variable below for how to enable this option.
COPY index.html /workspace
#  Set the DEFAULT_LANDING environment variable to the notebook URL we want as
#  the landing page a user will see when they first open their server. We can use
#  any notebook URL minus the host and '/user/<username>' parts.
#
#  Setting the environment variable here hard codes the default value in any
#  image you build. You can override it in a derived Dockerfile, or by passing
#  the desired value to a container at runtime.
#  Use '/tree' to replicate the built-in behaviour of the notebook - the
#  directory browser starting in the user's home directory.
# ENV DEFAULT_LANDING /tree
#  Start in a specific directory deeper in the user's home directory.
# ENV DEFAULT_LANDING /tree/tutorials/ConvectionTutorial/Notebooks
#  Use the index.html we copied above as the landing page.
ENV DEFAULT_LANDING="/files/index.html"
#  Jump directly to a specific notebook.
# ENV DEFAULT_LANDING /notebooks/examples/1_01_Steady_State_Heat.ipynb
#  Open a file in the corresponding notebook viewer
# ENV DEFAULT_LANDING /view/<filename>
#  Open a file in the notebook editor
# ENV DEFAULT_LANDING /edit/install_guides/nci_raijin.sh
USER $NB_USER
