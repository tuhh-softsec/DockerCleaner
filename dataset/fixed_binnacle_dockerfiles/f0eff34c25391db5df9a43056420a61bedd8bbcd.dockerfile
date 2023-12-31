FROM jupyter/all-spark-notebook
LABEL maintainer="Nilesh Patil (@nilesh-patil)"
USER root
#   pre-requisites
RUN apt-get update -qq \
 && apt-get install --no-install-recommends apt-utils=2.6.0 software-properties-common=0.99.35 fonts-dejavu=2.37-6 tzdata=2022g-7ubuntu2 gfortran=4:12.2.0-3ubuntu1 gcc=4:12.2.0-3ubuntu1 -qq -y \
 && apt-get clean \
 && add-apt-repository universe \
 && rm -rf /var/lib/apt/lists/*
#   Julia dependencies
#   install Julia packages in /opt/julia instead of $HOME
ENV JULIA_DEPOT_PATH="/opt/julia"
ENV JULIA_PKGDIR="/opt/julia"
ENV JULIA_VERSION="1.0.2"
#   install Julia
RUN mkdir /opt/julia-${JULIA_VERSION} \
 && cd /tmp \
 && wget -q https://julialang-s3.julialang.org/bin/linux/x64/`echo ${JULIA_VERSION} | cut -d. -f 1,2 `/julia-${JULIA_VERSION}-linux-x86_64.tar.gz \
 && tar xzf julia-${JULIA_VERSION}-linux-x86_64.tar.gz -C /opt/julia-${JULIA_VERSION} --strip-components=1 \
 && rm /tmp/julia-${JULIA_VERSION}-linux-x86_64.tar.gz \
 && ln -fs /opt/julia-*/bin/julia /usr/local/bin/julia
#   Show Julia where conda libraries are \
RUN mkdir /etc/julia \
 && echo "push!(Libdl.DL_LOAD_PATH, \"$CONDA_DIR/lib\")" >> /etc/julia/juliarc.jl \
 && mkdir $JULIA_PKGDIR \
 && chown $NB_USER $JULIA_PKGDIR \
 && fix-permissions $JULIA_PKGDIR
USER $NB_UID
#  # Julia kernel & packages
RUN julia -e 'import Pkg; Pkg.update()' \
 && (test $TEST_ONLY_BUILD || julia -e 'import Pkg; Pkg.add("HDF5")' ) \
 && julia -e 'import Pkg; Pkg.add("Gadfly")' \
 && julia -e 'import Pkg; Pkg.add("RDatasets")' \
 && julia -e 'import Pkg; Pkg.add("IJulia")' \
 && julia -e 'using IJulia' \
 && mv $HOME/.local/share/jupyter/kernels/julia* $CONDA_DIR/share/jupyter/kernels/ \
 && chmod -R go+rx $CONDA_DIR/share/jupyter \
 && rm -rf $HOME/.local \
 && fix-permissions $JULIA_PKGDIR $CONDA_DIR/share/jupyter
#   Anaconda Python Environments
RUN conda create -n python27 python=2.7 anaconda \
 && conda create -n python36 python=3.6 anaconda
#  #################################
#  ### Add Packages to Base ########
#  #################################
RUN /bin/bash -c "source activate base \
 && conda install --quiet --yes -c anaconda numpy scipy scikit-learn scikit-image pandas tqdm ipykernel ; conda install --quiet --yes -c anaconda tensorflow ; conda install --quiet --yes -c pytorch pytorch-cpu torchvision-cpu ; conda install --quiet --yes -c conda-forge openpyxl h5py matplotlib ; conda upgrade --all -y ; pip install opencv-python imgaug; conda clean --all -y ; python -m ipykernel install --user --name base --display-name 'Python 3(Base)' \
 && fix-permissions $CONDA_DIR /home/$NB_USER \
 && source deactivate \
 && source activate python27 \
 && conda install --quiet --yes -c anaconda numpy scipy scikit-learn scikit-image pandas tqdm ipykernel \
 && conda install --quiet --yes -c anaconda tensorflow ; conda install --quiet --yes -c pytorch pytorch-cpu torchvision-cpu ; conda install --quiet --yes -c conda-forge openpyxl h5py matplotlib ; conda upgrade --all -y ; pip install opencv-python ; conda clean --all -y ; python -m ipykernel install --user --name python27 --display-name 'Python 2.7' \
 && fix-permissions $CONDA_DIR /home/$NB_USER \
 && source deactivate \
 && source activate python36 \
 && conda install --quiet --yes -c anaconda numpy scipy scikit-learn scikit-image pandas tqdm ipykernel \
 && conda install --quiet --yes -c anaconda tensorflow ; conda install --quiet --yes -c pytorch pytorch-cpu torchvision-cpu ; conda install --quiet --yes -c conda-forge openpyxl h5py matplotlib ; conda upgrade --all -y ; pip install opencv-python imgaug; conda clean --all -y ; python -m ipykernel install --user --name python36 --display-name 'Python 3.6' \
 && source deactivate \
 && mv $HOME/.local/share/jupyter/kernels/* $CONDA_DIR/share/jupyter/kernels/ \
 && chmod -R go+rx $CONDA_DIR/share/jupyter \
 && rm -rf $HOME/.local \
 && fix-permissions $CONDA_DIR /home/$NB_USER $CONDA_DIR/share/jupyter"
EXPOSE 8888/tcp 4040/tcp 8080/tcp 8081/tcp 7077/tcp
# Please add your HEALTHCHECK here!!!
