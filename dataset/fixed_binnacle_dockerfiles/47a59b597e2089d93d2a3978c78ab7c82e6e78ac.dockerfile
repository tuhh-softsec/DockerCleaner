FROM ubuntu:18.04
USER root
#   install Ubuntu packages
RUN apt-get update \
 && apt-get install --no-install-recommends autoconf=2.69-11 autogen=1:5.18.12-4 automake=1:1.15.1-3ubuntu2 autotools-dev=20180224.1 ca-certificates=20211016ubuntu0.18.04.1 g++=4:7.4.0-1ubuntu2.3 git=1:2.17.1-1ubuntu0.17 less=487-0.1 libtool=2.4.6-2 make=4.1-9.1ubuntu1 nano=2.9.3-2 pkg-config=0.29.1-0ubuntu2 python=2.7.15~rc1-1 unzip=6.0-21ubuntu1.2 wget=1.19.4-1ubuntu2.2 zlib1g-dev=1:1.2.11.dfsg-0ubuntu2.2 -y \
 && rm -rf /var/lib/apt/lists/*
#   copy helper scripts
WORKDIR /opt/cp2k-toolchain
RUN mkdir scripts
COPY ./scripts/VERSION ./scripts/parse_if.py ./scripts/tool_kit.sh ./scripts/common_vars.sh ./scripts/signal_trap.sh ./scripts/
COPY ./install_cp2k_toolchain.sh .
RUN ./install_cp2k_toolchain.sh --install-all --dry-run
COPY ./scripts/install_valgrind.sh ./scripts/
RUN ./scripts/install_valgrind.sh \
 && rm -rf ./build
COPY ./scripts/install_cmake.sh ./scripts/
RUN ./scripts/install_cmake.sh \
 && rm -rf ./build
COPY ./scripts/install_gcc.sh ./scripts/
RUN ./scripts/install_gcc.sh \
 && rm -rf ./build
COPY ./scripts/get_openblas_arch.sh ./scripts/
COPY ./scripts/setup_buildtools.sh ./scripts/
RUN ./scripts/setup_buildtools.sh \
 && rm -rf ./build
COPY ./scripts/install_mpich.sh ./scripts/
RUN ./scripts/install_mpich.sh \
 && rm -rf ./build
COPY ./scripts/install_openmpi.sh ./scripts/
RUN ./scripts/install_openmpi.sh \
 && rm -rf ./build
COPY ./scripts/install_reflapack.sh ./scripts/install_mkl.sh ./scripts/install_acml.sh ./scripts/install_openblas.sh ./scripts/install_mathlibs.sh ./scripts/
RUN ./scripts/install_mathlibs.sh \
 && rm -rf ./build
COPY ./scripts/install_fftw.sh ./scripts/
RUN ./scripts/install_fftw.sh \
 && rm -rf ./build
COPY ./scripts/install_libint.sh ./scripts/
RUN ./scripts/install_libint.sh \
 && rm -rf ./build
COPY ./scripts/install_libxc.sh ./scripts/
RUN ./scripts/install_libxc.sh \
 && rm -rf ./build
COPY ./scripts/install_libsmm.sh ./scripts/
RUN ./scripts/install_libsmm.sh \
 && rm -rf ./build
COPY ./scripts/install_libxsmm.sh ./scripts/
RUN ./scripts/install_libxsmm.sh \
 && rm -rf ./build
COPY ./scripts/install_scalapack.sh ./scripts/
RUN ./scripts/install_scalapack.sh \
 && rm -rf ./build
COPY ./scripts/install_elpa.sh ./scripts/
RUN ./scripts/install_elpa.sh \
 && rm -rf ./build
COPY ./scripts/install_ptscotch.sh ./scripts/
RUN ./scripts/install_ptscotch.sh \
 && rm -rf ./build
COPY ./scripts/install_parmetis.sh ./scripts/
RUN ./scripts/install_parmetis.sh \
 && rm -rf ./build
COPY ./scripts/install_metis.sh ./scripts/
RUN ./scripts/install_metis.sh \
 && rm -rf ./build
COPY ./scripts/install_superlu.sh ./scripts/
RUN ./scripts/install_superlu.sh \
 && rm -rf ./build
COPY ./scripts/install_pexsi.sh ./scripts/
RUN ./scripts/install_pexsi.sh \
 && rm -rf ./build
COPY ./scripts/install_quip.sh ./scripts/
RUN ./scripts/install_quip.sh \
 && rm -rf ./build
COPY ./scripts/install_gsl.sh ./scripts/
RUN ./scripts/install_gsl.sh \
 && rm -rf ./build
COPY ./scripts/install_spglib.sh ./scripts/
RUN ./scripts/install_spglib.sh \
 && rm -rf ./build
COPY ./scripts/install_hdf5.sh ./scripts/
RUN ./scripts/install_hdf5.sh \
 && rm -rf ./build
COPY ./scripts/install_libvdwxc.sh ./scripts/
RUN ./scripts/install_libvdwxc.sh \
 && rm -rf ./build
COPY ./scripts/install_sirius.sh ./scripts/
RUN ./scripts/install_sirius.sh \
 && rm -rf ./build
COPY ./scripts/install_json_fortran.sh ./scripts/
RUN ./scripts/install_json_fortran.sh \
 && rm -rf ./build
COPY ./scripts/arch_base.tmpl ./scripts/generate_arch_files.sh ./scripts/
RUN ./scripts/generate_arch_files.sh \
 && rm -rf ./build
#  EOF
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
