FROM w251/tensorflow:dev-tx2-4.2_b158-py3
#   FROM tensorflow
#  + numpy
#  +  nltk==3.2.5
#   resampy
#  +  python_speech_features
#  + pandas==0.23.0
#   + six
#   + mpi4py
#   librosa==0.6.1
#  + matplotlib
#   + joblib==0.11
#  +  sentencepiece
#   + sacrebleu
#   + h5py
#   + tqdm
#   Need python 3..
RUN :
#   RUN apt-get install -y git
#   RUN apt-get install -y libfreetype6-dev pkg-config
ENV DEBIAN_FRONTEND="noninteractive"
#   numpy, pandas, matplotlib
RUN (apt-get update ;apt-get install --no-install-recommends python3-numpy python3-scipy python3-matplotlib python3-pandas python3-nose python3-sympy -y )
#   nltk
RUN pip3 install nltk==3.2.5
WORKDIR /tmp
#   sentencepiece
RUN (apt-get update ;apt-get install --no-install-recommends cmake build-essential pkg-config libgoogle-perftools-dev -y )
RUN git clone https://github.com/google/sentencepiece
#   google switched to Bazel for building but we haven't
WORKDIR /tmp/sentencepiece
RUN git checkout tags/v0.1.82
RUN mkdir build \
 && cd build \
 && cmake .. \
 && make -j 6 \
 && make install
RUN ldconfig -v
WORKDIR /tmp/sentencepiece/python
RUN python3 setup.py build
RUN python3 setup.py install
RUN rm -fr /tmp/sentencepiece
RUN pip3 install python_speech_features
RUN pip3 install tqdm
RUN (apt-get update ;apt-get install --no-install-recommends libhdf5-dev -y )
RUN pip3 install h5py
RUN pip3 install joblib==0.11
RUN pip3 install sacrebleu
RUN pip3 install six
RUN (apt-get update ;apt-get install --no-install-recommends libopenmpi-dev -y )
RUN pip3 install mpi4py
#   RUN pip3 install resampy
RUN (apt-get update ;apt-get install --no-install-recommends wget xz-utils -y )
WORKDIR /tmp
RUN wget http://releases.llvm.org/7.0.1/llvm-7.0.1.src.tar.xz
RUN tar Jxvf *.xz
RUN git clone https://github.com/numba/llvmlite.git
WORKDIR /tmp/llvm-7.0.1.src
RUN mkdir build
WORKDIR /tmp/llvm-7.0.1.src/build
RUN cmake .. -DCMAKE_BUILD_TYPE=Release -DLLVM_TARGETS_TO_BUILD="ARM;X86;AArch64" -"DCMAKE_C_FLAGS=-mcpu=cortex-a57"
RUN make -j6
RUN make install
WORKDIR /tmp/llvmlite
RUN python3 setup.py build
RUN python3 setup.py install
RUN (apt-get update ;apt-get install --no-install-recommends python3-sklearn -y )
RUN pip3 install numba
RUN pip3 install resampy
RUN pip3 install librosa==0.6.1
RUN rm -rf /tmp/llvmlite
RUN rm -rf /tmp/llvm-7.0.1.src
RUN rm -rf /tmp/llvm-7.0.1.src.tar.xz
WORKDIR /
RUN git clone https://github.com/NVIDIA/OpenSeq2Seq
WORKDIR /OpenSeq2Seq
#   unit tests
#   RUN python3 -m unittest discover -s open_seq2seq -p '*_test.py'
WORKDIR /
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
