FROM lablup/kernel-base:jail AS jail-builder
FROM lablup/kernel-base:hook AS hook-builder
FROM lablup/kernel-base:python3.6 AS python-binary
FROM nvidia/cuda:8.0-cudnn7-runtime-ubuntu16.04
ENV LANG="C.UTF-8"
ENV PYTHONUNBUFFERED="1"
MAINTAINER Mario Cho "m.cho@lablup.com"
RUN apt-get update -y \
 && apt-get install -y ca-certificates wget libexpat1 libgdbm3 libbz2-dev libffi6 libsqlite3-0 liblzma5 zlib1g libmpdec2 libssl1.0.0 libssl-dev libncursesw5 libtinfo5 libreadline6 proj-bin libgeos-dev mime-support gcc g++ libproj-dev libgeos-dev libzmq3-dev libuv1
COPY --from=python-binary /python.tar.gz /
RUN cd / ; tar xzpf python.tar.gz ; rm python.tar.gz ; ldconfig
RUN export LD_LIBRARY_PATH=/usr/local/ssl/lib:$LD_LIBRARY_PATH
#  Test if Python is working
RUN python -c 'import sys; print(sys.version_info); import ssl'
#  Install CUDA-8.0 + cuDNN 7.3.1
RUN ln -s /usr/local/cuda-8.0 /usr/local/cuda \
 && ln -s /usr/lib/x86_64-linux-gnu/libcudnn.so.7.2.1 /usr/local/cuda/lib64/libcudnn.so \
 && ldconfig
ENV LD_LIBRARY_PATH="/usr/local/cuda/lib64:/usr/local/nvidia/lib64" \
    PATH="/usr/local/nvidia/bin:/usr/local/cuda/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin"
#  Prepare for building PyTorch wheel
RUN pip install wheel --no-cache-dir \
 && pip install pyzmq simplejson msgpack-python uvloop --no-cache-dir \
 && pip install aiozmq dataclasses tabulate namedlist six "python-dateutil>=2" --no-cache-dir \
 && pip install h5py --no-cache-dir \
 && pip install Cython --no-cache-dir \
 && pip install matplotlib bokeh --no-cache-dir \
 && pip install pyproj --no-cache-dir \
 && pip install Cartopy --no-cache-dir \
 && pip install torchvision --no-cache-dir \
 && pip install keras --no-cache-dir \
 && pip install ipython --no-cache-dir \
 && pip install pandas --no-cache-dir \
 && pip install seaborn --no-cache-dir \
 && pip install pillow --no-cache-dir \
 && pip install networkx cvxpy --no-cache-dir \
 && pip install scikit-learn scikit-image --no-cache-dir \
 && pip install scikit-image --no-cache-dir \
 && pip install pygments --no-cache-dir \
 && pip install jupyter --no-cache-dir \
 && rm -f /tmp/*.whl
#  Install PyTorch
RUN pip install https://download.pytorch.org/whl/cu80/torch-0.1.12.post1-cp36-cp36m-linux_x86_64.whl --no-cache-dir \
 && rm -rf /root/.cache
RUN apt-get install libseccomp2 gosu -y \
 && apt-get clean \
 && rm -r /var/lib/apt/lists /var/cache/apt/archives \
 && ln -s /usr/sbin/gosu /usr/sbin/su-exec \
 && mkdir /home/work \
 && chmod 755 /home/work ; mkdir /home/backend.ai \
 && chmod 755 /home/backend.ai
ADD entrypoint.sh /usr/local/bin/entrypoint.sh
RUN chmod +x /usr/local/bin/entrypoint.sh
ENTRYPOINT ["/usr/local/bin/entrypoint.sh"]
COPY policy.yml /home/backend.ai/policy.yml
#  Install jail
COPY --from=jail-builder /go/src/github.com/lablup/backend.ai-jail/backend.ai-jail /home/backend.ai/jail
COPY --from=hook-builder /root/backend.ai-hook/libbaihook.so /home/backend.ai/libbaihook.so
ENV LD_PRELOAD="/home/backend.ai/libbaihook.so"
#  Install kernel-runner scripts package
RUN pip install "backend.ai-kernel-runner[python]~=1.4.0" --no-cache-dir
#  Matplotlib configuration and pre-heating
ENV MPLCONFIGDIR="/home/backend.ai/.matplotlib"
RUN mkdir /home/backend.ai/.matplotlib
COPY matplotlibrc /home/backend.ai/.matplotlib/
RUN echo 'import matplotlib.pyplot' > /tmp/matplotlib-fontcache.py \
 && python /tmp/matplotlib-fontcache.py \
 && rm /tmp/matplotlib-fontcache.py
WORKDIR /home/work
VOLUME ["/home/work"]
EXPOSE 2000/tcp 2001/tcp 2002/tcp 2003/tcp
LABEL ai.backend.nvidia.enabled="yes" \
      com.nvidia.cuda.version="8.0.61" \
      com.nvidia.volumes.needed="nvidia_driver" \
      ai.backend.timeout="0" \
      ai.backend.maxmem="8g" \
      ai.backend.maxcores="4" \
      ai.backend.envs.corecount="OPENBLAS_NUM_THREADS,OMP_NUM_THREADS,NPROC" \
      ai.backend.features="batch query uid-match user-input" \
      ai.backend.service-ports="ipython:pty:3000,jupyter:http:8080"
CMD ["/home/backend.ai/jail", "-policy", "/home/backend.ai/policy.yml", "/usr/local/bin/python", "-m", "ai.backend.kernel", "python"]
#  vim: ft=dockerfile sts=4 sw=4 et tw=0
