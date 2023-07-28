FROM lablup/kernel-base:jail AS jail-builder
FROM lablup/kernel-base:hook AS hook-builder
FROM lablup/kernel-base:python3.6 AS python-binary
FROM lablup/common-tensorflow:1.11-py36 AS tf-binary
FROM ubuntu:16.04
MAINTAINER Mario Cho "m.cho@lablup.com"
ENV LANG="C.UTF-8"
ENV PYTHONUNBUFFERED="1"
RUN apt-get update -y \
 && apt-get install -y ca-certificates wget libexpat1 libgdbm3 libbz2-dev libffi6 libsqlite3-0 liblzma5 zlib1g libmpdec2 libssl1.0.0 libssl-dev libncursesw5 libtinfo5 libreadline6 proj-bin libgeos-dev mime-support gcc g++ libproj-dev libgeos-dev libzmq3-dev libuv1
#  Copy the whole Python from the docker library image
COPY --from=python-binary /python.tar.gz /
RUN cd / ; tar xzpf python.tar.gz ; rm python.tar.gz ; ldconfig
RUN export LD_LIBRARY_PATH=/usr/local/ssl/lib:$LD_LIBRARY_PATH
#  Test if Python is working
RUN python -c 'import sys; print(sys.version_info); import ssl'
#  As we mostly have "manylinux" glibc-compatible binary packaes,
COPY --from=tf-binary /tmp/tensorflow_pkg/tensorflow-*.whl /tmp
#  Install TensorFlow build dependencies (ensure we have proper numpy)
#  Prepare for building TensorFlow wheel
RUN pip install wheel --no-cache-dir \
 && pip install pyzmq simplejson msgpack-python uvloop --no-cache-dir \
 && pip install aiozmq dataclasses tabulate namedlist six "python-dateutil>=2" --no-cache-dir \
 && pip install h5py --no-cache-dir \
 && pip install Cython --no-cache-dir \
 && pip install matplotlib bokeh --no-cache-dir \
 && pip install pyproj --no-cache-dir \
 && pip install Cartopy --no-cache-dir \
 && pip install wheel /tmp/*.whl --no-cache-dir \
 && pip install keras --no-cache-dir \
 && pip install ipython --no-cache-dir \
 && pip install pandas --no-cache-dir \
 && pip install seaborn --no-cache-dir \
 && pip install pillow --no-cache-dir \
 && pip install networkx cvxpy --no-cache-dir \
 && pip install scikit-learn scikit-image --no-cache-dir \
 && pip install pygments --no-cache-dir \
 && rm -f /tmp/*.whl
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
#  For utilizing TPU cluster resolver
RUN pip install google-api-python-client --no-cache-dir \
 && pip install oauth2client --no-cache-dir
WORKDIR /home/work
VOLUME ["/home/work"]
EXPOSE 2000/tcp 2001/tcp 2002/tcp 2003/tcp
LABEL ai.backend.timeout="0" \
      ai.backend.maxmem="8g" \
      ai.backend.maxcores="4" \
      ai.backend.envs.corecount="OPENBLAS_NUM_THREADS,OMP_NUM_THREADS,NPROC" \
      ai.backend.features="batch query uid-match user-input"
CMD ["/home/backend.ai/jail", "-policy", "/home/backend.ai/policy.yml", "/usr/local/bin/python", "-m", "ai.backend.kernel", "python"]
#  vim: ft=dockerfile
