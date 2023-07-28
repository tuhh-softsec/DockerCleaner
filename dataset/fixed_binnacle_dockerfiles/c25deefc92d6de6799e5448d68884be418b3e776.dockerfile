FROM lablup/kernel-base:jail AS jail-builder
FROM lablup/kernel-base:hook AS hook-builder
FROM lablup/kernel-base:python3.6 AS python-binary
FROM ubuntu:16.04
MAINTAINER Mario Cho "m.cho@lablup.com"
ENV LANG="C.UTF-8"
ENV PYTHONUNBUFFERED="1"
RUN apt-get update -y \
 && apt-get install --no-install-recommends ca-certificates wget libexpat1 libgdbm3 libbz2-dev libffi6 libsqlite3-0 liblzma5 zlib1g libmpdec2 libssl1.0.0 libssl-dev libncursesw5 libtinfo5 libreadline6 proj-bin libgeos-dev mime-support gcc g++ libproj-dev libgeos-dev libzmq3-dev libuv1 -y
COPY --from=python-binary /python.tar.gz /
RUN cd / ; tar xzpf python.tar.gz ; rm python.tar.gz ; ldconfig
RUN export LD_LIBRARY_PATH=/usr/local/ssl/lib:$LD_LIBRARY_PATH
#   Test if Python is working
RUN python -c 'import sys; print(sys.version_info); import ssl'
#   Prepare for building Pytorch wheel
RUN pip install wheel==0.40.0 --no-cache-dir \
 && pip install pyzmq==25.0.2 simplejson==3.19.1 msgpack-python==0.5.6 uvloop==0.17.0 --no-cache-dir \
 && pip install aiozmq==1.0.0 dataclasses==0.8 tabulate==0.9.0 namedlist==1.8 six==1.16.0 "python-dateutil>=2" --no-cache-dir \
 && pip install h5py==3.8.0 --no-cache-dir \
 && pip install Cython==0.29.34 --no-cache-dir \
 && pip install matplotlib==3.7.1 bokeh==3.1.0 --no-cache-dir \
 && pip install pyproj==3.5.0 --no-cache-dir \
 && pip install Cartopy==0.21.1 --no-cache-dir \
 && pip install torchvision==0.15.1 --no-cache-dir \
 && pip install keras==2.12.0 --no-cache-dir \
 && pip install ipython==8.12.0 --no-cache-dir \
 && pip install pandas==2.0.0 --no-cache-dir \
 && pip install seaborn==0.12.2 --no-cache-dir \
 && pip install pillow==9.5.0 --no-cache-dir \
 && pip install networkx==3.1 cvxpy==1.3.1 --no-cache-dir \
 && pip install scikit-learn==1.2.2 scikit-image==0.20.0 --no-cache-dir \
 && pip install scikit-image==0.20.0 --no-cache-dir \
 && pip install pygments==2.15.0 --no-cache-dir \
 && pip install jupyter==1.0.0 --no-cache-dir \
 && rm -f /tmp/*.whl
#   Install Torch
RUN pip install https://download.pytorch.org/whl/cpu/torch-0.4.1-cp36-cp36m-linux_x86_64.whl pip==23.1 torchvision==0.15.1 rm==2020.12.3 /root/.cache --no-cache-dir --no-cache-dir -rf
RUN apt-get install --no-install-recommends libseccomp2 gosu -y \
 && apt-get clean \
 && rm -r /var/lib/apt/lists /var/cache/apt/archives \
 && ln -s /usr/sbin/gosu /usr/sbin/su-exec \
 && mkdir /home/work \
 && chmod 755 /home/work ; mkdir /home/backend.ai \
 && chmod 755 /home/backend.ai
COPY entrypoint.sh /usr/local/bin/entrypoint.sh
RUN chmod +x /usr/local/bin/entrypoint.sh
ENTRYPOINT ["/usr/local/bin/entrypoint.sh"]
COPY policy.yml /home/backend.ai/policy.yml
#   Install jail
COPY --from=jail-builder /go/src/github.com/lablup/backend.ai-jail/backend.ai-jail /home/backend.ai/jail
COPY --from=hook-builder /root/backend.ai-hook/libbaihook.so /home/backend.ai/libbaihook.so
ENV LD_PRELOAD="/home/backend.ai/libbaihook.so"
#   Install kernel-runner scripts package
RUN pip install "backend.ai-kernel-runner[python]~=1.4.0" --no-cache-dir
#   Matplotlib configuration and pre-heating
ENV MPLCONFIGDIR="/home/backend.ai/.matplotlib"
RUN mkdir /home/backend.ai/.matplotlib
COPY matplotlibrc /home/backend.ai/.matplotlib/
RUN echo 'import matplotlib.pyplot' > /tmp/matplotlib-fontcache.py \
 && python /tmp/matplotlib-fontcache.py \
 && rm /tmp/matplotlib-fontcache.py
WORKDIR /home/work
VOLUME ["/home/work"]
EXPOSE 2000/tcp 2001/tcp 2002/tcp 2003/tcp
LABEL ai.backend.timeout="0" \
      ai.backend.maxmem="8g" \
      ai.backend.maxcores="4" \
      ai.backend.envs.corecount="OPENBLAS_NUM_THREADS,OMP_NUM_THREADS,NPROC" \
      ai.backend.features="batch query uid-match user-input" \
      ai.backend.service-ports="ipython:pty:3000,jupyter:http:8080"
CMD ["/home/backend.ai/jail", "-policy", "/home/backend.ai/policy.yml", "/usr/local/bin/python", "-m", "ai.backend.kernel", "python"]
#   vim: ft=dockerfile sts=4 sw=4 et tw=0
#   Install Torch
RUN pip install torch==2.0.0 torchvision==0.15.1 --no-cache-dir \
 && rm -rf /root/.cache
#   vim: ft=dockerfile sts=4 sw=4 et tw=0
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
