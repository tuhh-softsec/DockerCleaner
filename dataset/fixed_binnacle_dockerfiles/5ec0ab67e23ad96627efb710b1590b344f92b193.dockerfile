#  ## Capnp compiler
FROM alpine:3.5 AS capnp
RUN mkdir -p /src
RUN apk update \
 && apk add autoconf=2.69-r0 automake=1.15-r0 libtool=2.4.6-r1 linux-headers=4.4.6-r1 git=2.11.3-r2 g++=6.2.1-r1 make=4.2.1-r0
RUN cd /src \
 && git clone https://github.com/sandstorm-io/capnproto.git
WORKDIR /src/capnproto/c++
RUN ./setup-autotools.sh
RUN autoreconf -i
RUN ./configure
RUN make -j6 check
RUN make install
RUN which capnp
#  ## SDK
#  FROM ocaml/opam@sha256:b42566186327141d715c212da3057942bd4cfa5503a87733d366835fa2ddf98d
FROM ocaml/opam:alpine-3.5_ocaml-4.05.0 AS sdk
COPY --from=capnp /usr/local/bin/capnp /usr/local/bin/
COPY --from=capnp /usr/local/bin/capnpc /usr/local/bin/
COPY --from=capnp /usr/local/lib/libcapnpc-0.7-dev.so /usr/local/lib/
COPY --from=capnp /usr/local/lib/libcapnp-0.7-dev.so /usr/local/lib/
COPY --from=capnp /usr/local/lib/libkj-0.7-dev.so /usr/local/lib/
COPY --from=capnp /usr/local/include/capnp /usr/local/include/capnp
RUN sudo mkdir -p /src
USER opam
WORKDIR /src
RUN git -C /home/opam/opam-repository fetch \
 && git -C /home/opam/opam-repository reset ac26509c --hard \
 && opam update
COPY sdk.opam /src
RUN sudo chown opam -R /src
RUN opam pin add sdk.local /src -n
RUN opam depext -y alcotest sdk
RUN opam install alcotest mtime
RUN opam install --deps sdk
RUN opam list
COPY ./sdk /src/
RUN sudo chown opam -R /src
RUN opam update sdk \
 && opam install sdk -t
#  ## dhcp-client
FROM sdk AS dhcp-client
#   charrua
COPY dhcp-client.opam /src
RUN sudo chown opam -R /src
RUN opam pin add dhcp-client /src -n
RUN opam install dhcp-client --deps
COPY ./dhcp-client /src/dhcp-client
RUN sudo chown opam -R /src
RUN opam config exec -- jbuilder build --dev -p dhcp-client
RUN sudo mkdir -p /out
RUN sudo cp /src/_build/default/dhcp-client/main.exe /out/dhcp-client
RUN sudo cp /src/_build/default/dhcp-client/main_eng.exe /out/dhcp-client-engine
RUN sudo cp /src/_build/default/dhcp-client/main_net.exe /out/dhcp-client-network
RUN sudo cp /src/_build/default/dhcp-client/main_act.exe /out/dhcp-client-actuator
#  ## One binary
FROM scratch
USER 0
COPY --from=dhcp-client /out/dhcp-client /
CMD ["/dhcp-client", "-vv"]
#  ## DHCP client engine
FROM scratch
USER 0
COPY --from=dhcp-client /out/dhcp-client-engine /
CMD ["/dhcp-client-engine", "-vv"]
#  ## DHCP network proxy
FROM scratch
USER 0
COPY --from=dhcp-client /out/dhcp-client-actuator /
CMD ["/dhcp-client-actuator", "-vv"]
#  ## Host actuator
FROM scratch
USER 0
COPY --from=dhcp-client /out/dhcp-client-actuator /
CMD ["/dhcp-client-actuator", "-vv"]
RUN addgroup -S docker-user ; adduser -S -G docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
