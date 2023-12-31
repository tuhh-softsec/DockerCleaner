FROM ocaml/opam:ubuntu-16.04_ocaml-4.04.2
RUN cd /home/opam/opam-repository \
 && git pull origin master \
 && opam update
RUN opam pin add no-camlp4 https://github.com/avsm/no-camlp4.git
RUN opam depext -uivj 3 no-camlp4 alcotest angstrom anycache arp asn1-combinators astring atd atdgen base base64 camlzip cmdliner cohttp conduit core_kernel cow cowabloga cpuid crunch cstruct.3.0.2 ctypes ctypes-foreign datakit datakit-ci depyt dns dockerfile duration ezjsonm ezxmlm functoria git github gmp-freestanding hex hkdf hvsock integers io-page ipaddr irc-client irmin-watcher jekyll-format jenga jsonm logs logs-syslog lru-cache lwt magic-mime merlin mirage mirage-block mirage-block-lwt mirage-block-ramdisk mirage-block-solo5 mirage-block-unix mirage-block-xen mirage-bootvar-solo5 mirage-bootvar-xen mirage-btrees mirage-channel mirage-clock-unix mirage-clock-freestanding mirage-console-solo5 mirage-console-unix mirage-console-xen mirage-console-xen-backend mirage-console-xen-cli mirage-console-xen-proto mirage-device mirage-dns mirage-entropy mirage-flow mirage-flow-lwt mirage-flow-unix mirage-flow-rawlink mirage-fs-unix mirage-http mirage-logs mirage-net-flow mirage-net-solo5 mirage-net-unix mirage-net-xen mirage-os-shim mirage-random mirage-solo5 mirage-unix mirage-vnetif mirage-xen nocrypto obytelib ocaml-freestanding ocamlclean ocb-stubblr ocplib-endian odig opam-file-format otr parse-argv pbkdf pcap-format ppx_expect protocol-9p ptime re randomconv rresult scrypt-kdf session shared-memory-ring solo5-kernel-virtio stdio syslog-message tar tcpip tls tyre tuntap uri uucp uuseg utop vchan webbrowser webmachine x509 xen-evtchn xen-gnt xenctrl yojson xenstore zarith-freestanding
#  to fix: dns-forward nbd qcow vhd-format ezirmin mirage-block-ccm git-mirage topkg-care git-unix irmin websocket charrua-core charrua-unix fat-filesystem ansi-parse tlstunnel notty
#  not relevant: owl tyxml-ppx
RUN opam install -yj4 odoc odig
#  RUN opam config exec -- odig ocamldoc --docdir-href ../_doc
RUN opam config exec -- odig odoc
EXPOSE 8080/tcp
ENTRYPOINT opam config exec -- cohttp-server-lwt /home/opam/.opam/4.04.2/var/cache/odig/odoc
