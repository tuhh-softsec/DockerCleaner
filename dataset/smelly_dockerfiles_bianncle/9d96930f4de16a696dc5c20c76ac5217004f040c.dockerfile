FROM fpco/stack-build:lts-9.6
RUN apt-get update \
 && apt-get install -y libpq-dev
RUN stack build --resolver=lts-9.6 --system-ghc Spock-0.12.0.0 Spock-core-0.12.0.0 Spock-worker-0.3.1.0 acid-state-0.14.3 aeson-1.1.2.0 aeson-pretty-0.8.5 async-2.1.1.1 attoparsec-0.13.1.0 attoparsec-iso8601-1.0.0.0 authenticate-oauth-1.6 base-compat-0.9.3 base-orphans-0.6 base16-bytestring-0.1.1.6 base64-bytestring-1.0.0.1 bifunctors-5.4.2 binary-0.8.3.0 blaze-builder-0.4.0.2 byteable-0.1.1 bytestring-0.10.8.1 bytestring-builder-0.10.8.1.0 cabal-doctest-1.0.2 case-insensitive-1.2.0.10 cereal-0.5.4.0 cmark-0.5.6 cmdargs-0.10.18 comonad-5.0.2 connection-0.2.8 containers-0.5.7.1 contravariant-1.4 cookie-0.4.2.1 cryptohash-0.11.9 cryptonite-0.23 data-default-0.7.1.1 deepseq-1.4.2.0 directory-1.3.0.0 distributive-0.5.3 dlist-0.8.0.3 either-4.4.1.1 errors-2.2.2 exceptions-0.8.3 expiring-cache-map-0.0.6.1 file-embed-0.0.10 free-4.12.4 hashable-1.2.6.1 haskell-src-meta-0.8.0.1 hreader-1.1.0 hset-2.2.0 http-api-data-0.3.7.1 http-client-0.5.7.0 http-client-tls-0.3.5.1 http-date-0.0.6.1 http-types-0.9.1 http2-1.6.3 inflections-0.3.0.0 integer-gmp-1.0.0.1 iproute-1.7.1 ixset-typed-0.3.1.1 kan-extensions-5.0.2 katip-0.5.0.2 lens-4.15.4 lens-aeson-1.0.2 lifted-base-0.2.3.11 list-t-1.0.0.1 lucid-2.9.8.1 microlens-th-0.4.1.1 mime-types-0.1.0.7 mmorph-1.0.9 monad-control-1.0.2.2 monad-logger-0.3.25.1 mtl-2.2.1 network-2.6.3.2 network-uri-2.6.1.0 old-time-1.1.0.3 parallel-3.2.1.1 postgresql-libpq-0.9.3.1 postgresql-query-3.3.0 postgresql-simple-0.5.3.0 postgresql-simple-migration-0.1.11.0 profunctors-5.2.1 protolude-0.1.10 psqueues-0.2.3.0 random-1.1 reflection-2.1.2 reroute-0.4.1.0 resource-pool-0.2.3.2 resourcet-1.1.9 safe-exceptions-0.1.6.0 safecopy-0.9.3.3 scientific-0.3.5.2 semigroupoids-5.2.1 simple-sendfile-0.2.25 snowball-1.0.0.1 stm-2.4.4.1 stm-containers-0.2.16 streaming-commons-0.1.17 string-conv-0.1.2 tagged-0.8.5 tagsoup-0.14.1 template-haskell-2.11.1.0 text-1.2.2.2 text-icu-0.7.0.1 th-abstraction-0.2.6.0 th-lift-0.7.7 th-lift-instances-0.1.11 time-1.6.0.1 time-locale-compat-0.1.1.3 tls-1.3.11 tls-session-manager-0.0.0.1 transformers-base-0.4.4 transformers-compat-0.5.1.4 unix-2.7.2.1 unix-compat-0.4.3.1 unordered-containers-0.2.8.0 uri-bytestring-0.2.3.3 uuid-types-1.0.3 vault-0.3.0.7 vector-0.12.0.1 wai-3.2.1.1 wai-middleware-static-0.8.1 warp-3.2.13 warp-tls-3.2.4 wreq-0.5.0.1
