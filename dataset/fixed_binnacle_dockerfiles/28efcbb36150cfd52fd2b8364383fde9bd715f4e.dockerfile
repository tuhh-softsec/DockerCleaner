FROM ubuntu:15.10
ENV PATH="/usr/games:$PATH  "
COPY . /usr/local/share/quine-relay
WORKDIR /usr/local/share/quine-relay  
RUN : \
 && apt-get upgrade -y
RUN (apt-get update ;apt-get install --no-install-recommends afnix -qq -y ) \
 && apt-get clean
RUN (apt-get update ;apt-get install --no-install-recommends algol68g -qq -y ) \
 && apt-get clean
RUN (apt-get update ;apt-get install --no-install-recommends aplus-fsf -qq -y ) \
 && apt-get clean
RUN (apt-get update ;apt-get install --no-install-recommends asymptote -qq -y ) \
 && apt-get clean
RUN (apt-get update ;apt-get install --no-install-recommends ats-lang-anairiats -qq -y ) \
 && apt-get clean
RUN (apt-get update ;apt-get install --no-install-recommends bash -qq -y ) \
 && apt-get clean
RUN (apt-get update ;apt-get install --no-install-recommends bc -qq -y ) \
 && apt-get clean
RUN (apt-get update ;apt-get install --no-install-recommends bf -qq -y ) \
 && apt-get clean
RUN (apt-get update ;apt-get install --no-install-recommends bsdgames -qq -y ) \
 && apt-get clean
RUN (apt-get update ;apt-get install --no-install-recommends cduce -qq -y ) \
 && apt-get clean
RUN (apt-get update ;apt-get install --no-install-recommends clisp -qq -y ) \
 && apt-get clean
RUN (apt-get update ;apt-get install --no-install-recommends clojure1.6 -qq -y ) \
 && apt-get clean
RUN (apt-get update ;apt-get install --no-install-recommends cmake -qq -y ) \
 && apt-get clean
RUN (apt-get update ;apt-get install --no-install-recommends coffeescript -qq -y ) \
 && apt-get clean
RUN (apt-get update ;apt-get install --no-install-recommends dc -qq -y ) \
 && apt-get clean
RUN (apt-get update ;apt-get install --no-install-recommends ecere-dev -qq -y ) \
 && apt-get clean
RUN (apt-get update ;apt-get install --no-install-recommends elixir -qq -y ) \
 && apt-get clean
RUN (apt-get update ;apt-get install --no-install-recommends emacs24 -qq -y ) \
 && apt-get clean
RUN (apt-get update ;apt-get install --no-install-recommends erlang -qq -y ) \
 && apt-get clean
RUN (apt-get update ;apt-get install --no-install-recommends f2c -qq -y ) \
 && apt-get clean
RUN (apt-get update ;apt-get install --no-install-recommends fp-compiler -qq -y ) \
 && apt-get clean
RUN (apt-get update ;apt-get install --no-install-recommends fsharp -qq -y ) \
 && apt-get clean
RUN (apt-get update ;apt-get install --no-install-recommends g++ -qq -y ) \
 && apt-get clean
RUN (apt-get update ;apt-get install --no-install-recommends gambas3-script -qq -y ) \
 && apt-get clean
RUN (apt-get update ;apt-get install --no-install-recommends gap -qq -y ) \
 && apt-get clean
RUN (apt-get update ;apt-get install --no-install-recommends gauche -qq -y ) \
 && apt-get clean
RUN (apt-get update ;apt-get install --no-install-recommends gawk -qq -y ) \
 && apt-get clean
RUN (apt-get update ;apt-get install --no-install-recommends gcc -qq -y ) \
 && apt-get clean
RUN (apt-get update ;apt-get install --no-install-recommends gdc -qq -y ) \
 && apt-get clean
RUN (apt-get update ;apt-get install --no-install-recommends genius -qq -y ) \
 && apt-get clean
RUN (apt-get update ;apt-get install --no-install-recommends gforth -qq -y ) \
 && apt-get clean
RUN (apt-get update ;apt-get install --no-install-recommends gfortran -qq -y ) \
 && apt-get clean
RUN (apt-get update ;apt-get install --no-install-recommends ghc -qq -y ) \
 && apt-get clean
RUN (apt-get update ;apt-get install --no-install-recommends ghostscript -qq -y ) \
 && apt-get clean
RUN (apt-get update ;apt-get install --no-install-recommends gnat -qq -y ) \
 && apt-get clean
RUN (apt-get update ;apt-get install --no-install-recommends gnu-smalltalk -qq -y ) \
 && apt-get clean
RUN (apt-get update ;apt-get install --no-install-recommends gnuplot -qq -y ) \
 && apt-get clean
RUN (apt-get update ;apt-get install --no-install-recommends gobjc -qq -y ) \
 && apt-get clean
RUN (apt-get update ;apt-get install --no-install-recommends golang -qq -y ) \
 && apt-get clean
RUN (apt-get update ;apt-get install --no-install-recommends gpt -qq -y ) \
 && apt-get clean
RUN (apt-get update ;apt-get install --no-install-recommends gri -qq -y ) \
 && apt-get clean
RUN (apt-get update ;apt-get install --no-install-recommends groff -qq -y ) \
 && apt-get clean
RUN (apt-get update ;apt-get install --no-install-recommends groovy -qq -y ) \
 && apt-get clean
RUN (apt-get update ;apt-get install --no-install-recommends haxe -qq -y ) \
 && apt-get clean
RUN (apt-get update ;apt-get install --no-install-recommends icont -qq -y ) \
 && apt-get clean
RUN (apt-get update ;apt-get install --no-install-recommends iconx -qq -y ) \
 && apt-get clean
RUN (apt-get update ;apt-get install --no-install-recommends intercal -qq -y ) \
 && apt-get clean
RUN (apt-get update ;apt-get install --no-install-recommends iverilog -qq -y ) \
 && apt-get clean
RUN (apt-get update ;apt-get install --no-install-recommends jasmin-sable -qq -y ) \
 && apt-get clean
RUN (apt-get update ;apt-get install --no-install-recommends jq -qq -y ) \
 && apt-get clean
RUN (apt-get update ;apt-get install --no-install-recommends julia -qq -y ) \
 && apt-get clean
RUN (apt-get update ;apt-get install --no-install-recommends libgd-dev -qq -y ) \
 && apt-get clean
RUN (apt-get update ;apt-get install --no-install-recommends libpng12-dev -qq -y ) \
 && apt-get clean
RUN (apt-get update ;apt-get install --no-install-recommends lisaac -qq -y ) \
 && apt-get clean
RUN (apt-get update ;apt-get install --no-install-recommends llvm -qq -y ) \
 && apt-get clean
RUN (apt-get update ;apt-get install --no-install-recommends lua5.3 -qq -y ) \
 && apt-get clean
RUN (apt-get update ;apt-get install --no-install-recommends make -qq -y ) \
 && apt-get clean
RUN (apt-get update ;apt-get install --no-install-recommends maxima -qq -y ) \
 && apt-get clean
RUN (apt-get update ;apt-get install --no-install-recommends mlton -qq -y ) \
 && apt-get clean
RUN (apt-get update ;apt-get install --no-install-recommends mono-devel -qq -y ) \
 && apt-get clean
RUN (apt-get update ;apt-get install --no-install-recommends mono-mcs -qq -y ) \
 && apt-get clean
RUN (apt-get update ;apt-get install --no-install-recommends mono-vbnc -qq -y ) \
 && apt-get clean
RUN (apt-get update ;apt-get install --no-install-recommends nasm -qq -y ) \
 && apt-get clean
RUN (apt-get update ;apt-get install --no-install-recommends neko -qq -y ) \
 && apt-get clean
RUN (apt-get update ;apt-get install --no-install-recommends nickle -qq -y ) \
 && apt-get clean
RUN (apt-get update ;apt-get install --no-install-recommends nim -qq -y ) \
 && apt-get clean
RUN (apt-get update ;apt-get install --no-install-recommends ocaml -qq -y ) \
 && apt-get clean
RUN (apt-get update ;apt-get install --no-install-recommends octave -qq -y ) \
 && apt-get clean
RUN (apt-get update ;apt-get install --no-install-recommends open-cobol -qq -y ) \
 && apt-get clean
RUN (apt-get update ;apt-get install --no-install-recommends openjdk-6-jdk -qq -y ) \
 && apt-get clean
RUN (apt-get update ;apt-get install --no-install-recommends pari-gp -qq -y ) \
 && apt-get clean
RUN (apt-get update ;apt-get install --no-install-recommends parrot -qq -y ) \
 && apt-get clean
RUN (apt-get update ;apt-get install --no-install-recommends perl -qq -y ) \
 && apt-get clean
RUN (apt-get update ;apt-get install --no-install-recommends php5-cli -qq -y ) \
 && apt-get clean
RUN (apt-get update ;apt-get install --no-install-recommends pike8.0 -qq -y ) \
 && apt-get clean
RUN (apt-get update ;apt-get install --no-install-recommends python -qq -y ) \
 && apt-get clean
RUN (apt-get update ;apt-get install --no-install-recommends r-base -qq -y ) \
 && apt-get clean
RUN (apt-get update ;apt-get install --no-install-recommends ratfor -qq -y ) \
 && apt-get clean
RUN (apt-get update ;apt-get install --no-install-recommends regina-rexx -qq -y ) \
 && apt-get clean
RUN (apt-get update ;apt-get install --no-install-recommends rhino -qq -y ) \
 && apt-get clean
RUN (apt-get update ;apt-get install --no-install-recommends ruby2.1 -qq -y ) \
 && apt-get clean
RUN (apt-get update ;apt-get install --no-install-recommends scala -qq -y ) \
 && apt-get clean
RUN (apt-get update ;apt-get install --no-install-recommends scilab -qq -y ) \
 && apt-get clean
RUN (apt-get update ;apt-get install --no-install-recommends slsh -qq -y ) \
 && apt-get clean
RUN (apt-get update ;apt-get install --no-install-recommends spl-core -qq -y ) \
 && apt-get clean
RUN (apt-get update ;apt-get install --no-install-recommends swi-prolog -qq -y ) \
 && apt-get clean
RUN (apt-get update ;apt-get install --no-install-recommends tcl -qq -y ) \
 && apt-get clean
RUN (apt-get update ;apt-get install --no-install-recommends ucblogo -qq -y ) \
 && apt-get clean
RUN (apt-get update ;apt-get install --no-install-recommends valac -qq -y ) \
 && apt-get clean
RUN (apt-get update ;apt-get install --no-install-recommends xsltproc -qq -y ) \
 && apt-get clean
RUN (apt-get update ;apt-get install --no-install-recommends yorick -qq -y ) \
 && apt-get clean
RUN (apt-get update ;apt-get install --no-install-recommends zoem -qq -y ) \
 && apt-get clean
RUN make -C vendor
CMD make check
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
