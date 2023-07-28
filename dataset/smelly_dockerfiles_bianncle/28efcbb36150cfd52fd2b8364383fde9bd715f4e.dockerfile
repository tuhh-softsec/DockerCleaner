FROM ubuntu:15.10
ENV PATH="/usr/games:$PATH  "
ADD . /usr/local/share/quine-relay
WORKDIR /usr/local/share/quine-relay  
RUN apt-get update \
 && apt-get upgrade -y
RUN apt-get install afnix -qq -y \
 && apt-get clean
RUN apt-get install algol68g -qq -y \
 && apt-get clean
RUN apt-get install aplus-fsf -qq -y \
 && apt-get clean
RUN apt-get install asymptote -qq -y \
 && apt-get clean
RUN apt-get install ats-lang-anairiats -qq -y \
 && apt-get clean
RUN apt-get install bash -qq -y \
 && apt-get clean
RUN apt-get install bc -qq -y \
 && apt-get clean
RUN apt-get install bf -qq -y \
 && apt-get clean
RUN apt-get install bsdgames -qq -y \
 && apt-get clean
RUN apt-get install cduce -qq -y \
 && apt-get clean
RUN apt-get install clisp -qq -y \
 && apt-get clean
RUN apt-get install clojure1.6 -qq -y \
 && apt-get clean
RUN apt-get install cmake -qq -y \
 && apt-get clean
RUN apt-get install coffeescript -qq -y \
 && apt-get clean
RUN apt-get install dc -qq -y \
 && apt-get clean
RUN apt-get install ecere-dev -qq -y \
 && apt-get clean
RUN apt-get install elixir -qq -y \
 && apt-get clean
RUN apt-get install emacs24 -qq -y \
 && apt-get clean
RUN apt-get install erlang -qq -y \
 && apt-get clean
RUN apt-get install f2c -qq -y \
 && apt-get clean
RUN apt-get install fp-compiler -qq -y \
 && apt-get clean
RUN apt-get install fsharp -qq -y \
 && apt-get clean
RUN apt-get install g++ -qq -y \
 && apt-get clean
RUN apt-get install gambas3-script -qq -y \
 && apt-get clean
RUN apt-get install gap -qq -y \
 && apt-get clean
RUN apt-get install gauche -qq -y \
 && apt-get clean
RUN apt-get install gawk -qq -y \
 && apt-get clean
RUN apt-get install gcc -qq -y \
 && apt-get clean
RUN apt-get install gdc -qq -y \
 && apt-get clean
RUN apt-get install genius -qq -y \
 && apt-get clean
RUN apt-get install gforth -qq -y \
 && apt-get clean
RUN apt-get install gfortran -qq -y \
 && apt-get clean
RUN apt-get install ghc -qq -y \
 && apt-get clean
RUN apt-get install ghostscript -qq -y \
 && apt-get clean
RUN apt-get install gnat -qq -y \
 && apt-get clean
RUN apt-get install gnu-smalltalk -qq -y \
 && apt-get clean
RUN apt-get install gnuplot -qq -y \
 && apt-get clean
RUN apt-get install gobjc -qq -y \
 && apt-get clean
RUN apt-get install golang -qq -y \
 && apt-get clean
RUN apt-get install gpt -qq -y \
 && apt-get clean
RUN apt-get install gri -qq -y \
 && apt-get clean
RUN apt-get install groff -qq -y \
 && apt-get clean
RUN apt-get install groovy -qq -y \
 && apt-get clean
RUN apt-get install haxe -qq -y \
 && apt-get clean
RUN apt-get install icont -qq -y \
 && apt-get clean
RUN apt-get install iconx -qq -y \
 && apt-get clean
RUN apt-get install intercal -qq -y \
 && apt-get clean
RUN apt-get install iverilog -qq -y \
 && apt-get clean
RUN apt-get install jasmin-sable -qq -y \
 && apt-get clean
RUN apt-get install jq -qq -y \
 && apt-get clean
RUN apt-get install julia -qq -y \
 && apt-get clean
RUN apt-get install libgd-dev -qq -y \
 && apt-get clean
RUN apt-get install libpng12-dev -qq -y \
 && apt-get clean
RUN apt-get install lisaac -qq -y \
 && apt-get clean
RUN apt-get install llvm -qq -y \
 && apt-get clean
RUN apt-get install lua5.3 -qq -y \
 && apt-get clean
RUN apt-get install make -qq -y \
 && apt-get clean
RUN apt-get install maxima -qq -y \
 && apt-get clean
RUN apt-get install mlton -qq -y \
 && apt-get clean
RUN apt-get install mono-devel -qq -y \
 && apt-get clean
RUN apt-get install mono-mcs -qq -y \
 && apt-get clean
RUN apt-get install mono-vbnc -qq -y \
 && apt-get clean
RUN apt-get install nasm -qq -y \
 && apt-get clean
RUN apt-get install neko -qq -y \
 && apt-get clean
RUN apt-get install nickle -qq -y \
 && apt-get clean
RUN apt-get install nim -qq -y \
 && apt-get clean
RUN apt-get install ocaml -qq -y \
 && apt-get clean
RUN apt-get install octave -qq -y \
 && apt-get clean
RUN apt-get install open-cobol -qq -y \
 && apt-get clean
RUN apt-get install openjdk-6-jdk -qq -y \
 && apt-get clean
RUN apt-get install pari-gp -qq -y \
 && apt-get clean
RUN apt-get install parrot -qq -y \
 && apt-get clean
RUN apt-get install perl -qq -y \
 && apt-get clean
RUN apt-get install php5-cli -qq -y \
 && apt-get clean
RUN apt-get install pike8.0 -qq -y \
 && apt-get clean
RUN apt-get install python -qq -y \
 && apt-get clean
RUN apt-get install r-base -qq -y \
 && apt-get clean
RUN apt-get install ratfor -qq -y \
 && apt-get clean
RUN apt-get install regina-rexx -qq -y \
 && apt-get clean
RUN apt-get install rhino -qq -y \
 && apt-get clean
RUN apt-get install ruby2.1 -qq -y \
 && apt-get clean
RUN apt-get install scala -qq -y \
 && apt-get clean
RUN apt-get install scilab -qq -y \
 && apt-get clean
RUN apt-get install slsh -qq -y \
 && apt-get clean
RUN apt-get install spl-core -qq -y \
 && apt-get clean
RUN apt-get install swi-prolog -qq -y \
 && apt-get clean
RUN apt-get install tcl -qq -y \
 && apt-get clean
RUN apt-get install ucblogo -qq -y \
 && apt-get clean
RUN apt-get install valac -qq -y \
 && apt-get clean
RUN apt-get install xsltproc -qq -y \
 && apt-get clean
RUN apt-get install yorick -qq -y \
 && apt-get clean
RUN apt-get install zoem -qq -y \
 && apt-get clean
RUN make -C vendor
CMD make check
