#  NEO private network - Dockerfile
FROM microsoft/dotnet:2.1.4-runtime-bionic
LABEL maintainer="City of Zion"
LABEL authors="metachris, ashant, hal0x2328, phetter"
ENV DEBIAN_FRONTEND="noninteractive"
#  Disable dotnet usage information collection
#  https://docs.microsoft.com/en-us/dotnet/core/tools/telemetry#behavior
ENV DOTNET_CLI_TELEMETRY_OPTOUT="1"
#  Install system dependencies. always should be done in one line
#  https://docs.docker.com/engine/userguide/eng-image/dockerfile_best-practices/#run
RUN apt-get update \
 && apt-get install --no-install-recommends unzip screen expect libleveldb-dev git-core wget curl git-core python3.6 python3.6-dev python3.6-venv python3-pip libleveldb-dev libssl-dev vim man libunwind8 -y
#  APT cleanup to reduce image size
RUN rm -rf /var/lib/apt/lists/*
#  neo-python setup: clonse and install dependencies
RUN git clone https://github.com/CityOfZion/neo-python.git /neo-python
WORKDIR /neo-python
#  RUN git checkout development
RUN pip3 install -e .
RUN wget https://s3.amazonaws.com/neo-experiments/neo-privnet.wallet
#  Add the neo-cli package
COPY ./neo-cli.zip /opt/neo-cli.zip
COPY ./SimplePolicy.zip /opt/SimplePolicy.zip
COPY ./ApplicationLogs.zip /opt/ApplicationLogs.zip
#  Extract and prepare four consensus nodes
RUN unzip -q -d /opt/node1 /opt/neo-cli.zip
RUN unzip -q -d /opt/node2 /opt/neo-cli.zip
RUN unzip -q -d /opt/node3 /opt/neo-cli.zip
RUN unzip -q -d /opt/node4 /opt/neo-cli.zip
#  Extract and prepare SimplePolicy plugin
RUN unzip -q -d /opt/node1/neo-cli /opt/SimplePolicy.zip
RUN unzip -q -d /opt/node2/neo-cli /opt/SimplePolicy.zip
RUN unzip -q -d /opt/node3/neo-cli /opt/SimplePolicy.zip
RUN unzip -q -d /opt/node4/neo-cli /opt/SimplePolicy.zip
#  Extract and prepare SimplePolicy plugin
RUN unzip -q -d /opt/node1/neo-cli /opt/ApplicationLogs.zip
RUN unzip -q -d /opt/node2/neo-cli /opt/ApplicationLogs.zip
RUN unzip -q -d /opt/node3/neo-cli /opt/ApplicationLogs.zip
RUN unzip -q -d /opt/node4/neo-cli /opt/ApplicationLogs.zip
#  Remove zip neo-cli package
RUN rm /opt/neo-cli.zip
RUN rm /opt/SimplePolicy.zip
RUN rm /opt/ApplicationLogs.zip
#  Create chain data directories
RUN mkdir -p /opt/chaindata/node1
RUN mkdir -p /opt/chaindata/node2
RUN mkdir -p /opt/chaindata/node3
RUN mkdir -p /opt/chaindata/node4
#  Add config files
COPY ./configs/config1.json /opt/node1/neo-cli/config.json
COPY ./configs/config1.json /opt/node1/neo-cli/config.orig.json
COPY ./configs/protocol.json /opt/node1/neo-cli/protocol.json
COPY ./wallets/wallet1.json /opt/node1/neo-cli/
COPY ./configs/config-applicationlogs1.json /opt/node1/neo-cli/Plugins/ApplicationLogs/config.json
COPY ./configs/config-applicationlogs1.json /opt/node1/neo-cli/Plugins/ApplicationLogs/config.orig.json
COPY ./configs/config2.json /opt/node2/neo-cli/config.json
COPY ./configs/config2.json /opt/node2/neo-cli/config.orig.json
COPY ./configs/protocol.json /opt/node2/neo-cli/protocol.json
COPY ./wallets/wallet2.json /opt/node2/neo-cli/
COPY ./configs/config-applicationlogs2.json /opt/node2/neo-cli/Plugins/ApplicationLogs/config.json
COPY ./configs/config-applicationlogs2.json /opt/node2/neo-cli/Plugins/ApplicationLogs/config.orig.json
COPY ./configs/config3.json /opt/node3/neo-cli/config.json
COPY ./configs/config3.json /opt/node3/neo-cli/config.orig.json
COPY ./configs/protocol.json /opt/node3/neo-cli/protocol.json
COPY ./wallets/wallet3.json /opt/node3/neo-cli/
COPY ./configs/config-applicationlogs3.json /opt/node3/neo-cli/Plugins/ApplicationLogs/config.json
COPY ./configs/config-applicationlogs3.json /opt/node3/neo-cli/Plugins/ApplicationLogs/config.orig.json
COPY ./configs/config4.json /opt/node4/neo-cli/config.json
COPY ./configs/config4.json /opt/node4/neo-cli/config.orig.json
COPY ./configs/protocol.json /opt/node4/neo-cli/protocol.json
COPY ./wallets/wallet4.json /opt/node4/neo-cli/
COPY ./configs/config-applicationlogs4.json /opt/node4/neo-cli/Plugins/ApplicationLogs/config.json
COPY ./configs/config-applicationlogs4.json /opt/node4/neo-cli/Plugins/ApplicationLogs/config.orig.json
#  Add scripts
COPY ./scripts/run.sh /opt/
COPY ./scripts/run_datadir_wrapper.sh /opt/
ADD ./scripts/start_consensus_node.sh /opt/
COPY ./scripts/claim_neo_and_gas_fixedwallet.py /neo-python/
COPY ./scripts/claim_gas_fixedwallet.py /neo-python/
COPY ./wallets/neo-privnet.python-wallet /tmp/wallet
#  Some .bashrc helpers: 'neopy', and a welcome message for bash users
RUN echo "alias neopy=\"cd /neo-python \
 && np-prompt -p\"" >> /root/.bashrc
RUN echo "printf \"\n* Consensus nodes are running in screen sessions, check 'screen -ls'\"" >> /root/.bashrc
RUN echo "printf \"\n* neo-python is installed in /neo-python, with a neo-privnet.wallet file in place\"" >> /root/.bashrc
RUN echo "printf \"\n* You can use the alias 'neopy' in the shell to start neo-python's prompt.py with privnet settings\"" >> /root/.bashrc
RUN echo "printf \"\n* Please report issues to https://github.com/CityOfZion/neo-privatenet-docker\n\n\"" >> /root/.bashrc
#  Inform Docker what ports to expose
EXPOSE 20333/tcp
EXPOSE 20334/tcp
EXPOSE 20335/tcp
EXPOSE 20336/tcp
EXPOSE 30333/tcp
EXPOSE 30334/tcp
EXPOSE 30335/tcp
EXPOSE 30336/tcp
#  On docker run, start the consensus nodes
CMD ["/bin/bash", "/opt/run_datadir_wrapper.sh"]
