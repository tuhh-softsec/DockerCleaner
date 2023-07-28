#  Licensed to the Apache Software Foundation (ASF) under one
#  or more contributor license agreements.  See the NOTICE file
#  distributed with this work for additional information
#  regarding copyright ownership.  The ASF licenses this file
#  to you under the Apache License, Version 2.0 (the
#  "License"); you may not use this file except in compliance
#  with the License.  You may obtain a copy of the License at
#
#    http://www.apache.org/licenses/LICENSE-2.0
#
#  Unless required by applicable law or agreed to in writing,
#  software distributed under the License is distributed on an
#  "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
#  KIND, either express or implied.  See the License for the
#  specific language governing permissions and limitations
#  under the License.
# ###########################################################
#  Dockerfile to build Traffic Server container images
#    as Edges for Traffic Control 1.4
#  Based on CentOS 6.6
# ###########################################################
#  For cache, you may either use (RAM or disk) block devices or disk directories
#  To use RAM block devices, pass them as /dev/ram0 and /dev/ram1 via `docker run --device`
#  To use disk directories, simply don't pass devices, and the container will configure Traffic Server for directories
#  Block devices may be created on the native machine with, for example, `modprobe brd`.
#  The recommended minimum size for each block devices is 1G.
#  For example, `sudo modprobe brd rd_size=1048576 rd_nr=2`
#  Example Build and Run:
#
#  docker build --rm --tag traffic_server_edge:1.4 Traffic_Server_Edge
#
#  docker run --name my-edge-0 --hostname my-edge-0 --net cdnet --device /dev/ram0:/dev/ram0 --device /dev/ram1:/dev/ram1 --env TRAFFIC_OPS_URI=http://my-traffic-ops:3000 --env TRAFFIC_OPS_USER=superroot --env TRAFFIC_OPS_PASS=supersecreterpassward --detach traffic_server_edge:1.4
#
#  OR
#
#  docker run --name my-edge-0 --hostname my-edge-0 --net cdnet --env TRAFFIC_OPS_URI=http://my-traffic-ops:3000 --env TRAFFIC_OPS_USER=superroot --env TRAFFIC_OPS_PASS=supersecreterpassward --detach traffic_server_edge:1.4
FROM centos:6.6
MAINTAINER dev@trafficcontrol.apache.org
RUN yum install -y perl-JSON
RUN curl -O http://traffic-control-cdn.net/downloads/trafficserver-5.3.2-599.089d585.el6.x86_64.rpm
RUN yum install -y trafficserver-5.3.2-599.089d585.el6.x86_64.rpm
RUN mkdir /opt/ort
RUN cd /opt/ort \
 && curl -O https://raw.githubusercontent.com/Comcast/traffic_control/RELEASE-1.4.0-RC0/traffic_ops/bin/traffic_ops_ort.pl
RUN chmod 777 /opt/ort/traffic_ops_ort.pl
RUN curl -O http://traffic-control-cdn.net/downloads/astats_over_http-1.2-8.el6.x86_64.rpm
RUN yum install -y astats_over_http-1.2-8.el6.x86_64.rpm
RUN setcap 'cap_net_bind_service=+ep' /opt/trafficserver/bin/traffic_cop
RUN setcap 'cap_net_bind_service=+ep' /opt/trafficserver/bin/traffic_crashlog
RUN setcap 'cap_net_bind_service=+ep' /opt/trafficserver/bin/traffic_ctl
RUN setcap 'cap_net_bind_service=+ep' /opt/trafficserver/bin/traffic_layout
RUN setcap 'cap_net_bind_service=+ep' /opt/trafficserver/bin/traffic_line
RUN setcap 'cap_net_bind_service=+ep' /opt/trafficserver/bin/traffic_logcat
RUN setcap 'cap_net_bind_service=+ep' /opt/trafficserver/bin/traffic_logstats
RUN setcap 'cap_net_bind_service=+ep' /opt/trafficserver/bin/traffic_manager
RUN setcap 'cap_net_bind_service=+ep' /opt/trafficserver/bin/traffic_sac
RUN setcap 'cap_net_bind_service=+ep' /opt/trafficserver/bin/trafficserver
RUN setcap 'cap_net_bind_service=+ep' /opt/trafficserver/bin/traffic_server
RUN setcap 'cap_net_bind_service=+ep' /opt/trafficserver/bin/traffic_top
RUN setcap 'cap_net_bind_service=+ep' /opt/trafficserver/bin/traffic_via
RUN setcap 'cap_net_bind_service=+ep' /opt/trafficserver/bin/tspush
RUN setcap 'cap_net_bind_service=+ep' /opt/trafficserver/bin/tsxs
#  \todo move Heka to its own container, sharing the ATS log file via --volume
RUN curl -LO https://github.com/mozilla-services/heka/releases/download/v0.10.0/heka-0_10_0-linux-amd64.rpm
RUN yum install -y heka-0_10_0-linux-amd64.rpm
RUN mkdir etc/hekad
RUN printf '[ats_traffic_logs] \ntype = "LogstreamerInput" \nsplitter = "TokenSplitter" \ndecoder = "ATS_transform_decoder" \nlog_directory = "/opt/trafficserver/var/log/trafficserver" \nfile_match = "custom_ats_2.log" \n[ATS_transform_decoder] \ntype = "PayloadRegexDecoder" \nmatch_regex = '"'^(?P<UnixTimestamp>[\d]+\.[\d]+) chi=(?P<chi>\S+) phn=(?P<phn>\S+) shn=(?P<shn>\S+) url=(?P<url>\S+) cqhm=(?P<cqhm>\w+) cqhv=(?P<cqhv>\S+) pssc=(?P<pssc>\d+) ttms=(?P<ttms>\d+) b=(?P<b>\d+) sssc=(?P<sssc>\d+) sscl=(?P<sscl>\d+) cfsc=(?P<cfsc>\S+) pfsc=(?P<pfsc>\S+) crc=(?P<crc>\S+) phr=(?P<phr>\S+) uas=(?P<uas>\S+) xmt=(?P<xmt>\S+)'"' \n\n[ATS_transform_decoder.message_fields] \nType = "ats_traffic" \ntimestamp = "%%UnixTimestamp%%" \nclientip = "%%chi%%" \nhost = "%%phn%%" \nshn = "%%shn%%" \nurl = "%%url%%" \nmethod = "%%cqhm%%" \nversion = "%%cqhv%%" \nstatus = "%%pssc%%" \nrequest_duration = "%%ttms%%" \nbytes = "%%b%%" \nresponse_code = "%%sssc%%" \nresponse_length = "%%sscl%%" \nclient_status = "%%cfsc%%" \nproxy_code = "%%pfsc%%" \ncrc = "%%crc%%" \nphr = "%%phr%%" \nuseragent = "%%uas%%" \nmoney_trace = "%%xmt%%" \n[PayloadEncoder] \ntype = "PayloadEncoder" \n[FxaKafkaOutput] \ntype = "KafkaOutput" \ntopic = "ipcdn" \nmessage_matcher = "TRUE" \nencoder = "PayloadEncoder" \naddrs = ["{{.KafkaUri}}"] \n[Message_Counter] \ntype = "CounterFilter" \nmessage_matcher = "Type != '"'heka.counter-output'"'" \nencoder = "CounterLogEncoder" \n[CounterLogEncoder] \ntype="PayloadEncoder" \nappend_newlines = true \nprefix_ts = true \nts_format = "Mon Jan _2 15:04:05 MST 2006" \n[CounterLogOutput] \ntype = "FileOutput" \nmessage_matcher = "Type == '"'heka.counter-output'"'" \nencoder = "CounterLogEncoder" \npath = "/tmp/hekad_counter.log"' > /etc/hekad/heka.toml
RUN printf '#!/bin/sh \n\nif [ -f /etc/rc.status ]; then \n . /etc/rc.status \n rc_reset \nfi \nif [ -f /etc/rc.d/init.d/functions ]; then \n . /etc/rc.d/init.d/functions \nfi \n. /etc/init.d/functions \nname="hekad" \nexec="/usr/bin/hekad" \nprog="hekad" \nuser="root" \ngroup="root" \npidfile=/var/run/${prog}.pid \nconf=/etc/hekad/heka.toml \nlog=/var/log/heka.log \nDAEMON_ARGS=${DAEMON_ARGS---user root} \nnice=19 \nargs=" --config $conf" \n[ -e /etc/sysconfig/$prog ] \
 && . /etc/sysconfig/$prog \nlockfile=/var/lock/subsys/$prog \nHEKA_USER=root \nstart() { \n [ -x $exec ] || exit 5 \n [ -f $CONF_FILE ] || exit 6 \n nice -n ${nice} chroot --userspec $user:$group / sh -c " exec \"$prog\" $args " > ${log} 2>&1 & \n echo $! > $pidfile \n echo "$name started." \n return 0 \n} \nstop() { \n if status ; then \n pid=`cat "$pidfile"` \n echo "Killing $name (pid $pid) with SIGTERM" \n kill -9 $pid \n for i in 1 2 3 4 5 ; do \n echo "Waiting $name (pid $pid) to die..." \n status || break \n sleep 1 \n done \n if status ; then \n echo "$name stop failed; still running." \n else \n echo "$name stopped." \n fi \n fi \n} \nrestart() { \n stop \n start \n} \nreload() { \n restart \n} \nforce_reload() { \n restart \n} \nstatus(){ \n if [ -f "$pidfile" ] ; then \n pid=`cat "$pidfile"` \n if kill -0 $pid > /dev/null 2> /dev/null ; then \n return 0 \n else \n return 2 # program is dead but pid file exists \n fi \n else \n return 3 # program is not running \n fi \n} \nrh_status() { \n status -p $pidfile $prog \n} \nrh_status_q() { \n rh_status >/dev/null 2>&1 \n} \ncase "$1" in \n start) \n rh_status_q \
 && exit 0 \n $1 \n ;; \n stop) \n rh_status_q || exit 0 \n $1 \n ;; \n restart) \n $1 \n ;; \n reload) \n rh_status_q || exit 7 \n $1 \n ;; \n force-reload) \n force_reload \n ;; \n status) \n status \n code=$? \n if [ $code -eq 0 ] ; then \n echo "$prog is running" \n else \n echo "$prog is not running" \n fi \n exit $code \n ;; \n condrestart|try-restart) \n rh_status_q || exit 0 \n restart \n ;; \n *) \n echo $"Usage: $0 {start|stop|status|restart|condrestart|try-restart|reload|force-reload}" \n exit 2 \nesac' > /etc/init.d/hekad
RUN chmod +x /etc/init.d/hekad
EXPOSE 80/tcp 443/tcp
ADD run.sh /
ENTRYPOINT /run.sh
