#  ###########################################################################
#   (C) Copyright IBM Corporation 2015.                                      #
#                                                                            #
#   Licensed under the Apache License, Version 2.0 (the "License");          #
#   you may not use this file except in compliance with the License.         #
#   You may obtain a copy of the License at                                  #
#                                                                            #
#        http://www.apache.org/licenses/LICENSE-2.0                          #
#                                                                            #
#   Unless required by applicable law or agreed to in writing, software      #
#   distributed under the License is distributed on an "AS IS" BASIS,        #
#   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. #
#   See the License for the specific language governing permissions and      #
#   limitations under the License.                                           #
#                                                                            #
#  ###########################################################################
FROM ubuntu:16.04
MAINTAINER Kavitha Suresh Kumar <kavisuresh@in.ibm.com>
RUN apt-get update \
 && apt-get install --no-install-recommends unzip=6.0-20ubuntu1.1 wget=1.17.1-1ubuntu1.5 -y
ARG URL
#  ######################## Installation Manager #############################
#   Install Installation Manager
RUN wget -q $URL/Install_Mgr_v1.6.2_Lnx_WASv8.5.5.zip -O /tmp/IM.zip \
 && mkdir /tmp/im \
 && unzip -qd /tmp/im /tmp/IM.zip \
 && /tmp/im/installc -acceptLicense -accessRights admin -installationDirectory "/opt/IBM/InstallationManager" -dataLocation "/var/ibm/InstallationManager" -showProgress \
 && rm -fr /tmp/IM.zip /tmp/im
#  ######################## IBM HTTP Server ##################################
#   Install IBM HTTP Server
RUN mkdir /tmp/supp \
 && wget -q $URL/WAS_V8.5.5_SUPPL_1_OF_3.zip -O /tmp/supp1.zip \
 && wget -q $URL/WAS_V8.5.5_SUPPL_2_OF_3.zip -O /tmp/supp2.zip \
 && wget -q $URL/WAS_V8.5.5_SUPPL_3_OF_3.zip -O /tmp/supp3.zip \
 && unzip -qd /tmp/supp /tmp/supp1.zip \
 && unzip -qd /tmp/supp /tmp/supp2.zip \
 && unzip -qd /tmp/supp /tmp/supp3.zip \
 && rm /tmp/supp1.zip /tmp/supp2.zip /tmp/supp3.zip \
 && /opt/IBM/InstallationManager/eclipse/tools/imcl -showProgress -acceptLicense install com.ibm.websphere.IHS.v85 -repositories /tmp/supp/repository.config -installationDirectory /opt/IBM/HTTPServer -properties "user.ihs.httpPort=80,user.ihs.allowNonRootSilentInstall=true"
#  #################### IBM HTTPServer Fixpack ##################################
#   Install IBM HTTP Server Fixpack
RUN mkdir /tmp/suppfp \
 && wget -q $URL/8.5.5-WS-WASSupplements-FP0000009-part1.zip -O /tmp/spart1.zip \
 && wget -q $URL/8.5.5-WS-WASSupplements-FP0000009-part2.zip -O /tmp/spart2.zip \
 && unzip -qd /tmp/suppfp /tmp/spart1.zip \
 && unzip -qd /tmp/suppfp /tmp/spart2.zip \
 && rm /tmp/spart1.zip /tmp/spart2.zip \
 && /opt/IBM/InstallationManager/eclipse/tools/imcl -showProgress -acceptLicense install com.ibm.websphere.IHS.v85 -repositories /tmp/suppfp/repository.config -installationDirectory /opt/IBM/HTTPServer -properties "user.ihs.httpPort=80,user.ihs.allowNonRootSilentInstall=true"
#  ######################### WebServer Plugins  ##################################
#   Install WebServer Plugins
RUN /opt/IBM/InstallationManager/eclipse/tools/imcl -showProgress -acceptLicense install com.ibm.websphere.PLG.v85 -repositories /tmp/supp/repository.config -installationDirectory /opt/IBM/WebSphere/Plugins
#  #################### WebServer Plugins Fixpack ################################
#   Install WebServer Plugins Fixpack
RUN /opt/IBM/InstallationManager/eclipse/tools/imcl -showProgress -acceptLicense install com.ibm.websphere.PLG.v85 -repositories /tmp/suppfp/repository.config -installationDirectory /opt/IBM/WebSphere/Plugins \
 && rm -fr /opt/IBM/WebSphere/Plugins/java /tmp/suppfp
#  ###################### WebSphere Customization Tools ##########################
#   Install IBM WebSphere Customization Tools
RUN /opt/IBM/InstallationManager/eclipse/tools/imcl -showProgress -acceptLicense install com.ibm.websphere.WCT.v85 -repositories /tmp/supp/repository.config -installationDirectory /opt/IBM/WebSphere/Toolbox \
 && rm -fr /tmp/supp
#  ##################### WebSphere Customization Tools Fixpack ################### 
#   Install IBM WebSphere Customization Tools Fixpack
RUN mkdir /tmp/wct \
 && wget -q $URL/8.5.5-WS-WCT-FP0000009-part1.zip -O /tmp/wct1.zip \
 && wget -q $URL/8.5.5-WS-WCT-FP0000009-part2.zip -O /tmp/wct2.zip \
 && unzip -qd /tmp/wct /tmp/wct1.zip \
 && unzip -qd /tmp/wct /tmp/wct2.zip \
 && rm /tmp/wct1.zip /tmp/wct2.zip \
 && /opt/IBM/InstallationManager/eclipse/tools/imcl -showProgress -acceptLicense install com.ibm.websphere.WCT.v85 -repositories /tmp/wct/repository.config -installationDirectory /opt/IBM/WebSphere/Toolbox \
 && rm -fr /tmp/wct
CMD ["tar", "cvf", "/tmp/ihs_plg_wct.tar", "/opt/IBM/HTTPServer", "/opt/IBM/WebSphere/Plugins", "/opt/IBM/WebSphere/Toolbox"]
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
