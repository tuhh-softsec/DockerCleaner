FROM andreptb/tomcat:7-jdk8
#  DO NOT TOUCH ABOVE THIS LINE
#  INSTRUCTIONS do not use directly. 
#  1. Check out the KnetMiner source. 
#  2. Within it, create a folder with the OXL file and other files referenced in the ARG statement below
#  3. Change into the folder containing the KnetMiner source and your newly created species folder
#  4. Build with "docker image build --squash -t knetminer-myspecies -f common/quickstart/Dockerfile-dev --build-arg species_dir=myspeciesfolder [--build-arg varname=value -build-arg ...] .", specifying one build-arg parameter for each argument not provided with a default below or that you wish to override
#  5. Run the resulting image with "docker run -p8080:8080 -it --rm knetminer-myspecies". You can access it at http://localhost:8080/client/
#  DO NOT TOUCH BELOW THIS LINE
#  Files that need to be in the local folder (including the .oxl file)
# ARG oxl_file=data.oxl  
ARG oxl_file
ARG semantic_motifs_file=SemanticMotifs.txt  
ARG basemap_file=basemap.xml  
ARG sample_query_file=sampleQuery.xml  
ARG background_image_file=background.jpg  
ARG release_notes_file=release_notes.html  
#  Other arguments
ARG tax_id
ARG multiorganisms=false
ARG species_name
ARG species_link_name
ARG keywords
ARG description
ARG reference_genome
ARG species_dir
ARG knetminer_port
# EXPOSE 8080
EXPOSE ${knetminer_port}
WORKDIR /root
ENV ds="/root/knetminer/common/quickstart/datasource/src/main" \
    cl="/root/knetminer/common/quickstart/client/src/main/webapp/html" \
    tc="/usr/local/tomcat/conf/" \
    common="/root/knetminer/common"
RUN mkdir -p /root/knetminer
COPY pom.xml /root/knetminer/pom.xml
COPY common ${common}
RUN set -x \
 && apk add --no-cache maven nodejs-npm git libxml2-utils \
 && npm config set unsafe-perm true \
 && npm install bower gulp -g \
 && echo '{"allow_root":true}' > /root/.bowerrc \
 && cd /root/knetminer \
 && mkdir -p ${cl}/data ${cl}/image
COPY ${species_dir}/${oxl_file} /root/data.oxl
COPY ${species_dir}/${semantic_motifs_file} ${ds}/resources/SemanticMotifs.txt
COPY ${species_dir}/${basemap_file} ${cl}/data/basemap.xml
COPY ${species_dir}/${sample_query_file} ${cl}/data/sampleQuery.xml
COPY ${species_dir}/${background_image_file} ${cl}/image/background.jpg
COPY ${species_dir}/${release_notes_file} ${cl}/release_notes.html
RUN cd /root/knetminer \
 && mv ${ds}/resources/config.xml.template ${ds}/resources/config.xml \
 && mv ${ds}/java/rres/knetminer/datasource/provider/Quickstart.java.template ${ds}/java/rres/knetminer/datasource/provider/Quickstart.java \
 && mv ${cl}/release.html.template ${cl}/release.html \
 && mv ${cl}/index.jsp.template ${cl}/index.jsp \
 && mv ${cl}/javascript/utils-config.js.template ${cl}/javascript/utils-config.js \
 && chromosomes=$( echo "cat //genome/chromosome/@number" | xmllint --shell ${cl}/data/basemap.xml | sed -n 's/[^\"]*\"\([^\"]*\)\"[^\"]*/\1/gp' | sed -e :a -e '$!N; s/\n/,/; ta' ;) \
 && sed -e "s/!!REFGENOME!!/${reference_genome}/g" -i'' ${cl}/javascript/utils-config.js \
 && sed -e "s/!!SPECIES!!/${species_name}/g" -i'' ${cl}/javascript/utils-config.js \
 && sed -e "s/!!LINKNAME!!/${species_link_name}/g" -i'' ${cl}/javascript/utils-config.js \
 && sed -e "s/!!SPECIES!!/${species_name}/g" -i'' ${cl}/release.html \
 && sed -e "/!!RELEASE!!/{r ${cl}/release_notes.html" -e "d}" -i'' ${cl}/release.html \
 && sed -e "s/!!SPECIES!!/${species}/g" -i'' ${cl}/index.jsp \
 && sed -e "s/!!CHROMOSOMES!!/${chromosomes}/g" -i'' ${cl}/index.jsp \
 && sed -e "s/!!METAKEYWORDS!!/${keywords}/g" -i'' ${cl}/index.jsp \
 && sed -e "s/!!METADESC!!/${description}/g" -i'' ${cl}/index.jsp \
 && sed -e "s/!!REFGENOME!!/${reference_genome}/g" -i'' ${ds}/resources/config.xml \
 && sed -e "s/!!TAXID!!/${tax_id}/g" -i'' ${ds}/resources/config.xml \
 && sed -e "s/!!LINKNAME!!/${species_link_name}/g" -i'' ${ds}/java/rres/knetminer/datasource/provider/Quickstart.java \
 && sed -i.bk "s/8080/${knetminer_port}/g" ${tc}/server.xml \
 && mvn install \
 && cd ${common} \
 && mvn install \
 && cd ${common}/server-datasource-api \
 && mvn install \
 && cd ${common}/server-datasource-ondexlocal \
 && mvn install \
 && cd ${common}/server-base \
 && mvn install \
 && cd ${common}/client-base \
 && mvn install \
 && cd ${common}/quickstart \
 && mvn install \
 && mv ${common}/quickstart/client/target/client.war ${CATALINA_HOME}/webapps \
 && mv ${common}/quickstart/ws/target/ws.war ${CATALINA_HOME}/webapps
CMD ["catalina.sh", "run"]
