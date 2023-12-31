FROM bcgsc/orca-5:latest
LABEL maintainer="sjackan@gmail.com" \
      name="bcgsc/orca-6"
RUN brew install quast quest quickmerge quicktree quip quorum r8s racon rails rampart rapidnj raxml raxml-ng ray rcorrector readline readsim realphy recon repaq repeatmasker repeatmodeler repeatscout rmblast rna-star rnammer rtg-tools salmid salmon sambamba samblaster samclip samtools samtools@0.1 scarpa sdsl-lite seq-gen seqan seqkit seqtk sequel sga shovill shrimp sickle simulate-pcr ska skesa skewer smalt snap snippy snoscan snp-dists snp-sites snpeff soapdenovo solexaqa sortmerna spaced spades spici sqlite squeakr squeezambler sratoolkit ssake staden-io-lib stringtie sumaclust swarm swipe szip tagdust tasr taxonkit tbb tbl2asn tigmint tophat trans-abyss transdecoder transrate-tools treepl trf trimadap trimal trimmomatic trinity trnascan unicycler unikmer uniqtag uproc vague varscan varsim vcake vcflib vcftools velvet velvetoptimiser verticalize viennarna vsearch vt wiggletools wtdbg2 xmatchview xssp yaha
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
