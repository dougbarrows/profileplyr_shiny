FROM rocker/r-ver:devel

RUN apt-get update && apt-get install -y \
    sudo \
    gdebi-core \
    pandoc \
    pandoc-citeproc \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libxml2-dev \
    libbz2-dev \
    liblzma-dev \
    libv8-dev \
    libssl-dev \
    libmariadbclient-dev \
    libpq-dev \
    xtail \
    wget 


# Download and install shiny server
RUN wget --no-verbose https://download3.rstudio.org/ubuntu-14.04/x86_64/VERSION -O "version.txt" && \
    VERSION=$(cat version.txt)  && \
    wget --no-verbose "https://download3.rstudio.org/ubuntu-14.04/x86_64/shiny-server-$VERSION-amd64.deb" -O ss-latest.deb && \
    gdebi -n ss-latest.deb && \
    rm -f version.txt ss-latest.deb && \
    . /etc/environment && \
    R -e "install.packages(c('shiny', 'DT', 'shinydashboard', 'dplyr', 'shinythemes', 'shinyWidgets', 'shinyjs', 'shinyFiles', 'devtools'), repos='http://cran.us.r-project.org/', dependencies = TRUE)" && \
    R -e "install.packages('BiocManager')" && \
    R -e "BiocManager::install(version='devel')" && \
    R -e "BiocManager::install(c('rtracklayer', 'GenomicRanges', 'TxDb.Hsapiens.UCSC.hg38.knownGene', 'TxDb.Mmusculus.UCSC.mm9.knownGene', 'org.Mm.eg.db', 'GO.db'), dependencies = TRUE)" && \
    R -e "devtools::install_github('https://github.com/RockefellerUniversity/profileplyr')" && \
    chown shiny:shiny /var/lib/shiny-server 


COPY shiny-server.conf  /etc/shiny-server/shiny-server.conf
COPY /app /srv/shiny-server/

EXPOSE 3838


COPY shiny-server.sh /usr/bin/shiny-server.sh
RUN chmod -R +r /usr/bin/shiny-server.sh



CMD ["/usr/bin/shiny-server.sh"]
