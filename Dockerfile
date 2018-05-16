FROM rocker/shiny:latest

RUN apt-get update && apt-get install -y \
	net-tools \
	procps \    
	libssl-dev \
  libcurl4-openssl-dev

# Installation packages R    
RUN R -e "install.packages('BiocInstaller', method='libcurl', repos='http://bioconductor.org/packages/3.6/bioc')" \
 		&& R -e "BiocInstaller::biocLite('Biostrings')" \
		&& R -e "install.packages('seqinr', method='libcurl', repos='https://cran.univ-paris1.fr/', dependencies=TRUE)" \
		&& R -e "install.packages('plotly', method='libcurl', repos='https://cran.univ-paris1.fr/', dependencies=TRUE)"

# Delete all example files inside shiny server && \
RUN rm -r /srv/shiny-server/*

# Ajout du dossier de l'application dans le serveur
ADD gc_cover /srv/shiny-server/

# Script pour gérer la durée de vie des containers
ADD monitor_traffic.sh /usr/bin/monitor_traffic.sh
RUN chmod +x /usr/bin/monitor_traffic.sh

COPY shiny-server.sh /usr/bin/shiny-server.sh
RUN chmod +x /usr/bin/shiny-server.sh

CMD ["/usr/bin/shiny-server.sh"]
