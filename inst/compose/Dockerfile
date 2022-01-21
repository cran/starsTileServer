FROM rocker/geospatial 
MAINTAINER Bart 
RUN install2.r -n 5 plumber stars; \
	rm -rf /tmp/downloaded_packages
RUN R --quiet -e 'install.packages("starsdata", repos = "http://pebesma.staff.ifgi.de", type = "source")'
RUN R --quiet   -e "remotes::install_gitlab('bartk/starsTileServer')"
EXPOSE 3436
COPY script.R script.R
RUN R --quiet   -e "source('script.R')"
ENTRYPOINT ["R", "--quiet", "-e", "server<-readRDS('server.rds') ;server$run( port=3436, host='0.0.0.0', swagger=T)"]
