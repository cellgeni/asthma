FROM rocker/shiny:3.5.1

# install some R required stuff
RUN apt-get update -y --no-install-recommends \
    && apt-get -y install -f \
       zlib1g-dev \
       libssl-dev \
       libcurl4-openssl-dev \
       wget \
       && apt-get clean && \
       rm -rf /var/lib/apt/lists/*

# R packages
RUN install2.r Matrix gridExtra ggplot2 reshape2

# add app to the server
RUN rm -r /srv/shiny-server/*
COPY asthma/* /srv/shiny-server/


CMD ["/usr/bin/shiny-server.sh"]
