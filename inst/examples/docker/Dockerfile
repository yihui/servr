FROM rocker/tidyverse

ENV R_FORGE_PKGS Rserve
ENV R_CRAN_PKGS Rcpp R6 uuid checkmate mime jsonlite digest

RUN apt-get update && \
    apt-get install -y --no-install-recommends libssl-dev libjemalloc-dev wget pandoc libxml2 && \
    install2.r -r http://www.rforge.net/ $R_FORGE_PKGS && \
    install2.r $R_CRAN_PKGS

COPY . /app

# ENV LD_PRELOAD /usr/lib/x86_64-linux-gnu/libjemalloc.so

COPY ["./", "./"]

RUN Rscript -e "install.packages('servr')"

# Run restRServe API

EXPOSE 2222

ENTRYPOINT ["Rscript", "app.R"]

# Build:
# docker build .
# Run:
# docker run -p 2222 {image}