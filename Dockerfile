FROM inwt/r-shiny:4.4.3

USER root

# System dependencies required to compile and run ReSources dependencies,
# especially rgl and packages using graphical output.
RUN apt-get update \
    && apt-get install -y --no-install-recommends \
        lbzip2 \
        libcairo2-dev \
        libfreetype6-dev \
        libglu1-mesa-dev \
        libpng-dev \
        libx11-dev \
        libxss1 \
        pandoc \
        xvfb \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

# Add the Pandora package repository.
RUN echo "options(repos = c(getOption('repos'), PANDORA = 'https://Pandora-IsoMemo.github.io/drat/'))" >> /usr/local/lib/R/etc/Rprofile.site

# These packages are used by dependencies of the application
RUN Rscript -e "remotes::install_github('r-lib/httr2@v1.2.3', upgrade = 'never')"
RUN Rscript -e "remotes::install_github('tidyverse/ellmer@v0.4.1', upgrade = 'never')"

# Install nimble first because ReSources uses nimble in LinkingTo.
#
# The dated Posit Package Manager repository acts as a snapshot and pins a version.
RUN Rscript -e "install.packages('nimble', repos = 'https://packagemanager.posit.co/cran/__linux__/jammy/2025-03-01')"

# Build and install the package
WORKDIR /opt/ReSources

COPY . .

RUN installPackage DSSM \
    && installPackage

# Create the runtime user before installing Miniconda into its home.
RUN adduser --system --disabled-password --home /home/inwt inwt

ENV HOME=/home/inwt
ENV RETICULATE_MINICONDA_PATH=/home/inwt/.local/share/r-miniconda

USER inwt

# the first attempt to install miniconda will fail
RUN Rscript -e "reticulate::install_miniconda();" || true

# Accept Conda Terms of Service
RUN ${RETICULATE_MINICONDA_PATH}/bin/conda tos accept --override-channels --channel https://repo.anaconda.com/pkgs/main && \
    ${RETICULATE_MINICONDA_PATH}/bin/conda tos accept --override-channels --channel https://repo.anaconda.com/pkgs/r

# install miniconda
RUN Rscript -e "reticulate::install_miniconda(force = TRUE); \
                reticulate::use_miniconda('r-reticulate'); \
                reticulate::conda_install('r-reticulate', c('python-kaleido', 'packaging')); \
                reticulate::conda_install('r-reticulate', 'plotly', channel = 'plotly'); \
                reticulate::use_miniconda('r-reticulate');"

CMD ["Rscript", "-e", "library(shiny); ReSources::startApplication(3838, '0.0.0.0')"]
