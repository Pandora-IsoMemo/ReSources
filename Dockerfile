FROM ghcr.io/pandora-isomemo/base-image:latest

RUN adduser --system --disabled-password --home /home/inwt inwt
ENV HOME=/home/inwt 
USER inwt

ADD . .

# the first attempt to install miniconda will fail
RUN Rscript -e "reticulate::install_miniconda();" || true

# Accept Conda Terms of Service
RUN /home/inwt/.local/share/r-miniconda/bin/conda tos accept --override-channels --channel https://repo.anaconda.com/pkgs/main && \
    /home/inwt/.local/share/r-miniconda/bin/conda tos accept --override-channels --channel https://repo.anaconda.com/pkgs/r

# install miniconda
RUN Rscript -e "reticulate::install_miniconda(force = TRUE); \
                reticulate::use_miniconda('r-reticulate'); \
                reticulate::conda_install('r-reticulate', c('python-kaleido', 'packaging')); \
                reticulate::conda_install('r-reticulate', 'plotly', channel = 'plotly'); \
                reticulate::use_miniconda('r-reticulate');"

# Install nimble
RUN Rscript -e "install.packages('nimble', repos = 'https://packagemanager.posit.co/cran/__linux__/jammy/2025-03-01', version = '1.3.0')"
# install DSSM & ReSources
RUN installPackage DSSM \
    && installPackage

CMD ["Rscript", "-e", "library(shiny); ReSources::startApplication(3838, '0.0.0.0')"]
