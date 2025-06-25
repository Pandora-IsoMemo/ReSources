FROM ghcr.io/pandora-isomemo/base-image:latest

RUN adduser --system --disabled-password --home /home/inwt inwt
ENV HOME /home/inwt 
USER inwt

ADD . .

RUN Rscript -e "reticulate::install_miniconda(); \
                reticulate::use_miniconda('r-reticulate'); \
                reticulate::conda_install('r-reticulate', c('python-kaleido', 'packaging')); \
                reticulate::conda_install('r-reticulate', 'plotly', channel = 'plotly'); \
                reticulate::use_miniconda('r-reticulate') \
                install.packages('https://cran.r-project.org/src/contrib/Archive/nimble/nimble_1.0.1.tar.gz', repos = NULL)" \
    && installPackage DSSM \
    && installPackage

CMD ["Rscript", "-e", "library(shiny); ReSources::startApplication(3838, '0.0.0.0')"]
