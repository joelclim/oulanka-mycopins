FROM bioconductor/jupyterlab-ml:RELEASE_3_22

COPY environment.yml /tmp/environment.yml

USER root

RUN mamba clean -a 
RUN mamba env update -f /tmp/environment.yml 

USER jovyan

WORKDIR /home/jovyan

