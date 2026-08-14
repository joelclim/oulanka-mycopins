# Oulanka Mycopins
This project contains the analyses of the saproxylic fungi communities in boreal forests within Oulanka National Park. The analyses are brooken down into several Jupyter notebooks primarily using `R` to perform the analysis.

The analyses are in the `analyses` folder. They were generated from Jupyter notebooks found in the `notebook` folder. Supporting `R` scripts are in the `src` folder. The `data` folder contains the source datasets of the analyses.

## Setup
1. Clone this repository
```
$ git clone https://www.github.com/joelclim/oulanka-mycopins
```
3. [Install Docker](https://www.docker.com/get-started/)
4. Start Bioconductor/JupyterLab Docker container
```
$ cd oulanka-mycopins
$ sudo docker compose start
```
6. Open web browser and access the local JupyterLab. Password:`kean`
```
http://localhost:8888
```
7. Open any of the Jupyter notebooks to view the analyses.

## Generating analysis html from a Jupyter notebook
```
$ sudo docker exec -it (docker container name) bash
jovyan@904aa5366f03:~$ quarto render notebooks/(file name).ipynb --output-dir ../analyses/
```

## Analyses
1. [Exploratory Data Analysis](notebooks/1-Exploratory-Data-Analysis.ipynb)
2. Alpha Diversity
   1. [Rarefaction and Extrapolation Curves](notebooks/2a-Alpha-Diversity-Rarefaction-and-Extrapolation-Curves.ipynb)
   2. [Comparing Groups](notebooks/2b-Alpha-Diversity-Comparing-Groups.ipynb)
   3. Regression Analysis
3. Beta Diversity
   1. Ordination(NMDS)
   2. PERMANOVA
4. Indicator Species Analysis