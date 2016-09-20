# tsconvert
R library for tstp (time series transfer protocol), the XML-standard protocol to connect to the toposoft time series database (https://www.toposoft.de/)
    
This package has been developed in the course of the project "STBMOD" (https://www.fh-muenster.de/forschung/forschungskatalog/projekt.php?pr_id=722), 
funded by the German Federal Ministry of Education and Research (BMBF, FKZ 03FH033PX2).

Install using devtools:

``` r
if(!require(devtools)) {
  install.packages('devtools')
  devtools::install_github("dleutnant/tstpr")
}
```