# BioSampleParser
![alt text](https://raw.githubusercontent.com/angelolimeta/BioSampleParser/master/Biosampleparser.png)

A tool designed to provide users with easier access to study metadata.

BioSampleParser takes the following study identifiers as input:
* European Nucleotide Archive (EMBL-EBI) accession number, e.g. PRJNA397906.
* NCBI BioProject ID, e.g. 397906.

It then queries NCBI BioSample for any samples related to the study, downloads sample metadata in .xml format and parses it into a tabular format of choice (Data frame object in R, or .tsv file).

## Setup and Usage

Donwload BioSampleParser.R and place it in your directory of choice.

Run the following command in an R session to source the function:
``` r
source("/path/to/BioSampleParser.R")
```
Call the function with the query argumnet set to your study identifier:
``` r
metadata_df <- BioSampleParser(query = "PRJNA397906")
```

## List of all arguments

query
* character, ENA ID or NCBI BioProject ID for the study of interest

filePath
* character, path to local NCBI BioProject .xml file.

file.tsv
* character, filename for saving metadata as a .tsv file.

