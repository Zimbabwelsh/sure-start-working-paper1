# sure-start-working-paper1
Data and code to used to produce the analysis used in the Sure Start working paper. See https://doi.org/10.17605/OSF.IO/AY683


# How to use 

1. Run the code in the `makeFiles` folder to get cleaned version of the raw data (stored in `outputs`). 
2. Files in the main folder with the prefix `protocol` do the analysis (saved in `ouputs`). 
3. (optional) Files with the prefix `source` contain code for specific repetitive analysis tasks (e.g. make graphs, run models). 

# Data

Raw data is kept in the `data` folder. Processed and cleaned data for analysis is kept in `outputs` folder. 

## Open data sources

- NIMDM 2005 (Wards): https://admin.opendatani.gov.uk/dataset/2743964e-4bc3-45d9-a0ea-1729ba477406/resource/814afd82-7f27-4b42-b3bd-65f459d6f91c/download/nimdm-2005-ward-level.csv

- NIMDM 2010 (SOA): https://admin.opendatani.gov.uk/dataset/6b9e49fb-7f3c-4503-ba68-31acbf32b1c3/resource/18c9edc5-1f82-4cb5-99cb-bbd96e941de7/download/northern-ireland-multiple-deprivation-measure-2010-statistical-geographies-super-output-areas.csv

- NIMDM 2017 (SOA): https://www.nisra.gov.uk/publications/nimdm17-soa-level-results

- Sure Start coverage 2009 (source: former Sure Start website archived by National Archives, click on partnership links): https://webarchive.nationalarchives.gov.uk/ukgwa/20090325095035/http://www.surestart.gov.uk/aboutsurestart/help/contacts/northernireland/
This data is available on GitHub along with code preparation code: https://github.com/MengLeZhang/sure-start-historical-website


## Non-open data

One key dataset is not shared in this repo but described below:

- Development of Sure Start Coverage Table.XLSX: This was sent by the NI Department of Education. Worksheet 1 contains their records of Sure Start areas and theri timing. Worksheet 2 is a cleaned and reformatted version which contains key variables:
  - soa_nimd: name of SOA for linkage to imd
  - Phase.included: which phase a particular SOA was included
