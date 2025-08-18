# Floral specialization correlates with restricted range size in bees across six families
[Charles N. Thrift](https://orcid.org/0000-0002-4257-6951), [Thomas J. Wood](https://orcid.org/0000-0001-5653-224X), Hillary S. Young, & [Katja C. Seltmann](https://orcid.org/0000-0001-5354-6048)

_In prep_

Please contact Charles Thrift for questions regarding the code or data (cnthrift@ucsb.edu)

## Title 
Floral specialization correlates with restricted range size in bees across six families

## Question
1. Is there a correlation between species range size and diet in bees?

## Methods Summary
1. Estimate range size in bee species (extent of occurrence) using convex-hulls, with cleaned global occurrence data from Dorey et al., 2023
2. Quantify diet as the number of plant genera a bee species collects pollen from, with pollen data from Wood et al., 2023
3. Analyze the relationship between range and diet using linear models
4. Current work: incorporate phylogenetic diversity metrics for plants and account for phylogeny in bees

## Results Summary
- Range size is significantly correlated to diet breadth in bees. Bees with more specialized diets are likely to have smaller range sizes.
- This relationship is not identical in all 6 bee families we test.


# Repository Directory

## Description of data

| File Name | Description |
| :------- | :------ |
| data_clean_dietFamily.csv | Bee species, family, and range variables |
| family_rarified.csv | Rarefied diet information for bee species at plant **family** level |
| genus_rarified.csv | Rarefied diet information for bee species at the plant **genus** level |
| clipped_hulls_dietFamily_july25/ clipped_hulls_dietFamily_july25.shp | Convex hulls for each bee species, exluding ocean regions. **Note:** when reading this file, the other files in the "clipped_hulls_dietFamily_july25" folder must be present. (Don't download just the .shp file). |


## Scripts: Contains code for data analysis in R
1. create_hulls.R - build convex hulls for each species using occurrence data
2. analyses.R - run linear models and build all figures


## Data: Contains CSV files necessary for running code
1. 
2. 


| Header 1 | Header 2 | Header 3 |
| :------- | :------: | -------: |
| Row 1 Col 1 | Row 1 Col 2 | Row 1 Col 3 |
| Row 2 Col 1 | Row 2 Col 2 | Row 2 Col 3 |


