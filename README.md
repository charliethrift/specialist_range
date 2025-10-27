# Concentrated vulnerabilities in bees: Diet specialists have smaller geographic ranges
[Charles N. Thrift](https://orcid.org/0000-0002-4257-6951), [Thomas J. Wood](https://orcid.org/0000-0001-5653-224X), An Bui, Hillary S. Young, & [Katja C. Seltmann](https://orcid.org/0000-0001-5354-6048)

_In prep_

Please contact Charles Thrift for questions regarding the code or data (cnthrift@ucsb.edu)

## Abstract 
Wild bees are experiencing declines, yet most bee species remain unassessed for IUCN extinction risk. Geographic range size is used in risk assessment criteria under the assumption that species with smaller ranges are more vulnerable to anthropogenic impacts. Narrow diet breadth can also increase vulnerability but is not currently incorporated into assessments. Niche breadth theory predicts a positive association between range size and diet breadth, which could concentrate risk in a subset of species; however, this relationship may vary by taxa and is not well established in bees. Here, we combined pollen-use data from natural history collections with global occurrence records to analyze the relationship between diet breadth and range size among bees and between bee families. We assigned diet breadth (three metrics: categorical, numeric, and phylogenetic) and range size (extent of occurrence) to 633 species from six families. Across bees, range size and diet breadth were positively correlated, with diet specialists tending to occupy smaller ranges. These results suggest that range size and diet breadth jointly contribute to increased vulnerability, indicating that some specialist species may merit conservation prioritization. Our findings support the integration of trait-based approaches for assessing extinction risk and highlight the high value of natural history data in evaluating patterns of vulnerability among pollinators.


## Methods Summary
1. Estimate range size in bee species (extent of occurrence) using convex-hulls, with cleaned global occurrence data from Dorey et al., 2023
2. Quantify diet as: categorical (specialist/generalist), numeric (rarefied count of plant families), and phylogenetic (breadth of plant families), with pollen data from Wood et al., 2023
3. Analyze the relationship between range and diet using Pearson's correlation tests, t-tests, linear models, PGLS regression


# Repository Directory

## Description of data

| File Name | Description |
| :------- | :------ |
| data_2025_09.csv | Bee species, family, and range variables |
| clipped_hulls_dietFamily_july25/ clipped_hulls_dietFamily_july25.shp | Convex hulls for each bee species, exluding ocean regions. **Note:** when reading this file, the other files in the "clipped_hulls_dietFamily_july25" folder must be present. (Don't download just the .shp file). |


## Scripts: Contains code for data analysis in R

| File Name | Description |
| :------- | :------ |
| create_hulls.R | Build convex hulls for each species using occurrence data |
| analyses_2025_10.R | Main analyses and figures |
| analyses_phylogeny_2025_10.R | Calculate phylogenetic diet breadth and analyze |
| analyses_pgls_2025_10.R | Construct bee species phylogeny and run PGLS |




