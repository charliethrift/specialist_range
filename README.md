# Floral specialization correlates with restricted range size in bees across six families
[Charles N. Thrift](https://orcid.org/0000-0002-4257-6951), [Thomas J. Wood](https://orcid.org/0000-0001-5653-224X), Hillary S. Young, & [Katja C. Seltmann](https://orcid.org/0000-0001-5354-6048)

_In prep_

Please contact Charles Thrift for questions regarding the code or data (cnthrift@ucsb.edu)

## Abstract 
Diet breadth and range size are two important life history traits, and species with restricted values for either are at greater risk of extinction. Narrow breadth in both traits would likely indicate highly elevated extinction risk. It is thus important to understand if these traits tend to be correlated (creating a relatively small group of high extinction risk species) or if they are independent (diluting risks across more species). Positive relationships between dietary breadth and range size have been found in some taxa, including butterflies but not others (e.g. bats). Importantly, the relationship between diet breadth and range size has not yet been evaluated in bees (Hymenoptera: Anthophila), a group of great conservation concern due to the ecosystem services they provide, but for which quantitative diet breadth metrics is rarely available. To address this gap, we examined diet breadth for 633 bee species from six families, by quantifying the number of plant families or plant genera adult bees collect pollen from, using natural history collections. For these same species we also estimated species range sizes using occurrence data. We then analyzed the relationship between range size and diet breadth for each bee family. We found a positive correlation between diet breadth and range size, with diet specialists tending to have smaller geographic ranges; however the effects varied by bee family. This relationship was significant within bee families Andrenidae, Apidae, Colletidae, Halictidae, and Megachilidae, but not Melittidae. This suggests that for many bee families diet specialist bee species may be more vulnerable to anthropogenic change, via combined effects of small range size, potentially meriting conservation prioritization for specialist taxa in these families. Notably, we highlight that this conclusion was only made possible by characterizing diet breadth at scales finer than traditional, broad, and binary categories (e.g. specialist and generalist). Given that most bee species lack diet breadth data at a global scale, this emphasizes the importance of additional research into the diets of unstudied, potentially vulnerable taxa.


## Methods Summary
1. Estimate range size in bee species (extent of occurrence) using convex-hulls, with cleaned global occurrence data from Dorey et al., 2023
2. Quantify diet as the number of plant genera a bee species collects pollen from, with pollen data from Wood et al., 2023
3. Analyze the relationship between range and diet using linear models
4. Current work: incorporate phylogenetic diversity metrics for plants and account for phylogeny in bees


# Repository Directory

## Description of data

| File Name | Description |
| :------- | :------ |
| data_clean_dietFamily.csv | Bee species, family, and range variables |
| family_rarified.csv | Rarefied diet information for bee species at plant **family** level |
| genus_rarified.csv | Rarefied diet information for bee species at the plant **genus** level |
| clipped_hulls_dietFamily_july25/ clipped_hulls_dietFamily_july25.shp | Convex hulls for each bee species, exluding ocean regions. **Note:** when reading this file, the other files in the "clipped_hulls_dietFamily_july25" folder must be present. (Don't download just the .shp file). |


## Scripts: Contains code for data analysis in R

| File Name | Description |
| :------- | :------ |
| create_hulls.R | Build convex hulls for each species using occurrence data |
| analyses.R | Run linear models and build all figures |
| bee_phylogeny.R | Construct bee species phylogeny and run PGLS |




