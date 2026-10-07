Dataset: CA-SYS soil bacterial communities (16S rRNA gene sequencing)

Description:
This dataset contains bacterial community data obtained from soil samples collected between 2018 and 2021 within the CA-SYS experimental platform. The dataset includes OTU count tables, sample metadata, and phyloseq objects used for downstream statistical analyses.

Files:

- casys_otu_table_raw.csv  
  OTU count table (rows: OTUs, columns: samples; last column: taxonomy)

- casys_sample_metadata.csv  
  Metadata associated with each sample

- casys_phyloseq_raw.rds  
  Phyloseq object including all samples and OTUs before filtering

- casys_phyloseq_clean.rds  
  Filtered phyloseq object after removal of low-depth and contaminated samples

- 01_build_phyloseq_object.html
- 02_quality_control.html
- 03_taxonomic_composition.html
- 04_alpha_diversity.html
- 05_beta_diversity.html
  Reproducible analysis documents illustrating data structure and basic analyses

Dataset dimensions:
- 640 samples total (595 valid after quality filtering)
- 4 cropping systems × 4 years (2018–2021) × 42 experimental plots
- 10,633 OTUs
--------------------------------------------------

Metadata description:

SampleID: unique identifier of each sample  
StorageID: internal storage identifier  
Year: sampling year  
Parcelle: experimental plot  
ID_Point: sampling point within plot  

System: cropping system (4 agroecological systems):
    - SD1: permanent no-till
    - SD2: non-permanent no-till
    - TS1: tillage with exogenous nitrogen input
    - TS2: tillage without exogenous nitrogen input  

SystAE: agroecological system classification (3 categories based on spatial organization):
    - SD zone (SD1 and SD2)
    - TS zone (TS1 and TS2)
    - mixed zone (SD and TS co-occurring in the same area)

Landuse_atSampling: cover crop at sampling time  
CropY: crop the year of sampling 
CropY-1: crop 1 year before the year of sampling
CropY-2: crop 2 years before the year of sampling
CropY-3: crop 3 years before the year of sampling

Plowing: presence/absence of plowing (Yes/No)  
SupTillage: superficial tillage (Yes/No)  
TillageIntensity: level of tillage intensity  

FertiN: nitrogen fertilization (Yes/No)  
FertiPK: phosphorus and potassium fertilization (Yes/No)  

SequencingDepth: number of sequencing reads per sample  

SoilType: soil type classification 

Resist_V1_cor, Resist_V2_cor, Resist_V3_cor: In situ soil resistivity measurements 
Resist_PC1: first principal component derived from PCA on Resist_V1_cor, Resist_V2_cor, Resist_V3_cor  

sample_status: sample quality flag  
    - valid: sample retained for analysis  
    - contaminated: removed due to contamination  
    - low_depth: removed due to insufficient sequencing depth  
Filtering thresholds: sequencing depth < 11,000 reads (low_depth), 
  5 samples identified as contaminated during quality control
--------------------------------------------------

Experimental variables:

The dataset allows the analysis of microbial communities in relation to agricultural practices.

--------------------------------------------------

Notes:

- Raw sequencing data are available in the NCBI SRA repository (see associated publication for accession numbers).
- The year 2018 corresponds to a baseline sampling before the implementation of experimental treatments.
- Some samples were excluded from the filtered dataset based on sequencing depth and contamination criteria.
- The filtered phyloseq object corresponds to the dataset used for downstream analyses.
- Soil resistivity variables (Resist_V1_cor, Resist_V2_cor, Resist_V3_cor) were obtained from an external dataset [Seger et al., 2023], available at https://doi.org/10.57745/2V46MF.

--------------------------------------------------

Contact: 
Aymé SPOR 
INRAE  
ayme.spor@inrae.fr