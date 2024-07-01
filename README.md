# SOGI
CAMP 60

To recreate this analysis, you will need access to SAS and RStudio. Original work done in the software and operating system environments indicated below.

SAS version 9.4 in Windows:
Edition	Windows Server 2022 Datacenter
Version	21H2
Installed on	‎10/‎30/‎2022
OS build	20348.1487

RStudio Version 2022.12.0+353 (2022.12.0+353), R Version 4.1.0 (2021-05-18) in Mac OS 13.1

**Step 1: Prepare Data Sets**

The YRBS National Data Set for 2019 and years prior can be found here: https://www.cdc.gov/healthyyouth/data/yrbs/sadc_2019/sadc_2019_national.dat 

The YRBS National Data Set for 2021 can be found here: https://www.cdc.gov/healthyyouth/data/yrbs/files/2021/XXH2021_YRBS_Data.dat

Note: Both YRBS data sets are too large to store on GitHub; you must download them locally to reproduce the present analyses. To do this, use the corresponding Format and Input SAS programs, each modified from CDC for the present analysis. These are available in this repository as “YRBS SAS Formats” and “YRBS SAS Input” for the 2019 and prior years file and as "2021 YRBS SAS Formats" and 2021 YRBS SAS Input" for the 2021 data set. 

SAS program instructions: To generate the YRBS 2021 dataset, you must first open the script named "2021 YRBS SAS Formats.sas" in the SAS program. The heading of the file contains specific instructions to properly save the data format file. After successfully running this file, you may then run the script named "2021 YRBS SAS Inputs.sas" Again, the heading contains specific instructions to properly execute the file. Next, to generate the 2015-2019 dataset, similarly run and following corresponding instructions for the scripts "YRBS SAS Formats.sas" and "YRBS SAS Inputs.sas", respectively. After running these files, you will have produced the needed YRBS datasets to for the analyses.

The BRFSS datasets can be found here: https://www.cdc.gov/brfss/annual_data/annual_data.htm 

Note: You must individually download each year of data from 2014-2021.

To run the scripts in this project, you should store these data files in a folder called "data - raw". You should also make a folder called "data - clean" to house the data generated, and one called "plots" for the plots generated.

**Option 1: Run all files**

***Single Step: Run the Master Script***

Run "SOGI master script.R" to generate the necessary data sets and all project files. This will generate a series of plots with sexual orientation (BRFSS & YRBS), gender identity (BRFSS only), and sex-of-sex-partners (YRBS only).

**Option 2: Run specified files**

***Step A: Prepare Data Sets***

In order, run the scripts whose index number begins with "0". Before running script 01, note that at line 21, there is a space after “.XPT” before the closing quotation mark. The original file was built on Mac OS 13.1 and the space is required for RStudio to properly open the file. You may need to remove this space if you are working in a Windows or other non-Mac OS.

You have generated the clean data files to conduct the analyses of your choosing.

***Step B: Run Project Analyses***

After completing Step A once, you will save local copies of the data sets used by the remaining project files in the project. You may now run the remaining scripts based on which specific analyses are of interest.
