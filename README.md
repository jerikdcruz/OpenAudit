# OpenAudit: advancing governance research and accountability with natural language-processed audit reports

## About the repository

This repository contains the beta version of OpenAudit, an open data initiative aiming to make publicly-available audit reports more tractable for use by researchers and public-interest audiences using NLP and machine learning methods. Here, we focus on a subset of these reports in terms of coverage and content to provide a proof-of-concept of the planned broader dataset and platform. 

We thank the Philippine government's [Commission on Audit (COA)](https://www.coa.gov.ph/) for sharing their archive of audit reports that made this project possible, as well as [MIT GOV/LAB](https://mitgovlab.org/) and the [MIT Political Methodology Lab](https://pmlab.mit.edu/) for their financial support.

## Background and Context

In many developing democracies around the world, government audit institutions regularly publish audit and financial reports that track government bodies’ decisions in raising, spending, and managing public resources on behalf of their citizens. These publicly-available reports contain data of immense value to governance researchers and ordinary citizens; they potentially offer ways to track government decisions, processes, and behaviors related to corruption, accountability, and state capacity at levels of detail currently unavailable in commonly-used governance indicators (e.g. Transparency International's Corruption Perception Index and the World Bank's World Governance Indicators). Until the present, however, these reports generally remain underused by academics, policy researchers, and the general public alike. 

Starting with the universe of audit reports of the Philippines' COA from 1998-2022, OpenAudit leverages advances in NLP and machine learning methods to make these audit reports more ready-for-use for these audiences as well as public-interest watchdogs and reformers. Apart from processing these reports into more structured datasets that will be showcased on dedicated platforms, the project establishes partnerships to ensure the validity of resulting data and its integration with ready user communities. 

## Data for this beta version

For the pilot phase of OpenAudit, the team has been provided digital copies of all COA audit reports from 1998 to 2022. While this bigger corpus of reports is currently undergoing processing, this beta version of OpenAudit showcases what can be learned by focusing on the executive summaries of these reports. Like the audit reports themselves, these executive summaries are quite standardized in what they report, having sections dedicated to financial and operational highlights, auditors' opinions on financial statements, audit findings and recommendations, and government agencies' progress on implementing on the past year's audit recommendations. The randomly-selected executive summary of [Babatngon municipality](https://www.coa.gov.ph/download/3322/leyte/42439/babatngon-executive-summary-2015.pdf) in Leyte province for 2015 provides an example of the information typically conveyed by such documents. 

We use 17,392 audit reports' executive summaries spanning the universe of Philippine local governments for 2010-2020. Within these executive summaries, we zero-in on the "Audit Findings and Recommendations" sections, as these summarize the main issues flagged by auditors in their assessments of agencies' public finances. 

Access the unprocessed executive summaries [here](https://drive.google.com/drive/folders/12dtP97ojWtc1wGjusewsmkULpW39gK6d?usp=drive_link).

## Data Usage

We include the following files in this repository's beta version: 

* `Tax-Corruption-Tradeoff.pdf` provides an (unpublished) work-in-progress of how the data from the audit reports can be used for academic research on different aspects of governance (here, on fiscal capacity and accountability) to demonstrate the initiative's intellectual merit
* `unliquidata.csv` is the beta version processed data that extracts recorded amounts of anomalous transactions based on unliquidated cash transfers. This dataset will be refined progressively along with the other variables in the upcoming weeks
* `extraction1.R` is the R script for extracting this data from the audit reports; as with the `.csv` file, this will be progressively updated. Rerunning requires all the individual nested raw executive summaries to be unnested onto one level in a single folder
* `Topicmodel1.R` is the R script for running the topic proportion plot showcased in the work-in-progress as well as in other summary material of the initiative, based on an LDA Topic Model (k = 25). More NLP-related code will be shared as these are developed for purposes of the project
* `UPCOMING` is a short methodological note and workflow on how the executive summaries were processed and the measures created

## Project Team

The pilot phase of OpenAudit includes the following persons: 

 * **Project Proponent and Lead Investigator:** Jerik Cruz; PhD candidate, MIT Department of Political Science; Graduate Research Associate, MIT GOV/LAB
 * **Project Partner Representative:** Philip Arnold Tuaño, PhD; Dean, Ateneo School of Government
 * **Project Adviser:** Heidi Mendoza, CPA, MPA, MNSA; Professor of Praxis, Ateneo School of Government; Former Commissioner and Officer-in-Charge, Commission on Audit of the Philippines
 * **Research Assistant:** Rainier Mora; MA student, Department of Economics, Ateneo de Manila University

We also gratefully acknowledge the past research assistance of Mr. Cymon Lubangco, who helped secure access to the reports and executive summaries showcased in this repository.

## Future Repository Plans 

Over the next few months, we intend to expand on this beta-version repository in the following directions: 

* *Using the whole audit report as the data source:* we will extract and process data from the whole of the audit reports from 1998 to 2022, using large language models (currently under development), the pipeline and results of which will also be hosted on the repository
* *Developing new governance measures:* beyond tracking corruption/anomalous transactions, we will develop broader indicators related to transparency, accountability, and service delivery capacity (especially in health-related services)
* *Showcasing potential uses of dataset:* we will provide additional examples and replication materials of how the data can be used by researchers. Latter examples are likely to be drawn from prospective collaborations with academic and policy researchers, to showcase the project's intellectual and practical merit

Over the course of 2024, we plan to develop a dedicated website for the dataset which will be housed with the Ateneo School of Government, and to engage in conversations to scale up the project's scope beyond the Philippines. 

## Licensing

All data released in this beta version has been taken from the Philippine [Commission on Audit's (COA)](https://www.coa.gov.ph/) audit reports, which are documents in the public domain. 

This work and repository is licensed under a [Creative Commons Attribution 4.0 International](https://github.com/jerikdcruz/OpenAudit/blob/main/LICENSE.md) license.
