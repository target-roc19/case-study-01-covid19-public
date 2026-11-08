# TARGET-EU CASE STUDY 1: Effectiveness of BNT162b2 mRNA COVID-19 vaccine

This repository contains code which implements [case study 1](https://catalogues.ema.europa.eu/node/4899/administrative-details) of the [TARGET-EU project: Comparative effectiveness and safety studies using the target trial emulation and estimand frameworks](https://catalogues.ema.europa.eu/node/4440/administrative-details). The repository contains the code which implements the study design of this case study, along with documentation of that code, and where feasible, dummy data which allows users to run and understand the code. Below we describe briefly the case study and document the repository and its operation. 

## Background 
The rapid development of COVID-19 vaccines represents a critical milestone in mitigating the impact of the pandemic. However, ongoing evaluation of their real-world effectiveness is essential given the varied vaccination coverage.

Research question: What is the effectiveness of two-doses of BNT162b2 mRNA-based vaccine in preventing SARS-CoV-2 infection compared with no vaccination, among healthy adults (≥ 16 years old), including those with stable pre-existing medical conditions? 

Primary objective: To estimate the incidence of SARS-CoV-2 infection among individuals receiving the BNT162b2 vaccine, compared to unvaccinated individuals. 

### EMA Catalogue Entries
[Catalogue Entry of Case Study](https://catalogues.ema.europa.eu/node/4899/administrative-details)

[Protocol](https://catalogues.ema.europa.eu/node/4899/study-documents)

[Report](https://catalogues.ema.europa.eu/node/4899/study-documents)

## Repository 

This repository contains all code necessary to reproduce the results contained in the case study report, that is, the analytic pipeline designed to implement the specific study design. The repository does not contain any real data: The real data remains on secure servers, and only fictitious dummy data is shared here for testing, development and illustration purposes. 

The analytic pipeline is conceptualised as a series of transformations (denoted "T") which transform data objects ("D") to other data objects [Gini et al. 2016](https://pmc.ncbi.nlm.nih.gov/articles/PMC4780748/)

![pipeline](docs/VAC4EU-PIPELINE.png)

The exact analytic pipeline for this study is represented as a graph containing specific data objects and transformation and their relationships [here](https://github.com/target-roc19/case-study-01-covid19-public/blob/main/docs/cs1_pipeline_target.png)

Each data object in the pipeline is documented in a corresponding **codebook** which describes how the data object is obtained from its inputs. Thereby, the codebooks specify the implementation of the given study design, which is executed by the scripts in the repository.

### Repository Structure
```
[case-study-01-covid19]
|____configuration
|____data
| |____D2_cdm
| |____D3_study_variables
| |____D4_analytic_datasets
| |____D5_results
| |____D6_report
|____docs
| |____codebook
|____logs
|____run
|____tests
|____transformations
| |____T2_semantic_harmonization
| |____T3_study_design
| |____T4_statistical_analysis
| |____T5_processing_results
|____CHANGELOG.md
|____LICENSE
|____README.md
```

- The `data/D2_cdm` folder contains the base input data, following a Common Data Model (CDM) structure, in this case the [ConcePTION](https://github.com/IMI-ConcePTION/ConceptionTools/wiki) CDM.
- The `transformations` folder contains the scripts which compose the analytic pipeline
- Intermediate data objects created by `transformations` are stored in the other `data` subfolders
- The `docs/codebook` folder contains the study codebooks describing each intermediate data object
- The `transformations/common_configuration/RWE-BRIDGE` folder contains all project metadata, including events, medicines and vaccine codelists used in the study, as well as other metadata files specifying definitions of study concepts and variables. 

### Dummy Data
- Test data for a number of intermediate data objects can be found in `tests/testthat/test_data`
- Setting `test_mode: TRUE` in `configuration/config_project.yaml` generates dummy data for the eligible population data object while running the pipeline.
- Synthetic CDM will be made available soon


### How to Run
Run the full pipeline with:

```
run/run_pipeline.R
```
Or run specific steps, e.g.:

```
run/run_T2.R
```

- Which parts of the pipeline are executed can be configured through `configuration/config_pipeline.yaml`
- More specifically which scripts are executed in each part can be configured through `configuration/config_TX.yaml`.
- `config_project.yaml` defines which objects along which paths are used as inputs to different scripts in the pipeline, including the input CDM instance.

## License
See [License](https://github.com/target-roc19/case-study-01-covid19-public/LICENSE.txt)

## Open Science Badges
<a href='https://doi.org/10.25937/krp4-g724'>
<img height='88' src='https://www.comses.net/static/images/icons/open-code-badge.png'>
</a>

## Contact
For questions or support, contact:
- Case Study Leads
    - Epidemiologist: Constanza Andaur Navarro
    - Email: C.L.AndaurNavarro@umcutrecht.nl
    - Statistician: Oisin Ryan
    - Email: O.Ryan@umcutrecht.nl
- Lead Programmer
    - Name: Sebastian Mildiner Moraga
    - Email: s.mildinermoraga@umcutrecht.nl
- Programming Team
    - Organisation: University Medical Center Utrecht
    - Email: wp2.targeteu@umcutrecht.nl
