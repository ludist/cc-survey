# Survey Data

This directory contains the anonymized survey response data and population benchmarks for the Creative Commons Recognition and Copyright Infringement Perceptions Survey Experiment.

## Data Files

### `survey-data/rousse-cc-survey-2024.csv`

The single canonical dataset for all analyses.

| Property | Value |
|----------|-------|
| **Observations** | 1,319 |
| **Columns** | 191 |
| **Encoding** | UTF-8 (clean) |
| **Format** | CSV |

### Groups

| Group | Label | N | Description |
|-------|-------|---|-------------|
| A | CC Succinct | 400 | CC-BY license (icon only) |
| B | CC Verbose | 414 | CC-BY-NC license (icon + text) |
| B1 | EU Supplement | 97 | Replacement sample for Educational Use scenario |
| C | Control | 408 | No Creative Commons license shown |

### Analysis Views

The analysis code in `_common.R` creates two filtered views:

- **`survey_data`** — Groups A, B, C (n=1,222) — used for PD, PU, CA, LS, WN, FS scenarios
- **`survey_data_eu`** — Groups A, B1, C (n=905) — used for Educational Use (EU) scenario only

**Why B1?** An error in the original randomized distribution excluded Group B participants from seeing the EU scenario. B1 is a supplemental data collection of 97 participants who completed only the EU portion. See Ch. 1 (Methods) and Ch. 2 (Codebook) of the Online Appendix for full details.

## Collection

- **Platform:** CloudResearch Connect
- **Date:** March 2024
- **Quota:** Nationally representative by gender, age, and ethnicity
- **Target N:** 1,200 (evenly divided across 3 conditions)
- **IRB:** Northwestern University (exempt determination)

## Variable Naming Convention

Variables follow a systematic naming pattern:

- **`[SC][Q]`** — Merged variable (across all conditions). Example: `PD1` = Public Domain, Question 1
- **`[SC]C[Q]`** — Control group. Example: `PDC1`
- **`[SC]TA[Q]`** — Treatment A (CC-BY). Example: `PDTA1`
- **`[SC]TB[Q]`** — Treatment B (CC-BY-NC). Example: `PDTB1`

### Question Numbers

| Q | Measure | Scale |
|---|---------|-------|
| 1 | Likelihood of reuse | 1 (Extremely unlikely) → 7 (Extremely likely) |
| 2 | Expected legal consequences | 1 (No consequences) → 7 (Incarceration/large fine) |
| 3 | Normative consequences (what *should* happen) | Same as Q2 |
| 4 | Willingness to pay | Dollar amount (continuous) |
| Delta | Q2 − Q3 | Computed (expectation gap) |

### Seven Scenarios

| Code | Scenario | Legal Context |
|------|----------|---------------|
| PD | Public Domain | Work with no copyright protection |
| PU | Personal Use | Private, non-commercial copying |
| EU | Educational Use | Classroom/educational context |
| CA | Commercial Advertising | Commercial reuse of creative work |
| LS | Large Scale Copying | Licensed digital content |
| WN | Wikipedia/Nonprofit | Nonprofit/wiki reuse |
| FS | Filesharing | Derivative/transformative fan work |

### Notable Variable Changes (from raw Qualtrics export)

- **`LIBCON`**: Recoded from non-sequential Qualtrics raw codes (1,2,6,7,3) to sequential ordinal scale (1–5: Very Liberal → Very Conservative)
- **`STATE`**: Recoded from Qualtrics numeric IDs to two-letter USPS abbreviations
- **`PDC-TIME_Page Submit`**: Renamed from `TIME-PDC_Page Submit` for naming consistency
- Seven unused `4A` columns and `postSurveyFlowError` removed

## `population/` Subdirectory

| File | Description |
|------|-------------|
| `acs-benchmarks.csv` | ACS 2023 1-Year Estimates for age, gender, race, income, education |
| `acs-source-notes.md` | Census table numbers, recoding decisions, retrieval documentation |

These benchmarks are used in Ch. 5 (Population Representativeness) of the Online Appendix to compare the survey sample against the U.S. adult population.

## Archival Copy

The canonical, versioned archive of this data is hosted on OSF:
**[osf.io/snxyt](https://osf.io/snxyt)**

The OSF project also contains the preregistration and Qualtrics survey instrument.
