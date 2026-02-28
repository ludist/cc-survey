# Creative Commons Recognition and Copyright Infringement Perceptions Survey Experiment

[![License: CC0 1.0](https://img.shields.io/badge/Survey_Instrument-CC0_1.0-green.svg)](https://creativecommons.org/publicdomain/zero/1.0/)
[![License: CC BY-NC 4.0](https://img.shields.io/badge/Code_&_Data-CC_BY--NC_4.0-lightgrey.svg)](https://creativecommons.org/licenses/by-nc/4.0/)
[![OSF Preregistration](https://img.shields.io/badge/OSF-Preregistered-blue.svg)](https://osf.io/snxyt)

> **Online Appendix** for Thomas H. Rousse, "Some Respect Reserved," *Cardozo Arts & Entertainment Law Journal* (forthcoming 2026).

## Overview

This repository is the research compendium and Online Appendix for a preregistered national survey experiment examining how Creative Commons licensing affects public perceptions of legal risk, expected consequences, and willingness to pay for creative works.

The study used a between-subjects experimental design (N=1,319) with seven copyright scenarios, each presented under control (no license), CC-BY, and CC-BY-NC conditions. Data were collected via CloudResearch Connect in March 2024.

**[Browse the Online Appendix →](https://ludist.github.io/cc-survey/)**

The Online Appendix is a 16-chapter Quarto Book that walks through the full analysis pipeline — experimental design, codebook, data validation, randomization checks, population representativeness, brand recognition, and scenario-by-scenario results. All code is visible and foldable; margin notes explain methodology for non-technical readers.

### Chapters

| Ch. | Title | Description |
|-----|-------|-------------|
| — | Introduction | Citation, navigation, reproduction guide |
| 1 | Methods | Experimental design, hypotheses, statistical methods |
| 2 | Codebook | Variable dictionary, naming conventions, response scales |
| 3 | Data Validation | Data loading, sample characteristics, quality checks |
| 4 | Randomization | Covariate balance (Age, Income, Education, Race, Gender) |
| 5 | Population | Sample vs. ACS 2023 population benchmarks |
| 6 | Brand Recognition | CC logo recognition vs. control brands |
| 7 | Scenarios | Analysis pipeline overview + 7 scenario sub-chapters |
| 7.1–7.7 | PD, PU, EU, CA, LS, WN, FS | Per-scenario treatment effects |
| 8 | Summaries | Cross-scenario comparison tables |

## Repository Structure

```
cc-survey/
├── _quarto.yml                        # Book project config
├── _common.R                          # Shared R setup (all chapters source this)
├── styles.scss                        # SCSS theme (Tufte-style tables, typography, colors)
├── fonts/                             # Self-hosted web fonts (Equity A WOFF2)
├── sidebar-toc.html                   # Custom sidebar TOC partial
├── section-numbering.html             # Section numbering partial
├── index.qmd                          # Book landing page
├── 01-methods.qmd … 08-summaries.qmd  # Numbered chapters
├── _scenarios/                        # Scenario sub-chapters (§7.1–7.7)
│   ├── pd.qmd … fs.qmd
├── docs/                              # Pre-rendered book (GitHub Pages serves this)
├── analysis/
│   └── functions/                     # 8 literate R analysis modules
│       ├── data_prep.R
│       ├── stats_helpers.R
│       ├── plotting_themes.R
│       ├── analysis_functions.R
│       ├── export_helpers.R
│       ├── randomization_checks.R
│       ├── master_analysis.R
│       └── population_comparison.R
├── data/
│   ├── survey-data/
│   │   └── rousse-cc-survey-2024.csv  # Canonical dataset (1,319 obs, 191 cols)
│   ├── population/
│   │   ├── acs-benchmarks.csv         # ACS 2023 population benchmarks
│   │   └── acs-source-notes.md
│   └── survey/                        # Survey instrument PDF
├── materials/
│   ├── preregistration.md             # Pre-registered hypotheses & methods
│   ├── survey-codebook.csv            # Tabular codebook
│   ├── survey-codebook.json           # Structured codebook
│   └── IMAGES-LICENSE.md              # CC0 dedication for vignette images
├── renv.lock                          # R package version lockfile (R 4.5.2)
├── CITATION.cff                       # Machine-readable citation
├── LICENSE                            # Dual-license summary
└── LICENSES/
    ├── CC0-1.0.txt                    # Survey instrument license
    └── CC-BY-NC-4.0.txt               # Code, data & docs license
```

## Reproducing the Analysis

The `docs/` directory contains the pre-rendered Online Appendix, which is what GitHub Pages serves. You do not need to render anything to browse the appendix — just visit the [Online Appendix](https://ludist.github.io/cc-survey/).

To reproduce the analysis from source:

### Requirements

- [R](https://cran.r-project.org/) (4.5.2)
- [Quarto](https://quarto.org/) (≥ 1.4)
- [Positron](https://positron.posit.co/) or RStudio (recommended IDE)

### Steps

```bash
# Clone the repository
git clone https://github.com/ludist/cc-survey.git
cd cc-survey

# Restore R package environment
Rscript -e 'install.packages("renv"); renv::restore()'

# Render the entire book
quarto render

# Or render a single chapter
quarto render 06-brand.qmd --to html
```

The rendered book will appear in `_book/`. Open `_book/index.html` to browse locally.

## Links

| Resource | URL |
|----------|-----|
| **"Some Respect Reserved"** | *Forthcoming*, Cardozo Arts & Entertainment Law Journal |
| **SSRN Preprint** | [SSRN](https://papers.ssrn.com/sol3/papers.cfm?abstract_id=5379423) |
| **OSF Project** | [osf.io/snxyt](https://osf.io/snxyt) |
| **Preregistration** | [osf.io/snxyt](https://osf.io/snxyt) |
| **Online Appendix** | [ludist.github.io/cc-survey](https://ludist.github.io/cc-survey/) |

## Citation

```bibtex
@article{rousse2026somerespect,
  author  = {Rousse, Thomas H.},
  title   = {Some Respect Reserved},
  journal = {Cardozo Arts \& Entertainment Law Journal},
  year    = {2026},
  note    = {Forthcoming}
}
```

## License

This repository is dual-licensed:

| Component | License |
|-----------|---------|
| Survey instrument & vignette images (`materials/`) | [CC0 1.0 Universal](https://creativecommons.org/publicdomain/zero/1.0/) — public domain |
| Everything else (code, data, documentation, online appendix) | [CC-BY-NC 4.0](https://creativecommons.org/licenses/by-nc/4.0/) — attribution, non-commercial |

The survey instrument and original vignette images are released as CC0 to maximize reuse by other researchers. The analysis code, data, and documentation require attribution and are restricted to non-commercial use. Full legal texts are in [`LICENSES/`](LICENSES/).

## Author

**Thomas H. Rousse**
J.D., Ph.D., Northwestern University
[ORCID](https://orcid.org/0009-0007-9705-3035) · [LinkedIn](https://www.linkedin.com/in/throusse) · [SSRN](https://papers.ssrn.com/sol3/papers.cfm?abstract_id=5379423)
