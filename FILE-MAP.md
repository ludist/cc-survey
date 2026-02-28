# File Map: Vault → GitHub Repository

> This document maps files from the vault working directory to the public GitHub repository.
> Review before pushing. **Do not push this file.**
>
> **Updated:** 2026-02-23 — reflects Quarto Book architecture, single-file data, renamed functions.

## Goes to GitHub ✅

### Quarto Book Structure (project root)

| Vault Source | Repo Destination | Notes |
|-------------|------------------|-------|
| `_quarto.yml` | `_quarto.yml` | Book project config (at root) |
| `_common.R` | `_common.R` | Shared R setup (all chapters source this) |
| `styles.css` | `styles.css` | Custom CSS (Tufte-style tables, sizing) |
| `sidebar-toc.html` | `sidebar-toc.html` | Custom sidebar TOC injection |
| `index.qmd` | `index.qmd` | Book landing page (intro, citation, reproduction, session info) |
| `01-methods.qmd` | `01-methods.qmd` | Ch. 1: Experimental Design & Methods |
| `02-codebook.qmd` | `02-codebook.qmd` | Ch. 2: Codebook & Variable Dictionary |
| `03-validation.qmd` | `03-validation.qmd` | Ch. 3: Data Validation |
| `04-randomization.qmd` | `04-randomization.qmd` | Ch. 4: Randomization Checks |
| `05-population.qmd` | `05-population.qmd` | Ch. 5: Sample v. Population Analysis |
| `06-brand.qmd` | `06-brand.qmd` | Ch. 6: Brand Recognition |
| `07-scenarios.qmd` | `07-scenarios.qmd` | Ch. 7: Scenario Analysis Overview + Pipeline Docs |
| `_scenarios/pd.qmd` | `_scenarios/pd.qmd` | §7.1: Public Domain |
| `_scenarios/pu.qmd` | `_scenarios/pu.qmd` | §7.2: Personal Use |
| `_scenarios/eu.qmd` | `_scenarios/eu.qmd` | §7.3: Educational Use (B1 dataset) |
| `_scenarios/ca.qmd` | `_scenarios/ca.qmd` | §7.4: Commercial Advertising |
| `_scenarios/ls.qmd` | `_scenarios/ls.qmd` | §7.5: Large Scale Copying |
| `_scenarios/wn.qmd` | `_scenarios/wn.qmd` | §7.6: Wikipedia Image Search |
| `_scenarios/fs.qmd` | `_scenarios/fs.qmd` | §7.7: Filesharing |
| `08-summaries.qmd` | `08-summaries.qmd` | Ch. 8: Cross-Scenario Summary Tables |

### Data

| Vault Source | Repo Destination | Notes |
|-------------|------------------|-------|
| `data/survey-data/rousse-cc-survey-2024.csv` | `data/survey-data/rousse-cc-survey-2024.csv` | Single canonical dataset (1,319 obs, 191 cols, clean UTF-8) |
| `data/population/acs-benchmarks.csv` | `data/population/acs-benchmarks.csv` | ACS 2023 benchmarks (committed, no API key needed) |
| `data/population/acs-source-notes.md` | `data/population/acs-source-notes.md` | ACS table numbers and recoding decisions |
| `data/survey/CCMasterv5-24-02-12.pdf` | `data/survey/CCMasterv5-24-02-12.pdf` | Complete survey instrument |

### R Functions

| Vault Source | Repo Destination | Notes |
|-------------|------------------|-------|
| `analysis/functions/data_prep.R` | `analysis/functions/data_prep.R` | Renamed from `_literate` (now primary) |
| `analysis/functions/stats_helpers.R` | `analysis/functions/stats_helpers.R` | |
| `analysis/functions/plotting_themes.R` | `analysis/functions/plotting_themes.R` | |
| `analysis/functions/analysis_functions.R` | `analysis/functions/analysis_functions.R` | |
| `analysis/functions/export_helpers.R` | `analysis/functions/export_helpers.R` | |
| `analysis/functions/randomization_checks.R` | `analysis/functions/randomization_checks.R` | |
| `analysis/functions/master_analysis.R` | `analysis/functions/master_analysis.R` | |
| `analysis/functions/population_comparison.R` | `analysis/functions/population_comparison.R` | Ch. 5 population comparison functions |

### R Environment

| Vault Source | Repo Destination | Notes |
|-------------|------------------|-------|
| `.Rprofile` | `.Rprofile` | renv activation |
| `renv.lock` | `renv.lock` | Package version lockfile (R 4.5.2) |
| `.here` | `.here` | Project root sentinel for `here::here()` |

### Materials & Licensing

| Vault Source | Repo Destination | Notes |
|-------------|------------------|-------|
| `admin/Preregistration/THR-CC-PREREG.md` | `materials/preregistration.md` | Pre-registered hypotheses |
| *(staging)* | `materials/survey-codebook.csv` | Tabular codebook from QSF |
| *(staging)* | `materials/survey-codebook.json` | Structured codebook from QSF |
| *(staging)* | `materials/IMAGES-LICENSE.md` | CC0 for vignette images |
| *(staging)* | `README.md` | Repository README |
| *(staging)* | `CITATION.cff` | Machine-readable citation |
| *(staging)* | `LICENSE` | Dual-license summary (CC0 + CC-BY-NC) |
| *(staging)* | `LICENSES/CC0-1.0.txt` | Full CC0 1.0 legal text |
| *(staging)* | `LICENSES/CC-BY-NC-4.0.txt` | Full CC-BY-NC 4.0 legal text |
| *(staging)* | `.gitignore` | See staging |
| *(staging)* | `.github/workflows/publish.yml` | GitHub Action for auto-deploy |
| *(staging)* | `data/README.md` | Data dictionary (needs update for single-file architecture) |

## Stays in Vault (PRIVATE) 🚫

| Vault File/Directory | Why |
|---------------------|-----|
| `drafts/` | Article drafts, redlines, LRC versions |
| `correspondence/` | Editor communications |
| `admin/IRB/` | IRB protocol (institutional) |
| `analysis/functions/prev/` | Archived non-literate function versions |
| `analysis/prev/` | Old RMarkdown analysis versions (v2, v3) |
| `data/survey-data/prev/` | Pre-cleanup CSV versions (provenance) |
| `data/survey-data/ccSurveyWorkingFile3.csv` | Superseded by canonical CSV |
| `data/survey-data/ccSurveyWorkingFileB1.csv` | Superseded by canonical CSV |
| `data/population/fetch_acs.py` | One-time retrieval script |
| `notes/` | Planning docs (RESTRUCTURING-PLAN.md, etc.) |
| `github-staging/` | This staging area — not pushed |
| `*.xlsx` | Excel working files |
| `*.rds` | Cached analysis objects |
| `~$*.docx` | Word lock files |
| `_book/` | Rendered output (regenerated) |
| `_freeze/` | Quarto freeze cache (regenerated) |
| `renv/` | Local package library (regenerated from lockfile) |

## Decisions (Resolved)

1. ✅ **Vignette Images** — Include in `materials/vignette-images/`. Tommy owns copyright, releasing as CC0 public domain. Covered by root `LICENSE` (single source of truth).

2. ✅ **QSF file** — Codebook generated as `survey-codebook.csv` and `.json` in staging.

3. ✅ **Raw Qualtrics export** — Not in GitHub. Available on OSF. Link from GitHub README.

4. ✅ **Single-file data architecture** (2026-02-17) — Consolidated two CSV files into `rousse-cc-survey-2024.csv`. Group filtering in `_common.R`. Old files preserved in vault for provenance.

5. ✅ **Function renaming** (2026-02-16) — Dropped `_literate` suffix. Non-literate originals archived to `analysis/functions/prev/`.

## Open TODOs

- [x] **ORCID:** 0009-0007-9705-3035. Updated in all staging files (2026-02-15).
- [x] **OSF URL:** osf.io/snxyt (project) + osf.io/kq93t (registration). Updated in all staging files (2026-02-15).
- [x] **SSRN:** abstract_id=5379423. Updated in all staging files (2026-02-15). Note: SSRN *author page* per_id unknown — links point to paper page for now. Tommy can update to author page URL later.
- [x] **Title confirmation:** Confirmed "Some Respect Reserved" (CAELJ). All staging files updated 2026-02-15.
- [ ] **Update `data/README.md` in staging** — still references two-file architecture. Needs rewrite for single canonical CSV.
