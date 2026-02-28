# Setup Guide: From Staging to Live Repository

> Step-by-step instructions for creating the `ludist/cc-survey` repo.
> **Do not push this file to GitHub.**

## Prerequisites

- [x] GitHub CLI installed (`gh`) or browser access to github.com/ludist  [completion:: 2026-02-16]
- [ ] Git configured with your GitHub credentials
- [x] Quarto installed (verify: `quarto --version`)  [completion:: 2026-02-16]
- [x] R installed (verify: `Rscript --version`)  [completion:: 2026-02-16]

## Step 1: Create the GitHub Repository

```bash
# Option A: GitHub CLI
gh repo create cc-survey --public --description "Creative Commons and Copyright Risk Perception: A National Survey Experiment — Data & Analysis Compendium"

# Option B: Browser
# Go to github.com/new, name it "cc-survey", public, no template, no README (we have our own)
```

## Step 2: Initialize Local Repository

Choose a local working directory *outside* Google Drive (to avoid sync issues with .git):

```bash
# Create local repo
mkdir -p ~/projects/cc-survey
cd ~/projects/cc-survey
git init
git branch -M main
git remote add origin git@github.com:ludist/cc-survey.git
```

## Step 3: Copy Files from Staging

```bash
STAGING="/mnt/c/Users/Tommy/My Drive/10-19 Academic/10 Projects/Creative-Commons-Survey/github-staging"
VAULT_R="/mnt/c/Users/Tommy/My Drive/10-19 Academic/10 Projects/Creative-Commons-Survey/Diss-Creative Commons"

# --- New files from staging ---
cp "$STAGING/README.md" .
cp "$STAGING/LICENSE" .
cp "$STAGING/CITATION.cff" .
cp "$STAGING/_quarto.yml" .
cp "$STAGING/index.qmd" .
cp "$STAGING/.gitignore" .
cp -r "$STAGING/.github" .

# --- Data files ---
mkdir -p data
cp "$VAULT_R/R/ccSurveyWorkingFile3.csv" data/
cp "$VAULT_R/R/ccSurveyWorkingFileB1.csv" data/
cp "$STAGING/data/README.md" data/

# --- R analysis code (literate versions only) ---
mkdir -p R/functions
cp "$VAULT_R/R/functions/analysis_functions_literate.R" R/functions/
cp "$VAULT_R/R/functions/data_prep_literate.R" R/functions/
cp "$VAULT_R/R/functions/export_helpers_literate.R" R/functions/
cp "$VAULT_R/R/functions/master_analysis_literate.R" R/functions/
cp "$VAULT_R/R/functions/plotting_themes_literate.R" R/functions/
cp "$VAULT_R/R/functions/randomization_checks_literate.R" R/functions/
cp "$VAULT_R/R/functions/stats_helpers_literate.R" R/functions/

# --- Transparency notebook ---
cp "$VAULT_R/R/DataTransparency_Part1.qmd" .

# --- Materials ---
mkdir -p materials
cp "$VAULT_R/Preregistration/THR-CC-PREREG.md" materials/preregistration.md
cp "$VAULT_R/Preregistration/CC_Connect_Survey_3-15-24-corrected.qsf" materials/survey-instrument.qsf
cp "$STAGING/materials/IMAGES-LICENSE.md" materials/

# --- Vignette images (released as CC0 public domain) ---
mkdir -p materials/vignette-images
cp "$VAULT_R/Vignette Images/"*.{jpg,png} materials/vignette-images/ 2>/dev/null
# Review: only include the final versions used in the experiment

# --- Figures directory (empty, populated by rendering) ---
mkdir -p figures
```

## Step 4: Fix Internal Paths in DataTransparency_Part1.qmd

The notebook was written for the old directory structure where `.qmd` lived inside `R/`.
In the new repo structure, the `.qmd` is at root level. Update these paths:

| Old Path (in R/) | New Path (at root) |
|-------------------|--------------------|
| `functions/data_prep_literate.R` | `R/functions/data_prep_literate.R` |
| `functions/analysis_functions_literate.R` | `R/functions/analysis_functions_literate.R` |
| `functions/plotting_themes_literate.R` | `R/functions/plotting_themes_literate.R` |
| `functions/stats_helpers_literate.R` | `R/functions/stats_helpers_literate.R` |
| `functions/randomization_checks_literate.R` | `R/functions/randomization_checks_literate.R` |
| `functions/export_helpers_literate.R` | `R/functions/export_helpers_literate.R` |
| `functions/master_analysis_literate.R` | `R/functions/master_analysis_literate.R` |
| `ccSurveyWorkingFile3.csv` | `data/ccSurveyWorkingFile3.csv` |
| `ccSurveyWorkingFileB1.csv` | `data/ccSurveyWorkingFileB1.csv` |

Also check any `read.csv()` or `source()` calls in the R function files themselves — some may have hardcoded relative paths.

## Step 4b: Generate Codebook from QSF (for Transparency Notebook)

```bash
pip install qualtrics-utils
qualtrics-utils codebook materials/survey-instrument.qsf
# Outputs a JSON codebook with question IDs → text → answer choices
# Use this to build the variable dictionary in the transparency notebook
```

Alternatively, in R:
```r
# devtools::install_github("sumtxt/qsf")
library(qsf)
questions <- get_questions("materials/survey-instrument.qsf", df = TRUE)
write.csv(questions, "materials/survey-codebook.csv", row.names = FALSE)
```

## Step 5: Initialize renv

```bash
cd ~/projects/cc-survey
Rscript -e '
  install.packages("renv")
  renv::init()
  # Install the packages your analysis needs
  # renv::install(c("tidyverse", "knitr", "dunn.test", "effsize", ...))
  renv::snapshot()
'
```

This creates `renv.lock` (tracked) and `renv/` (gitignored).

## Step 6: Verify Local Render

```bash
quarto render
# Open _site/index.html in a browser
# Verify: landing page renders, notebook renders, data dictionary renders
# Verify: code-fold works, margin notes display, no broken links
```

## Step 7: Fill in Placeholders

Search all files for these placeholders and replace:

| Placeholder | Replace With |
|-------------|-------------|
| `osf.io/XXXXX` | Your actual OSF project URL |
| `SSRN_ID=XXXXXXX` | Your SSRN abstract ID |
| `0009-0007-9705-3035` | Your ORCID iD |
| `per_id=XXXXXXX` | Your SSRN author ID |

```bash
grep -rn "XXXXX\|XXXXXXX\|XXXX-XXXX" --include="*.md" --include="*.qmd" --include="*.cff" --include="*.yml"
```

## Step 8: Initial Commit & Push

```bash
git add -A
git commit -m "Initial research compendium: data, code, and transparency notebook

Preregistered experimental survey (N=1,239) examining Creative Commons
license effects on legal risk perception.

Publications:
- 'Some Respect Reserved' (CAELJ, forthcoming)
- Part 2 on respondent characteristics (forthcoming)

- Quarto website with interactive transparency notebook
- Full analysis code (R) with literate programming modules
- Anonymized survey data (main + EU supplement)
- Preregistration, data dictionary, and concordance table
- CC-BY-4.0 licensed"

git push -u origin main
```

## Step 9: Enable GitHub Pages

1. Go to `github.com/ludist/cc-survey/settings/pages`
2. Source: **GitHub Actions** (not "Deploy from a branch")
3. The `publish.yml` workflow will trigger on the next push to `main`
4. After ~5 minutes, your site will be live at `ludist.github.io/cc-survey/`

## Step 10: Create GitHub Profile README

1. Create repo `ludist/ludist` (special profile repo)
2. Copy `PROFILE-README.md` from staging → that repo's `README.md`
3. Push — it will appear on your `github.com/ludist` profile page

```bash
gh repo create ludist --public
cd ~/projects
git clone git@github.com:ludist/ludist.git
cd ludist
cp "$STAGING/PROFILE-README.md" README.md
git add README.md
git commit -m "Add profile README"
git push
```

## Step 11: Link OSF → GitHub

1. Go to your OSF project
2. Settings → Add-ons → Enable GitHub
3. Link the `ludist/cc-survey` repository
4. The OSF project page will show the GitHub repo in the file browser

## Step 12: Pin Repository

1. Go to `github.com/ludist`
2. Click "Customize your pins"
3. Pin `cc-survey` (and unpin `evennia` unless you want it visible)

## Post-Setup: Keeping Vault & Repo in Sync

The vault remains your working directory. The GitHub repo is the *public-facing subset*.
When you make changes:

1. Edit in vault (Positron / Claude Code)
2. Copy changed files to `~/projects/cc-survey/` using the mapping above
3. `cd ~/projects/cc-survey && git add -A && git commit -m "..." && git push`
4. GitHub Action re-renders and deploys

**Future enhancement:** You could write a simple sync script that copies the mapped files automatically. But manual is fine for now — you want to consciously choose what goes public.

## About PDF/DOCX Output

The website `_quarto.yml` only renders HTML. If you also need PDF or DOCX (e.g., for CAELJ editors), you can render those locally without changing the config:

```bash
quarto render DataTransparency_Part1.qmd --to pdf
quarto render DataTransparency_Part1.qmd --to docx
```

Or add a `_quarto-print.yml` profile for print output.
