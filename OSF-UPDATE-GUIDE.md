# OSF Project Update Guide

> Instructions for updating osf.io/snxyt. Do manually in the OSF web interface.
> **Do not push this file to GitHub.**

## 1. Update Project Metadata

**Settings → Project Details**

### Title
```
Creative Commons and Copyright Risk Perception: A National Survey Experiment
```

### Description
```
This project contains data, instruments, and preregistration materials for a preregistered national survey experiment examining how Creative Commons licensing affects public perceptions of copyright risk, legal consequences, and willingness to pay for creative works.

Using a between-subjects design, a nationally representative quota sample of 1,239 U.S. adults evaluated seven copyright scenarios — ranging from public domain works to streaming platform content — under three conditions: control (standard copyright notice), CC-BY license, and CC-BY-NC license. For each scenario, respondents rated the likelihood of legal consequences for reuse, the expected severity of those consequences, the severity they believed was appropriate, and a fair license price.

A shorter survey administered to all respondents evaluates the American public's recognition rates and knowledge of Creative Commons, alongside brand recognition for comparison brands (McDonald's, Firefox, and a novel logo) and internet skill self-assessment.

Key finding from the first publication: Creative Commons licenses do not reliably reduce perceived legal risk or expected consequences for reuse, and in some scenarios, licensed works are treated as more risky than unlicensed equivalents.

Publications using this data:
1. Thomas H. Rousse, "Some Respect Reserved," Cardozo Arts & Entertainment Law Journal (forthcoming 2026). Preprint: https://papers.ssrn.com/sol3/papers.cfm?abstract_id=5379423
2. A second paper examining respondent characteristics, copyright knowledge, and demographic patterns in legal risk perception is forthcoming.

Analysis code, transparency notebooks, and a project website are available at:
- GitHub: https://github.com/ludist/cc-survey
- Project website: https://ludist.github.io/cc-survey/
```

### Tags
Add each tag individually:
```
Creative Commons
copyright
survey experiment
open licensing
empirical legal studies
legal risk perception
between-subjects design
intellectual property
CloudResearch
MTurk
```

### License
Select: **CC-BY 4.0 International**

Set copyright holder: **Thomas H. Rousse**

### Category
Select: **Project**

---

## 2. Reorganize Files

### Create folders
In the OSF file browser, create two new folders:
- `data`
- `instruments`

### Move existing files into `instruments/`
- `CC_Connect_Survey_3-14-24.qsf` → `instruments/`
- `CC_Connect_Survey_3-13-24.qsf` → `instruments/`
- `CCConnect-Methods-Survey Exp.pdf` → `instruments/`

Leave `THR-CC-PREREG.md` at root level (or create a `preregistration/` folder).

### Upload new files

**To `data/` folder:**
- `ccSurveyWorkingFile3.csv` (from `Diss-Creative Commons/R/`)
- `ccSurveyWorkingFileB1.csv` (from `Diss-Creative Commons/R/`)
- `survey-codebook.json` (from `github-staging/materials/`)

**To `instruments/` folder:**
- `CC_Connect_Survey_3-15-24-corrected.qsf` (from `Diss-Creative Commons/Preregistration/`)

---

## 3. Update Wiki

**Wiki → Home**

Replace the default wiki content with:

```markdown
# Creative Commons and Copyright Risk Perception Survey

A preregistered national survey experiment (N=1,239) examining how Creative Commons licensing affects public perceptions of copyright risk.

## Quick Links

| Resource | Link |
|----------|------|
| **Preregistration** | [osf.io/kq93t](https://osf.io/kq93t) |
| **SSRN Preprint** | [Some Respect Reserved](https://papers.ssrn.com/sol3/papers.cfm?abstract_id=5379423) |
| **GitHub Repository** | [ludist/cc-survey](https://github.com/ludist/cc-survey) |
| **Project Website** | [ludist.github.io/cc-survey](https://ludist.github.io/cc-survey/) |

## Data Files

| File | Records | Description |
|------|---------|-------------|
| `data/ccSurveyWorkingFile3.csv` | 1,239 | Main dataset — all scenarios |
| `data/ccSurveyWorkingFileB1.csv` | 1,336 | EU scenario supplement (B1 replacement sample) |
| `data/survey-codebook.json` | 166 items | Machine-readable codebook extracted from QSF |

## Survey Instruments

| File | Description |
|------|-------------|
| `instruments/CC_Connect_Survey_3-15-24-corrected.qsf` | Final Qualtrics survey (JSON, importable) |
| `instruments/CC_Connect_Survey_3-14-24.qsf` | Pre-launch version |
| `instruments/CC_Connect_Survey_3-13-24.qsf` | Initial version |
| `instruments/CCConnect-Methods-Survey Exp.pdf` | Methods document |

## Experimental Design

Participants were randomly assigned to one of three conditions:

| Condition | Treatment |
|-----------|-----------|
| Control (C) | Standard copyright notice |
| Treatment A (TA) | CC-BY license abbreviation |
| Treatment B (TB) | CC-BY-NC license with full name + educational summary |

Seven copyright scenarios: Public Domain, Personal Use, Educational Use, Commercial Advertising, Large-Scale Copying, Wikipedia/Nonprofit, and Filesharing.

Four dependent measures per scenario:
1. Likelihood of legal consequences (7-point Likert)
2. Expected severity of consequences (7-point scale)
3. Normative severity — what consequences *should* apply (7-point scale)
4. Willingness to pay for a license (continuous, USD)

## Collection Details

- **Platform:** CloudResearch Connect (MTurk panel)
- **Date:** March 2024
- **Quota:** Nationally representative by gender, age, ethnicity
- **IRB:** Northwestern University (exempt determination)

## License

Data and materials: CC-BY 4.0. Survey stimulus images: CC0 (public domain).

## Citation

Rousse, Thomas H. (2026). Creative Commons and Copyright Risk Perception: A National Survey Experiment [Data set]. OSF. https://osf.io/snxyt/
```

---

## 4. Link GitHub Repository

**Settings → Add-ons → GitHub**

1. Enable the GitHub add-on
2. Authenticate with your GitHub account
3. Select repository: `ludist/cc-survey`
4. Import account: `ludist`

This will display the GitHub repo contents in the OSF file browser alongside OSF Storage.

---

## 5. Add ORCID

**Settings → Contributors**

If your ORCID (0009-0007-9705-3035) is linked to your OSF account, it will display automatically. If not:

1. Go to your OSF profile settings
2. Link your ORCID under "Connected Accounts"

---

## Checklist

- [ ] Update title
- [ ] Add description
- [ ] Add tags (10 tags)
- [ ] Set license to CC-BY-4.0
- [ ] Create `data/` and `instruments/` folders
- [ ] Move existing QSF and PDF files into `instruments/`
- [ ] Upload `ccSurveyWorkingFile3.csv` to `data/`
- [ ] Upload `ccSurveyWorkingFileB1.csv` to `data/`
- [ ] Upload `survey-codebook.json` to `data/`
- [ ] Upload `CC_Connect_Survey_3-15-24-corrected.qsf` to `instruments/`
- [ ] Update wiki with project overview and links
- [ ] Link GitHub repository via add-on
- [ ] Link ORCID to OSF profile
