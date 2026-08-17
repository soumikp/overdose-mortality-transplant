# overdose-mortality-transplant

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![R](https://img.shields.io/badge/R-%3E%3D4.0-blue.svg)](https://www.r-project.org/)

Analytic code and data accompanying **"Drug Overdose Mortality Decline and Deceased Donor Transplantation in the United States"** (Sethi et al., *American Journal of Transplantation*).

> **Note:** This repository was previously named `2025_srtr`. The published data-availability statement cites `https://github.com/soumikp/2025_srtr/tree/main`, which GitHub redirects here automatically.

## Background

Between 2009 and 2019, drug overdose deaths accounted for roughly 42% of the growth in deceased organ donors in the United States. National data indicate a ~27% decline in overdose mortality from 2023 to 2024 — the most substantial reduction in decades. This study asks whether that public health achievement carried unintended consequences for transplantation.

Using Organ Procurement and Transplantation Network (OPTN) data from January 2015 to December 2024, we analyzed trends in deceased donor kidney transplant (DD-KT) and deceased donor liver transplant (DD-LT) rates, alongside active waitlist size.

**Findings.** Overdose decedent donor (ODD) transplant rates rose until mid-2023, when they comprised nearly 1 in 5 DD-KTs and DD-LTs nationally. Rates then declined by 45.1% for DD-KTs and 37.3% for DD-LTs. Two long-standing national trends were disrupted concurrently: the overall DD-KT rate, rising since 2015, plateaued; and the active kidney transplant waitlist, declining since 2015, expanded.

## Authors

| Author | Affiliation |
|---|---|
| Vrishketan Sethi, MD | Department of Surgery, University of Pittsburgh School of Medicine |
| [Soumik Purkayastha, PhD](https://github.com/soumikp) | Department of Biostatistics, University of Pittsburgh School of Public Health |
| Hao Liu, MD, PhD | Department of Surgery, Houston Methodist Hospital, Houston, TX |
| Francis Spitz, BS | Division of Abdominal Transplant Surgery, Department of Surgery, University of Pittsburgh School of Medicine |
| Abiha Abdullah | Department of Surgery, University of Pittsburgh School of Medicine |
| Berkay Demirors, MD | Department of Surgery, University of Pittsburgh School of Medicine |
| Ruy Jorge Cruz Jr., MD, PhD | Division of Abdominal Transplant Surgery, Department of Surgery, University of Pittsburgh School of Medicine |
| Sundaram Hariharan, MD | Renal Electrolyte Division of Nephrology, Department of Medicine, University of Pittsburgh School of Medicine |
| Chethan Puttarajappa, MD, MS | Renal Electrolyte Division of Nephrology, Department of Medicine, University of Pittsburgh School of Medicine |
| Michele Molinari, MD, MPH | Division of Abdominal Transplant Surgery, Department of Surgery, University of Pittsburgh School of Medicine |

**Corresponding author:** Michele Molinari.

Questions about the statistical code may be directed to [Soumik Purkayastha](https://github.com/soumikp) via a [GitHub issue](https://github.com/soumikp/overdose-mortality-transplant/issues).

## Repository layout

Each figure has its own self-contained folder under `code/submission 2/`, holding the data, the script that generates it, a `Cleaning.rtf` documenting how the input data were derived, and the resulting PDF.

```
code/submission 2/
  Figure 1/ … Figure 6/                    Main figures
  SupplementaryFigure 1/ … 4/              Supplementary figures
  Table 1.docx, Table 2.docx               Manuscript tables

  Each figure folder contains:
    <Figure N>.R        Script generating the figure
    <Figure N>.pdf      Figure as it appears in the manuscript
    Cleaning.rtf        Derivation of the input data
    data/               Input CSVs (and Joinpoint .jpo session files)
```

Most folders keep their inputs in a `data/` subfolder; Figure 5 and Supplementary Figure 1 hold their CSV at the folder root instead.

Figure 1 additionally includes `KidneyTransplantTrends_01.R` and `KidneyTransplantTrends_02.R`, which prepare the underlying kidney transplant trend series. Supplementary Figure 4 is split across `batchA.R` and `batchB.R`.

## Requirements

R (≥ 4.0). Scripts load dependencies through [`pacman`](https://cran.r-project.org/package=pacman), so install it first:

```r
install.packages("pacman")
```

The scripts then pull in what they need — `tidyverse` (`dplyr`, `ggplot2`, `readr`, `stringr`, `lubridate`), `lmtest`, `sandwich`, `patchwork`, and `here`.

Joinpoint regression was performed in the [NCI Joinpoint Regression Program](https://surveillance.cancer.gov/joinpoint/); the `.jpo` session files in the relevant `data/` folders reproduce those analyses.

## Usage

Paths are resolved with [`here`](https://here.r-lib.org/), which anchors to the project root — open `2025_srtr.Rproj` in RStudio, or run from the repository root, and the scripts locate their data without editing paths.

```r
source("code/submission 2/Figure 2/Figure 2.R")
```

Each script is independent; run whichever figure you want to reproduce.

## Data availability

The aggregate data needed to regenerate every figure are included in this repository. These are derived from OPTN/SRTR registry data, reported here at the aggregate level.

Access to the underlying registry requires contacting the Scientific Registry of Transplant Recipients (SRTR). The data reported here have been supplied by the Hennepin Healthcare Research Institute (HHRI) as the contractor for SRTR. The interpretation and reporting of these data are the responsibility of the authors and in no way should be seen as an official policy of or interpretation by the SRTR or the U.S. Government.

## Citation

Sethi V, Purkayastha S, Liu H, Spitz F, Abdullah A, Demirors B, Cruz RJ Jr, Hariharan S, Puttarajappa C, Molinari M. Drug Overdose Mortality Decline and Deceased Donor Transplantation in the United States. *American Journal of Transplantation*.

## License

MIT — see [LICENSE](LICENSE).
