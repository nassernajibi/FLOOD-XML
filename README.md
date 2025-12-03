# FLOOD-XML (Flood LOss and Observed Damage using eXplainable Machine Learning)
### A Multi-Model Ensemble Dataset of Global Flood Damages Since the 1980s Based on Explainable Machine Learning Frameworks ###

<img src="auxfiles/flood-xml-logo.jpg" alt="FLOOD-XML-LOGO" width="90%" style="display: block; margin: auto;" />

---

## User Manual (Documentation Guide)

### Overview

FLOOD-XML is an open-source dataset and modeling framework designed in accordance with FAIR (Findable, Accessible, Interoperable, Reusable) and CARE (Collective Benefit, Authority to Control, Responsibility, Ethics) principles. It generates global, event-level flood damage estimates using explainable machine learning (XML) models. The workflow harmonizes multi-decade flood archives (DFO, EM-DAT) with event attributes such as duration, affected area, deaths, centroid location, GDP, and main reported cause. Using these features, FLOOD-XML trains a six-model ensemble (MLR/LM, RF, XGB, SVR, BRNN, k-NN) to produce consistent economic loss estimates (USD) for thousands of events from 1985 to the present.

---

## Getting Started

### Clone the repository

    git clone https://github.com/nassernajibi/FLOOD-XML.git
    cd FLOOD-XML

### Install required R packages

    install.packages(c(
      "readxl","reshape2","hydroGOF","psych","caret",
      "countrycode","lubridate","xgboost","plyr",
      "e1071","kernlab","brnn","stringr","ncdf4"
    ))

### Run a demo workflow

    setwd("path/to/FLOOD-XML")
    source("demo/demo_floodxml_workflow.R")

---

## Workflow Description

### 1. Preprocessing (functions/, programs/)
Processes EM-DAT and DFO archives, harmonizes features, computes durations, merges GDP, and generates ML-ready training arrays.

### 2. ML Model Training
Trains six ML models (LM, RF, XGB, SVR, BRNN, k-NN) on EM-DAT (or DFO'15 and/or combined datasets). Outputs predicted economic losses (USD) per event for DFO'21.

### 3. Explainability Analysis
SHAP, LIME, variable importance, partial dependence diagnostics, model comparison, and optional ANOVA reasoning.

### 4. Product Generation (product/)
Generates FLOOD-XML CSV files and NetCDF files including:
- ML damage estimates  
- time_start and time_end  
- deaths, area, duration, lat, lon, GDP  
- country and MainCause stored as metadata  
- EM-DAT dismagvalue stored when available  

### 5. Tutorials and Examples (demo/, tutorials/)
Step-by-step workflows and reproducible analyses.

---

## Usage Notes and Limitations

- FLOOD-XML provides **event-level** loss estimates, not building-level or grid-level losses.  
- Input archives include uncertainties; predictions inherit these limitations.  
- ML outputs are statistical estimates, not authoritative valuations.  
- Not intended for real-time operational forecasting or applications.  
- Users must update file paths inside scripts when running workflows.

---

## Code Availability

### functions/
Reusable R functions for preprocessing, feature engineering, ML model training, ensemble prediction, and NetCDF generation.

### programs/
Full end-to-end workflow scripts (training, prediction, evaluation).

### product/
Scripts for producing final FLOOD-XML datasets (CSV + NetCDF).

### demo/ and tutorials/
Reproducible examples and user-guided learning scripts.

---

## Data Availability

Raw input datasets are NOT included due to licensing. Users must obtain:
- EM-DAT (CRED)  
- Dartmouth Flood Observatory (DFO)  
- World Bank GDP  

A public DOI for FLOOD-XML outputs will be added upon dataset release.

---

## Acknowledgments

Developed by:

Nasser Najibi  
Assistant Professor  
Agricultural & Biological Engineering  
University of Florida  
Director, Climate Resilience Lab  

Thanks to EM-DAT, DFO, and World Bank contributors.

---

## Citation

Please cite FLOOD-XML as:

Najibi, N. (20XX). FLOOD-XML: Flood LOss and Observed Damage using eXplainable Machine Learning. GitHub Repository. https://github.com/nassernajibi/FLOOD-XML

BibTeX:

    @misc{Najibi_FLOODXML,
      author       = {Najibi, Nasser},
      title        = {FLOOD-XML: Flood LOss and Observed Damage using eXplainable Machine Learning},
      year         = {20XX},
      howpublished = {\url{https://github.com/nassernajibi/FLOOD-XML}},
      note         = {Version X.X}
    }

---

## License

This project is released under the GNU GPL-3.0 License.  
See the LICENSE file for full terms.

---

## Repository Structure

    FLOOD-XML/
    ├─ auxfiles/
    │  └─ flood-xml-logo.jpg
    │
    ├─ demo/
    │  └─ demo_*.R
    │
    ├─ functions/
    │  └─ *.R
    │
    ├─ inputs/
    │  └─ (user-provided datasets)
    │
    ├─ product/
    │  └─ *.R   # CSV → NetCDF, metadata embedding
    │
    ├─ programs/
    │  └─ workflow_*.R
    │
    ├─ tutorials/
    │  └─ *.R / *.Rmd
    │
    ├─ LICENSE
    └─ README.md

---

## Disclaimer

You are running the scripts/functions, which means you will not blame the author(s) if they break your stuff. The scripts/functions are provided AS IS without warranty of any kind. Author(s) disclaim all implied warranties, including merchantability or fitness for a particular purpose. Author(s) shall not be liable for any damages, losses, or consequences resulting from the use or inability to use this code. The contents do not represent the views of funding agencies, affiliated universities, or the U.S. Government.
