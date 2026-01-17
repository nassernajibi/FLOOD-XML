# FLOOD-XML (Flood LOss and Observed Damage using eXplainable Machine Learning)

[![Visitor Count](https://visitor-badge.laobi.icu/badge?page_id=nassernajibi.FLOOD-XML)](https://github.com/nassernajibi/FLOOD-XML)
[![GitHub stars](https://img.shields.io/github/stars/nassernajibi/FLOOD-XML?style=social)](https://github.com/nassernajibi/FLOOD-XML)

## A Multi-Model Ensemble Dataset of Global Flood Damages Since the 1980s Based on Explainable Machine Learning Frameworks ###

<img src="auxfiles/logo_GitHub.jpg" alt="FLOOD-XML-logo" width="95%" style="display: block; margin: auto;" />

---

## User Manual (Documentation Guide)

### Overview

FLOOD-XML is an open-source dataset and machine-learning (ML) based modeling framework designed in accordance with FAIR (Findable, Accessible, Interoperable, Reusable) and CARE (Collective Benefit, Authority to Control, Responsibility, Ethics) principles. It generates global, event-level flood damage estimates using explainable machine learning (XML) models. The workflow harmonizes multi-decade flood archives (DFO: https://floodobservatory.colorado.edu; EM-DAT: https://www.emdat.be) with event attributes, including duration, affected area, deaths, centroid location, GDP, and the main reported cause. Using these features, FLOOD-XML trains a six-model ensemble (MLR/LM, RF, XGB, SVR, BRNN, k-NN) to produce consistent economic loss estimates (USD) for thousands of events worldwide from 1985 to the present.

---

## Getting Started

### Clone the repository

    git clone https://github.com/nassernajibi/FLOOD-XML.git
    cd FLOOD-XML


### Run a demo workflow

    setwd("path/to/FLOOD-XML")
    source("demo/demo_floodxml_workflow.R")

---

## Workflow Description

### 1. Preprocessing (auxfiles/)
Processes EM-DAT and DFO archives, harmonizes features, computes durations, merges GDP, and generates ML-ready training arrays.

### 2. ML Model Training (programs/, functions/)
Trains six ML models (LM, RF, XGB, SVR, BRNN, k-NN) on EM-DAT data (or DFO'15 and/or combined datasets) based on the flood damage scaling functions. Outputs predicted economic losses (USD) per event for DFO'21 (and EM-DAT).

### 3. Explainability Analysis (programs/)
SHAP, LIME, variable importance, partial dependence diagnostics, model comparison metrics, and optional ANOVA reasoning.

### 4. Product Generation (product/)
Generates FLOOD-XML CSV files and NetCDF files including:
- ML damage estimates  
- starting and ending event dates
- deaths, area, duration, lat, lon, GDP  
- country and MainCause stored as metadata  
- EM-DAT dismagvalue stored when available  

### 5. Tutorials and Examples (demo/)
Step-by-step workflows and reproducible analyses.

---

## Usage Notes and Limitations

- FLOOD-XML provides **event-level** loss estimates, not building-level or grid-level losses.  
- Input archives include uncertainties; predictions inherit these limitations.  
- ML outputs are statistical (data-driven) estimates, not authoritative valuations.  
- Not intended for real-time operational forecasting or applications.  
- Users must update file paths inside scripts when running workflows.

---

## Code Availability

### programs/
Full end-to-end workflow scripts (training, prediction, evaluation).
Scripts for producing final FLOOD-XML datasets (CSV + NetCDF).

### functions/
Reusable R functions for preprocessing, feature engineering, ML model training, ensemble prediction, and NetCDF generation.

### demo/
Reproducible examples and user-guided learning scripts.

---

## Data Availability

This repository includes the processed FLOOD-XML outputs (CSV and NetCDF).
Users are responsible for complying with the licensing and use restrictions of the underlying raw data sources (EM-DAT, DFO, World Bank GDP).
A public DOI for the finalized FLOOD-XML dataset will be assigned and added here upon formal release.

---

## Acknowledgments

This work was conducted as a part of the (Insert your Working Group Title) Working Group supported by the John Wesley Powell Center for Analysis and Synthesis, funded by the U.S. Geological Survey [and other funding sources if appropriate].
Thanks to EM-DAT, DFO, and World Bank contributors.

<img src="auxfiles/PowellCenter_emblem.jpg" alt="Powell-Center-logo" width="25%" style="display: block; margin: auto;" />



---

## Citation

Please cite FLOOD-XML as:

XXXX. FLOOD-XML: Flood LOss and Observed Damage using eXplainable Machine Learning. GitHub Repository. https://github.com/nassernajibi/FLOOD-XML

BibTeX:

    @misc{XXX,
      author       = {XXXX},
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
    │  └─ *.R (ML auxiliary functions)
    │
    ├─ inputs/
    │  └─ (user-provided datasets: e.g., EM-DAT and DFO)
    │
    ├─ product/
    │  └─ *.nc   .csv → NetCDF, CSV files
    │
    ├─ programs/
    │  └─ 1_...*.R ... 4_...*.R
    │
    ├─ LICENSE
    └─ README.md

---

## Disclaimer

You are running the scripts/functions, which means you will not blame the author(s) if they break your stuff. The scripts/functions are provided AS IS without warranty of any kind. Author(s) disclaim all implied warranties, including merchantability or fitness for a particular purpose. Author(s) shall not be liable for any damages, losses, or consequences resulting from the use or inability to use this code. The contents do not represent the views of funding agencies, affiliated universities, or the U.S. Government.
