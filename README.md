# FLOOD-XML (Flood LOss and Observed Damage using eXplainable Machine Learning)

[![Visitor Count](https://visitor-badge.laobi.icu/badge?page_id=nassernajibi.FLOOD-XML)](https://github.com/nassernajibi/FLOOD-XML)

## A Multi-Model Ensemble Dataset of Global Flood Damages Since the 1980s Based on Explainable Machine Learning Frameworks ###

<img src="auxfiles/logo_GitHub.jpg" alt="FLOOD-XML-logo" width="95%" style="display: block; margin: auto;" />

---

## User Manual (Documentation Guide)

### 🗺️ Overview

FLOOD-XML is an open-source dataset and AI/ML-based modeling framework designed in accordance with FAIR (Findable, Accessible, Interoperable, Reusable) and CARE (Collective Benefit, Authority to Control, Responsibility, Ethics) principles. It generates global, event-level flood damage estimates using explainable machine learning (XML) models. The workflow harmonizes multi-decade flood archives (DFO: https://floodobservatory.colorado.edu; EM-DAT: https://www.emdat.be) with event attributes, including duration, affected area, deaths, centroid location, GDP, and the main reported cause. Using these features, FLOOD-XML trains a six-model ensemble (MLR/LM, RF, XGB, SVR, BRNN, k-NN) to produce consistent economic loss estimates (USD) for thousands of events worldwide from 1985 to the present.

### 🌊 FLOOD-XML Mapper
FLOOD-XML Mapper is an interactive web-based visualization and exploration portal developed to support transparent access, quality control, and stakeholder-oriented use of the FLOOD-XML dataset.
The Mapper enables users to dynamically explore global, event-level flood damage estimates in space and time, query individual flood events, and visualize associated attributes such as economic losses (across different ML models and multi-model median), duration, affected area, fatalities, reported causes, and time series of events.

🌍 Launch FLOOD-XML Portal → https://nassernajibi.github.io/FLOOD-XML-Mapper

---

## Getting Started

### Clone the repository

    git clone https://github.com/nassernajibi/FLOOD-XML.git
    cd FLOOD-XML


### Access/Run workflow

    setwd("path/to/FLOOD-XML")
    ./programs

## Workflow Description


### Repository Structure

    FLOOD-XML/
    ├─ auxfiles/
    │  └─ flood-xml-logo.jpg
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
	
### 1. Preprocessing (./auxfiles)
Processes EM-DAT and DFO archives, harmonizes features, computes durations, merges GDP, and generates ML-ready training arrays.

### 2. ML Model Training (./programs/, ./functions)
Trains six ML models (LM, RF, XGB, SVR, BRNN, k-NN) on EM-DAT data (or DFO'15 and/or combined datasets) based on the flood damage scaling functions. Outputs predicted economic losses (USD) per event for DFO'21 (and EM-DAT).

### 3. Explainability Analysis (./programs)
SHAP, LIME, variable importance, partial dependence diagnostics, model comparison metrics, and optional ANOVA reasoning.

### 4. Product Generation (./product)
Generates FLOOD-XML CSV files and NetCDF files including:
- ML-based flood damage estimates for a six-model ensemble  
- starting and ending event dates
- deaths, area, duration, lat, lon, GDP  
- country and MainCause stored as metadata  
- EM-DAT dismagvalue stored when available  

### 5. Tutorials and Examples (README.md)
Step-by-step workflows and reproducible analyses.

---

## Usage Notes and Limitations

- FLOOD-XML provides **event-level** loss estimates, not building-level or grid-level losses.  
- Input archives include uncertainties; predictions inherit these limitations.  
- ML outputs are statistical (data-driven) estimates, not authoritative valuations.  
- Not intended for real-time operational forecasting or applications (currently).  
- Users must update file paths inside scripts when running workflows.

---

## Code/Demo Availability

### ./programs
Full end-to-end workflow scripts (training, prediction, evaluation)
Scripts for producing final FLOOD-XML datasets (CSV + NetCDF)

### ./functions
Reusable R functions for preprocessing, feature engineering, ML model training, ensemble prediction, and NetCDF generation

### ./demo
(ongoing)


## Data Availability

This repository includes the processed FLOOD-XML outputs (CSV and NetCDF).
Users are responsible for complying with the licensing and use restrictions of the underlying raw data sources (EM-DAT, DFO, World Bank GDP).
A public DOI for the finalized FLOOD-XML dataset in HydroShare is assigned and added here (see Citation)

---

## Citation

Najibi, N., Hwang, J., Wang, D., Magers, B., Sankarasubramanian, A., Archfield, S., & Devineni, N. (2026). FLOOD-XML: A multi-model ensemble dataset of global flood damages since the 1980s based on explainable machine learning frameworks (Manuscript submitted for publication). Nature Scientific Data.

FLOOD-XML: Flood LOss and Observed Damage using eXplainable Machine Learning.GitHub Repository. https://github.com/nassernajibi/FLOOD-XML

FLOOD-XML v.1.0 (Flood LOss and Observed Damage using eXplainable Machine Learning), HydroShare, https://doi.org/10.4211/hs.43734306345c41358617d90800d12819

BibTeX:

    @misc{NajibiFLOODXML2026,
	author       = {Najibi, Nasser and Hwang, Jeongwoo and Wang, Dingbao and Magers, Bailey and Sankarasubramanian, A. and Archfield, Stacey and Devineni, Naresh},
	title        = {FLOOD-XML: Flood LOss and Observed Damage using eXplainable Machine Learning},
	year         = {2026},
	howpublished = {\url{https://github.com/nassernajibi/FLOOD-XML}},
	note         = {Version 1.0}
    }


## License

This project is released under the GNU GPL-3.0 License.  
See the LICENSE file for full terms.



## Acknowledgments

This work was supported by the US Department of the Interior, U.S. Geological Survey (USGS) grant number G25AC00204-00.
This work was conducted as a part of the “Global Flood Impacts” Working Group, supported by the John Wesley Powell Center for Analysis and Synthesis, funded by the USGS.
Any opinions, findings, and conclusions or recommendations expressed in this material do not necessarily reflect the views of the funding agency.

<img src="auxfiles/PowellCenter_emblem.jpg" alt="Powell-Center-logo" width="25%" style="display: block; margin: auto;" />



## Disclaimer

You are running the scripts/functions, which means you will not blame the author(s) if they break your stuff. The scripts/functions are provided AS IS without warranty of any kind. Author(s) disclaim all implied warranties, including merchantability or fitness for a particular purpose. Author(s) shall not be liable for any damages, losses, or consequences resulting from the use or inability to use this code. The contents do not represent the views of funding agencies, affiliated universities, or the U.S. Government.
