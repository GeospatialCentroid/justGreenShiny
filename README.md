# JustGreen Shiny Application

[![Project Status: Active](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![R](https://img.shields.io/badge/R-276DC3?style=for-the-badge&logo=r&logoColor=white)](https://www.r-project.org/)
[![Shiny](https://img.shields.io/badge/Shiny-0769AD?style=for-the-badge&logo=r&logoColor=white)](https://shiny.posit.co/)

Welcome to the **JustGreen** Shiny application. This interactive tool visualizes the health impacts of urban vegetation across the 200 most populated cities in the United States, supporting evidence-based urban planning and environmental justice.

## 🌟 Application Purpose

JustGreen translates complex environmental epidemiology into actionable insights. It quantifies the "health shield" provided by current urban green spaces (parks, gardens, street trees) and models the potential benefits of future greening initiatives.

### Core Questions Answered
1. **Current Impact**: How many deaths, strokes, and dementia cases are prevented annually by existing city vegetation?
2. **Future Potential**: What are the projected health gains if a city increases its green cover by 10%?
3. **Equity & Priority**: Where are the "green deserts" that overlap with high social vulnerability, and where should new investments be prioritized?

---

## 🚀 Getting Started

### Prerequisites
Ensure you have [R](https://cran.r-project.org/) (>= 4.1.0) installed. 

### Installation
1. Clone this repository:
   ```bash
   git clone https://github.com/GeospatialCentroid/justGreenShiny.git
   cd justGreenShiny
   ```

2. Install the required R packages:
   ```R
   install.packages(c("shiny", "leaflet", "ggplot2", "plotly", "DT", 
                      "shinyBS", "RColorBrewer", "bslib", "shinycssloaders", 
                      "markdown", "sf", "dplyr"))
   ```

### Running the App
Run the application directly from RStudio by opening `app.R` and clicking **Run App**, or via the console:
```R
shiny::runApp()
```

---

## 📊 Key Metrics & Methodology

- **NDVI (Greenness)**: Uses ESA Sentinel-2 satellite imagery to calculate the Normalized Difference Vegetation Index, a proxy for vegetation density and health.
- **Health Modeling**: Estimates are based on peer-reviewed exposure-response relationships for all-cause mortality, stroke, and dementia.
- **SVI (Social Vulnerability Index)**: Incorporates CDC data to identify census tracts with higher socioeconomic disadvantages, ensuring an equity-first approach to urban greening.

---

## 🛠 Application Structure

The app is organized into three primary modules:

1. **200 City Overview**: A national dashboard for comparing cities across the US. Identify regional trends and see where your city ranks in terms of "Lives Saved per 100,000."
2. **City Review (Tract Level)**: A deep dive into local disparities. Toggle between health metrics and Social Vulnerability (RPL) to find high-impact investment areas within a specific city.
3. **Downloadable Reports**: Generate a tailored, actionable HTML report for any of the 200 cities, perfect for inclusion in policy briefs or community presentations.

---

## 💻 Technical Architecture (For Developers)

- **UI Framework**: Built with `bslib` using the "Minty" preset for a modern, responsive design.
- **Modularity**: The application uses a modular structure (`modules/`) for map logic and info panels, improving maintainability.
- **Performance**: Spatial data is optimized using the `sf` package and pre-loaded RDS objects to ensure fast tab switching and map rendering.
- **Styling**: Custom CSS in `www/styles.css` extends the CSU brand identity (Green & Gold) across all interactive components.

---

## 🤝 Development & Data

JustGreen is a collaborative effort between the **Rojas Lab** (Environmental Epidemiology) and the **Geospatial Centroid** at Colorado State University.

- **Principal Investigator**: David Rojas, PhD
- **Technical Lead**: Dan Carver

**Data Source**: All underlying data is processed and hosted via the [justGreen data repository](https://github.com/GeospatialCentroid/justGreen).
