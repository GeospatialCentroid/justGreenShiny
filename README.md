# JustGreen Shiny Application

Welcome to the **JustGreen** Shiny application repository. This tool explores and visualizes the health impacts of urban green spaces across the 200 most populated cities in the United States. 

## Primary Functionality

JustGreen estimates the health benefits of current vegetation—such as parks, gardens, lawns, and other green spaces—and provides insights into where cities can invest more fairly to address environmental and health inequities. 

The application is built using R and Shiny, and it answers three primary questions:
1. How many deaths, strokes, and dementia cases does current vegetation prevent?
2. How many more could be prevented if a city increased green cover by 10%?
3. Where should cities add vegetation to reach the most people and address social vulnerabilities?

### Key Metrics Measured
- **NDVI (Greenness):** The Normalized Difference Vegetation Index captures chlorophyll in living plants from ESA Sentinel-2 satellite imagery, accounting for all vegetation types.
- **Health Estimates:** Modeled estimates of lives saved, stroke cases prevented, and dementia cases prevented based on peer-reviewed exposure-response relationships.
- **Social Vulnerability Index (SVI):** Using CDC data to identify socioeconomic disadvantages and map the overlap between high vulnerability and low green space, helping cities target investments.

## Application Structure

The JustGreen application is divided into three main sections:

### 1. 200 City Overview
A national-level interactive map displaying greenness levels and health outcomes for all 200 cities. 
- **Features:** Users can select different health metrics (Current Vegetation Levels, Lives Saved, Stroke Cases Prevented, Dementia Cases Prevented) and choose a specific city to see high-level summary statistics and interactive gauge charts.
- **Purpose:** To see how cities compare nationally and identify regional patterns.

### 2. City Review
A localized, granular view that breaks down data by census tract for a selected city.
- **Features:** Users can visualize tract-level disparities in vegetation, health outcomes, and Social Vulnerability (RPL). Clicking on individual tracts reveals detailed local statistics.
- **Purpose:** To reveal within-city inequities that aggregate statistics might hide, aiding in localized urban planning.

### 3. About
A comprehensive informational page detailing the tool's scientific basis, methodology, data sources, and limitations.

## Expected Outputs for End Users

The application provides the following outputs for users (individuals, municipalities, urban planners, and community groups):

1. **Interactive Maps and Visualizations:** Dynamic maps at both the national (city-level) and local (census tract-level) scales to visually identify trends and priority areas.
2. **Downloadable City Reports:** Users can download a comprehensive, tailored HTML report for any selected city. These reports summarize:
   - Current greenness levels and national ranking.
   - Annual health benefits derived from existing vegetation.
   - Projected benefits from a theoretical 10% increase in vegetation.
   - Identification of priority tracts for greening investment based on equity and vulnerability analysis.

These reports are designed to be actionable documents that can be shared at council meetings, used in grant applications, or attached to public comments on development projects.

## Development Team

JustGreen was built collaboratively by the **Rojas Lab** and the **Geospatial Centroid** at Colorado State University.

- **Principal Investigator:** David Rojas, PhD (Associate Professor of Environmental Epidemiology)
- **Lead Developer:** Dan Carver (Technical Manager, Geospatial Centroid)

## Data Sources
All supporting data for this application is generated via the [justGreen data repository](https://github.com/GeospatialCentroid/justGreen). The core inputs include ESA Sentinel-2 imagery for vegetation, US Census ACS for population data, CDC SVI for social vulnerability, and CDC mortality rates.
