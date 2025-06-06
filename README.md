
# Spatial Data Integration for Global Mine Land Use Analysis

This repository provides R scripts to integrate and analyze global mining land use datasets, linking spatial features to commodities and other mine operations information. It supports the preprocessing, clustering, and visualization of mining areas for environmental and resource management studies.

## Features

* **Data Integration**: Combines multiple mining land use and inventory datasets into a unified format.
* **Spatial Clustering**: Identifies clusters of mining activities based on spatial proximity and primary materials.
* **Commodity Linking**: Associates mining areas with specific commodities, facilitating commodity-based analyses.
* **Visualization**: Generates plots to visualize mining areas by country, biome, and materials, including treemaps and bar charts.

## Repository Structure

* `00-integrate-available-data.R`: Integrates various mining datasets into a cohesive dataset.
* `01-cluster-spatial-features.R`: Performs spatial clustering on mining features.
* `02-optimize-cluster-threshold.R`: Optimizes clustering thresholds for improved accuracy.
* `03-validate-cluster-results.R`: Validates the results of spatial clustering.
* `04-cluster-extensions-and-overview.R`: Extends clustering analysis and generates overview plots.

## Getting Started

1. **Data Preparation**: Several datasets are automatically downloaded. However, proprietary data must be placed in the `./tmp` folder. Fruther data can be integrated by adjusting `00-integrate-available-data.R`.
2. **Execution**: Run the scripts in numerical order to process the data step-by-step.

## License

This project is licensed under the GPL-3.0 License.

## Citation

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.15611393.svg)](https://doi.org/10.5281/zenodo.15611393)