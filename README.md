# CWSC_gridded_based_monthly_water_balance_models

This code contains fortran code used to calibration of monthly water balance models. Water balance models can estimate daily, monthly, and annual hydrological variables and processes by considering soil moisture. 

Most monthly water balance models have the concept of a water tank model. Three monthly water balance models are selected for the Soil moisture storage capacity (SMSC) parameter, i.e., Dynamic Water Balance Model (DWBM), Snowbelt-based Water Balance Model (SWBM), and Time-variant Gain Model (TVGM).

Major code contributor: Kang Xie (PhD Student, Wuhan University), Liting Zhou (PhD Student, Wuhan University) and Shujie Cheng (PhD Student, Wuhan University)


Abstract

Catchment water storage capacity (CWSC) links the atmosphere and terrestrial ecosystems, which is required as spatial parameters for geoscientific models. However, there are currently no available common datasets of the CWSC on a global scale, especially for hydrological models since conventional evapotranspiration-derived estimates cannot represent the extra storage capacity for the lateral flow and runoff generation. Here, we produce a dataset of the CWSC parameter for global hydrological models. Joint parameter calibration of three commonly used monthly water balance models provides the labels for a deep residual network. The global CWSC is constructed based on the deep residual network at 0.5° resolution by integrating 15 types of meteorological forcings, underlying surface properties, and runoff data. CWSC products are validated with the spatial distribution against root zone depth datasets and validated in the simulation efficiency on global grids and typical catchments from different climatic regions. We provide the global CWSC parameter dataset as a benchmark for geoscientific modelling by users.


Citations

If you find our code to be useful, please cite the following papers:

Xie, K. et al. Identification of spatially distributed parameters of hydrological models using the dimension-adaptive key grid calibration strategy - ScienceDirect. Journal of Hydrology 598, doi:10.1016/j.jhydrol.2020.125772 (2020).

Xie, K. et al. Physics-guided deep learning for rainfall-runoff modeling by considering extreme events and monotonic relationships. Journal of Hydrology 603, doi:10.1016/j.jhydrol.2021.127043 (2021).

Xie, K. et al. Verification of a New Spatial Distribution Function of Soil Water Storage Capacity Using Conceptual and SWAT Models. Journal of Hydrologic Engineering 25, doi:10.1061/(asce)he.1943-5584.0001887 (2020).


