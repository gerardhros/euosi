# euosi 0.4.0 2025-08-12

## Added
* vignette `eu_osi_for_potassium` with an introduction to potassium
* vignette `eu_osi_for_nitrogen` with an introduction to nitrogen

## Fixed
* `osi_c_potassium_de` for wrong P soil test in German evaluation of K supply
* `osi_c_potassium_lv` for wrong USDA soil classification for sand
* `osi_c_potassium_nl` for wrong reference to internal dt.maize subset
* `osi_c_potassium_uk` uses temporary crop name fix 

## Changed
* `osi_b_pmn_nl` has now BoBi correction resulting in more variation due to texture
* thresholds for evaluation soil N supply for `osi_c_nitrogen_de` and `osi_c_nitrogen_eu`

# euosi 0.3.0 2025-08-11

## Added
* vignette `eu_osi_introduction` with a short introduction
* vignette `eu_osi_variables` with a summary of required input and output variables used
* vignette `eu_osi_aggregation` to illustrate the aggregation method of the soil health assessment
* `osi_field_dt` to run `osi_field` with a data.table as input
* `osi_farm` as package table to illustrate functionality of euosi 
* `osi_vars_input` and `osi_vars_input` as package tables, prepared in `dev/osi_tables.R`
* `osi_farm` as package table with default inputs to illustrate functions of euosi

## Changed
* rename vignette on P to `eu_osi_for_phosphor`

## Fixed
* missing B_COUNTRY reference to K functions in `osi_field`

## Removed
* vignettes for B, Cu, Mg, N, pH, Zn and K given that these were more working documents than vignettes.

# euosi 0.2.0 2025-08-08

## Added
* `osi_plot_shi` to plot the SHI score for one or more groups

## Changed
* vignette updated for `osi_c_phosphor` 
* P threshold values for P-CAL in `osi_c_phosphor_de` given VDLUFA update in 2020
* update threshold values for Belgium and Finland

## Fixed
* small bug fixes in `osi_c_phosphor_nl` for unrealistic PBI values
* add temporary fixes for missing land use code for IE, SE, and UK where P-index depends on land use

# euosi 0.1.0 2025-05-27

## Added
* `osi_c_nitrogen` to assess soil N supply for countries BE, DE, FR, NL and EU 
* `osi_c_p` to assess soil P supply for countries AT, BE, CH, CZ, DE, DK, EE, ES, FI, FR, HU, IE, IT, LV, LT, NO, NL, PL, SE, SK, SL and UK
* `oci_c_k` to assess soil K supply for all countries
* `oci_c_mg` to assess soil Mg supply for all countries
* `osi_c_b` to assess soil B supply for countries CH, DE, IE, FR, NL, SE and UK
* `osi_c_zn` to assess Zn availability for countries DE, FR, IE, NL and UK
* `osi_nut_p` and `osi_nut_k` to assess excess of P and K for all 22 countries
* `osi_carbon` for soil carbon index, for application in EU, not country specific
* `osi_biodiversity` for conditions favoring soil biodiversity, for application in EU, not country specific
* `osi_conv_phosphor`, `osi_conv_potassium` and `osi_conv_magnesium` to convert LUCAS properties to extractable nutrient pools
* `osi_p_crumbleability` for application in EU, not country specific
* `osi_p_wef` to assess wind erosion risk, applicable for whole EU
* `osi_p_whc` to assess water holding capacity, plant available water and permeability, applicable for whole EU
* `osi_density` to evaluate impact of soil density on rootability, not country specific
* `osi_gw_nleach` for countries Be, FR, FI and EU to assess nitrate leaching risk
* `osi_c_ph` to assess soil pH for countries AT, BE, CH, DE, FR, FI, IE, NL, SE and UK
* `osi_b_pmn` to assess potentially mineralizable N for NL and EU
* `osi_clim`, an internal package table with climatic data per country
* `osi_erosion` to estimate water erosion using RUSL approach, applicable for whole EU
* `get_TEXTURE_GEPPA` to derive soil texture following French classification
* `get_TEXTURE_HYPRES` to derive soil texture following HYPRES classification
* `get_TEXTURE_BE` to derive soil texture following Belgium classification

## Changed
* `osi_crops` for countries BE, FI and FR
* `osi_thresholds` for countries BE, FI and FR
* `get_TEXTURE_USDA` allows names and codes to extract
* `osi_field` to run EU OSI on LUCAS dataset

# euosi 0.0.7 2024-01-09 

## Added
* logistic curve plus gaussian decline after reaching high values in `osi_evaluate_logistic_gaus_down`
* conversion methods to estimate soil pH from pH-water, pH-CaCl2 and pH-KCL
* conversion methos to estimate B-hot water from B-CaCl2
* conversion methods for SOM, SOC, SON and CN ratio
* conversion method for Potentially Mineralizable Nitrogen

## Changed
* add possibility to lower score at high values in `osi_evaluate_parabolic`

# euosi 0.0.6 2024-01-09

## Added
* `osi_c_mg_fr`, `osi_c_mg_nl` and `osi_c_mg` to assess the soil function to supply magnesium in France and the Netherlands 
* thresholds for magnesium in `osi_thresholds` for Mg soil assessment in France

# euosi 0.0.5 2024-01-09

## Changed
* nitrogen supply in France has been corrected in `osi_c_n_fr` for length of growing season

# euosi 0.0.4 2024-01-09

## Added
* `osi_c_b_fr` and `osi_c_b` to assess the soil function to supply boron in France 
* `osi_c_cu_fr` and `osi_c_cu` to assess the soil function to supply Copper in France 
* `osi_c_ph_fr` and `osi_c_ph` to assess the the pH in view of a desired target 
* three vignettes describing background information regarding these functions

## Changed
* parameters in `osi_parms` and `osi_parameters.csv` updated with new ones for pH, Cu and B for France

# euosi 0.0.3 2024-01-09

## Added
* vignette for input and functions for OSI in France, `description_osi_parameters_france`
* thresholds for Cu in `osi_thresholds` for Cu soil assessment in France

## Changed
* parameters in `osi_parms` and `osi_parameters.csv` updated with new ones for France

# euosi 0.0.2 2024-01-09

## Added
* `osi_c_nitrogen_fr` to assess the soil function to supply N in France
* `osi_c_posphor_fr` to assess the soil function to supply P in France
* `osi_c_potassium_fr` to assess the soil function to supply K in France
* `osi_c_zinc_fr` to assess the soil function to supply Zn in France
* French soil types and regions in `osi_soiltype`
* a series of vignettes describing the background for these functions
* a series of unit tests 

## Changed
* update package table `osi_soiltype`, `osi_thresholds` and `osi_crops`
* update wrapper funcions `osi_c_nitrogen`, `osi_c_phosphor`, `osi_c_potassium`,and `osi_c_zinc`


# euosi 0.0.1 2023-02-17
First version of euosi

## Added
* `osi_c_nitrogen_nl` to assess the soil function to supply N in the Netherlands
* `osi_c_posphor_nl` to assess the soil function to supply P in the Netherlands
* `osi_c_potassium_nl` to assess the soil function to supply K in the Netherlands
* `osi_c_zinc_nl` to assess the soil function to supply Zn in the Netherlands
* `osi_b_pmn_nl` and `osi_b_pmn` to assess the microbial activity of a soil
* `osi_p_whc` to assess the capacity of soils to retain water
* `osi_p_wef` to assess the risk for wind erodibility
* `osi_p_density` to assess impact of soil density on soil functioning
* helper functions `osi_evaluate_logistic`, `osi_evaluate_parabolic` and `cf_ind_importance` 
* wrapper function `osi_c_nitrogen` 
* wrapper function `osi_c_phosphor`
* wrapper function `osi_c_potassium`
* wrapper function `osi_c_zinc`
* wrapper function `osi_main`
