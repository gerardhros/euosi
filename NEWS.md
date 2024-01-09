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
