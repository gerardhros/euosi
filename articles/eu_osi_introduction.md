# EU OSI introduction

## Introduction

The crucial role of healthy soil in achieving sustainable food
production and environment is increasingly recognized, as is the
importance of proper assessment of soil quality. Healthy soils are not
just a growing medium for crops, but they regulate and support essential
ecosystem services, such as water purification, carbon sequestration,
and nutrient cycling, and they provide habitats for biodiversity.
Improving and sustaining soil health are therefore key to sustainable
crop production. To date, many attempts have been made to develop
indices for assessing soil health, but an operational and reproducible
methodology to assess soil health has not been developed so far. There
is a broad consensus that multiple aspects of soils (e.g., chemistry,
structure, and biology) and their interactions need to be considered.
Further, various approaches have been proposed to translate soil
attributes into indicators and aggregate them into an index, including
(advanced) statistical methods and refined expert knowledge systems.
These approaches however are often highly dependent on costly soil
measures, are difficult to interpret, and not easily applicable across
spatial scales, ranging from field to regions. To overcome these
challenges, [Ros et
al. (2002)](https://pubs.acs.org/doi/10.1021/acs.est.2c04516#)
introduced a new soil assessment framework, being the Open Soil Index.

The OSI builds on extensive soil and agronomic research to maintain a
direct link between soil health indicators and the objective to achieve
a sustainable crop production, leveraging routine laboratory data and
public databases to make its large-scale application affordable. The OSI
framework has a modular design to allow for easy adjustment and
expansion, is developed in an open-source environment ([OBIC R
package](https://agrocares.github.io/Open-Bodem-Index-Calculator/)) to
assure transparency, and provides advice for field-level farming
practices. In this way, OSI strives to provide an operational soil
assessment that valorizes soil health and therewith promote sustainable
soil management.

From 2021 onwards, soil scientists, agronomy researchers, farm advisors
and extension service providers in the Netherlands collaborated to
expand the OSI approach to include multiple ecosystem services. Where
the OSI strongly focuses on the interpretion of soil health in view of
crop production, many other ecosystem services are recognized as crucial
societal objectives that soil can contribute. Relevant soil ecosystem
services from the Sustainable Development Goals (SDGs) include not only
delivery of healthy food (SDG2 and SDG3) but also clean and sufficient
water (SDG6), the mitigation of climate (SDG13), and the support for
biodiversity and protection of land degradation (SDG15). Engaging
farmers to consider these SDGs requires extension of the framework to
include more environmental soil functions. The newly developed framework
follows the conceptual approach of the OSI and expands the soil health
assessment in view of these ecosystem services. The soil health
indicator functions can be found in the [BLN R
package](https://agrocares.github.io/BLN/). This tutorial gives a short
explanation of the package focussing on the main functionalities of the
R package.

In 2023, Gerard Ros (WU and NMI) and Elise van den Eynde (From JRC)
discussed the possibility to expand the OSI framework to the European
scale given the need for scientific sound and standardized soil health
assessments for monitoring, policy support, and for integrative soil
recommendation systems used by farmers and farm advisors. This
ultimately led to the development of the [euosi R
package](https://gerardhros.github.io/euosi/).

## Importing dataset for research farm

We start with some (virtual) data being collected for a farms with
fields in the Netherlands, Belgium and Finland. The preparation of this
file is described in `dev/osi_farm.R`.

The dataset contains soil properties from 60 agricultural fields and is
documented in
[`?osi_farm`](https://gerardhros.github.io/euosi/reference/osi_farm.md).
An overview of all variables is given below. When interested, look to
the `summary` of the `osi_farm` object.

More information on the input and ouput variables are documented in a
separate vignette
[`vignette("eu_osi_variables")`](https://gerardhros.github.io/euosi/articles/eu_osi_variables.md).

``` r
# select the data for de euosi farm
dt.of <- copy(euosi::osi_farm)

# show the first line with all properties for a single field and latest year
print(colnames(dt.of[1]))
```

    ##  [1] "ID"             "B_COUNTRY"      "B_LU"           "B_SOILTYPE_AGR"
    ##  [5] "B_PREC_SUM"     "B_PREC_WIN"     "B_PET_SUM"      "B_PET_WIN"     
    ##  [9] "B_TEMP_SUM"     "B_TEMP_WIN"     "A_SOM_LOI"      "A_CLAY_MI"     
    ## [13] "A_SAND_MI"      "A_PH_CC"        "A_CACO3_IF"     "A_CEC_CO"      
    ## [17] "A_C_OF"         "A_N_RT"         "A_N_PMN"        "A_P_OL"        
    ## [21] "A_K_AAA"        "A_B_HW"         "A_ZN_CC"        "A_ZN_RT"       
    ## [25] "A_MG_AAA"

## Calculate soil health index for a single field

Lets see how to assess the soil quality for a single field. First select
the first field given a crop rotation scheme for 4 years (the Dutch
fields). This is basically a subset of the farm and contains all soil
and field properties for the last 4 years. We subsequently use the
function
[`osi_field()`](https://gerardhros.github.io/euosi/reference/osi_field.md)
to calculate the different soil indicators and soil scores. Note that
the osi_farm has only the generic input parameters. Country specific
soil properties can also be used (see
[`?osi_field`](https://gerardhros.github.io/euosi/reference/osi_field.md)).
These specific parameters are by default set to NA.

We start with a request for the soil indicators.

``` r
# select the data for the first field of osi farm
dt <- dt.of[ID=='F1']

# calculate the OSI indicator values for that field
out <- euosi::osi_field(B_LU = dt$B_LU,
                        B_SOILTYPE_AGR = dt$B_SOILTYPE_AGR,
                        B_COUNTRY = dt$B_COUNTRY, 
                        B_BGZ = NA_character_,
                        B_PREC_SUM = dt$B_PREC_SUM,
                        B_PREC_WIN = dt$B_PREC_WIN, 
                        B_PET_SUM = dt$B_PET_SUM,
                        B_PET_WIN = dt$B_PET_WIN,
                        B_TEMP_SUM = dt$B_TEMP_SUM,
                        B_TEMP_WIN = dt$B_TEMP_WIN,
                        A_CLAY_MI = dt$A_CLAY_MI,
                        A_SAND_MI = dt$A_SAND_MI,
                        A_SOM_LOI = dt$A_SOM_LOI, 
                        A_C_OF= dt$A_C_OF,
                        A_CEC_CO = dt$A_CEC_CO,
                        A_PH_CC = dt$A_PH_CC, 
                        A_CACO3_IF = dt$A_CACO3_IF,
                        A_N_RT = dt$A_N_RT,
                        A_N_PMN = dt$A_N_PMN,
                        A_P_OL = dt$A_P_OL,
                        A_K_AAA = dt$A_K_AAA,
                        A_MG_AAA = dt$A_MG_AAA, 
                        A_B_HW = dt$A_B_HW, 
                        A_ZN_CC = dt$A_ZN_CC, 
                        A_ZN_EDTA = NA_real_,
                        A_ZN_RT = dt$A_ZN_RT,
                        ID = dt$ID, 
                        output = 'indicators')

# show the first line with all properties for a single field and latest year

  # format in a long table, and subset the numeric columns only
  dt2 <- copy(dt)[1]

  # select numeric values and round to two digits
  cols <- colnames(dt2)[grepl('A_|B_PREC|B_PET|B_TEMP',colnames(dt2))]
  dt2[,c(cols) := lapply(.SD,function(x) round(x,2)),.SDcols = cols]

  # melt the data.table
  dt.melt1 <- melt(dt2[,mget(c(cols,'ID'))], 
                  id.vars = c('ID'),
                  variable.name = 'parameter', value.name = 'value')

  # print the table
  # knitr::kable(dt.melt1,caption='numerical input variables for euosi')
  
  # print the table in nicer format
  knitr::kable(
     list(dt.melt1[1:10,2:3],dt.melt1[11:20,2:3]),
     caption = 'numerical input variables for euosi.',
     booktabs = TRUE
 )
```

[TABLE]

numerical input variables for euosi.

``` r
# do the same for the categorial values
  
  # format in a long table, and subset the numeric columns only
  dt3 <- copy(dt)[1]
  
  # select categorial variables
  cols2 <- colnames(dt3)[!colnames(dt3) %in% cols]
  
  # convert to chracter
  dt3 <- dt3[,lapply(.SD,as.character),.SDcols = cols2]
  
  # melt the data.table
  dt.melt2 <- melt(dt3[,mget(cols2)], 
                  id.vars = c('ID'),
                  variable.name = 'parameter', value.name = 'value')

  # print the table
  knitr::kable(dt.melt2,caption='categorial input variables for euosi')
```

| ID  | parameter      | value   |
|:----|:---------------|:--------|
| F1  | B_COUNTRY      | NL      |
| F1  | B_LU           | 172     |
| F1  | B_SOILTYPE_AGR | dekzand |

categorial input variables for euosi

To simplify, one can also use a data.table as input. In that case use
the function `soi_field_dt()`. The function checks whether all desired
input variables are present. Again, the illustration below shows the use
of `osi_field_dt` to retrieve the euosi indicators. For the
interpretation of each of the soil indicators, one can have a look at
the
[`vignette("eu_osi_variables")`](https://gerardhros.github.io/euosi/articles/eu_osi_variables.md)

``` r
# select the data for the first field of osi farm
dt <- dt.of[ID=='F1']

# calculate the osi indicator values
out <- osi_field_dt(dt, output = 'indicators')

# print the names of the output object
print(colnames(out))
```

    ##  [1] "ID"              "i_b_biodiv"      "i_b_pmn"         "i_c_b"          
    ##  [5] "i_c_k"           "i_c_mg"          "i_c_n"           "i_c_p"          
    ##  [9] "i_c_ph"          "i_c_zn"          "i_e_carbon"      "i_e_kexcess"    
    ## [13] "i_e_nleach"      "i_e_pexcess"     "i_e_watererosie" "i_p_cr"         
    ## [17] "i_p_dens"        "i_p_ksat"        "i_p_paw"         "i_p_wef"        
    ## [21] "i_p_whc"

To ask for the euosi soil health scores, summarizing the contribution of
the soil to various ecosystem services, one can ask for the scores as
output. Just replace the argument output with `scores` rather than
`indicators`. It is also possible to ask for both: just set the argument
output equal to `all`, being the default option.

Below we summarize the osi score for a single field. Given the
hierarchical structure of the euosi soil quality assesment, the
following scores are calculated:

- OSI total score (`s_euosi_total`) represents the overall integrated
  soil quality assessment, where the score varies from zero (serious
  bottlenecks being present) up to one (the quality is optimal for the
  crop rotation plan on this field)
- ESD crop production (`s_euosi_ess_prod`): the quality score reflecting
  the ability of the soil to sustain crop production
- ESD environment (`s_euosi_ess_env`): the quality score reflecting the
  ability of the soil to contribute to a healthy environment for air,
  water and biodiversity
- ESD climate mitigation (`s_euosi_clim`): the quality score reflecting
  the ability of the soil to store carbon in soil and mitigate climate
  change
- ESD nutrient cycling (`s_euosi_nutcycle`): the quality score
  reflecting the ability of the soil to recycle (regional) nutrients
  efficiently
- ESD water (`s_euosi_water`): the quality score reflecting the ability
  of the soil to retain and purify water leaching to ground and surface
  water

Besides these four ecoystem services provided by the soil, the ESS crop
production can be shown in more detail, thereby reflecting underlying
processes controlling the ESS. The ESS crop production can be separated
into the soil quality assessment in view of chemical (`s_euosi_prod_c`),
physical (`s_euosi_prod_p`) and biological (`s_euosi_prod_b`) soil
functions contributing to the ability of soils to sustain crop
production.

``` r
# calculate the OSI scoring values for ecosystem services
out <- osi_field_dt(dt, output = 'scores')
 
# reformat the output in a long table
out.melt <- melt(out,id.vars='ID',variable.name='soil_ESD',value.name='shi')

# set levels for the soil_ESD with a new label for the figure
plevel = c('s_euosi_ess_env','s_euosi_ess_prod',
           's_euosi_prod_c','s_euosi_prod_p','s_euosi_prod_b',
           's_euosi_clim','s_euosi_water','s_euosi_nutcycle',
           's_euosi_total')
tlabel = c('ESS environment', 'ESS crop production',
           'crop production chemistry','crop production physics','crop production biology',
           'ESS climate mitigation','ESS water','ESS nutrient cycling',
           'EUOSI total score')
out.melt[,tsoil_ESD := factor(soil_ESD,levels = plevel,labels = tlabel)]

# print the output object
knitr::kable(out.melt[,.(ID,ESS = soil_ESD,description=tsoil_ESD,score = shi)],
             caption='OSI scores for field 1')
```

| ID  | ESS              | description               | score |
|:----|:-----------------|:--------------------------|------:|
| F1  | s_euosi_ess_env  | ESS environment           |  0.92 |
| F1  | s_euosi_ess_prod | ESS crop production       |  0.71 |
| F1  | s_euosi_prod_b   | crop production biology   |  0.99 |
| F1  | s_euosi_prod_c   | crop production chemistry |  0.58 |
| F1  | s_euosi_clim     | ESS climate mitigation    |  0.88 |
| F1  | s_euosi_nutcycle | ESS nutrient cycling      |  0.94 |
| F1  | s_euosi_prod_p   | crop production physics   |  0.69 |
| F1  | s_euosi_water    | ESS water                 |  0.92 |
| F1  | s_euosi_total    | EUOSI total score         |  0.79 |

OSI scores for field 1

``` r
# to show the output in a lollipop figure
plabel = c('ESS environment','ESS crop\nproduction',
           'crop production\nchemistry','crop production\nphysics','crop production\nbiology',
           'ESS climate\nmitigation','ESS water','ESS nutrient\ncycling',
           'OSI total score')
out.melt[,psoil_ESD := factor(soil_ESD,levels = plevel,labels = plabel)]
```

![Soil health index Example for
Europe.](eu_osi_introduction_files/figure-html/example-1-1.png)

Soil health index Example for Europe.

## Reading more?

More vignettes will be made available regarding the derivation of all
soil health indicators, the aggregation methods applied, the derivation
of optimum land use given the current soil quality assessments, and the
best management practices to be applied to improve the soil quality. Are
you interested to contribute, please contact the author(s) of this R
package. Enjoy!
