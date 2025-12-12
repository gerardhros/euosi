# EU OSI variables

## euosi input variables

The main function of the package
[`osi_field()`](https://gerardhros.github.io/euosi/reference/osi_field.md)
requires as input a series of soil properties required to assess the
soil quality. The required input columns are specified in
[`euosi::osi_vars_input`](https://gerardhros.github.io/euosi/reference/osi_vars_input.md)
and can also be seen in the table below.

Note that not all parameters are required. Optional parameters incude:

- all country specific soil measurement methods: the `osi_field`
  function has been made given the main soil properties being available
  across Europe from the lUCAS monitoring network. Note that the
  optional input parameters can be used for the individual soil health
  function already, but not yet for the `osi_field` function.
- climatic conditions (precipitation, evaporation and temperature) are
  set to defaults per country, but can be overwritten with user defined
  data

All numeric input variables are described below:

| code           | parameter                                                                           | unit        |    min |     max |
|:---------------|:------------------------------------------------------------------------------------|:------------|-------:|--------:|
| A_AL_CO        | aluminium extractable (bacl2)                                                       | mmol+/kg    |  0e+00 |   100.0 |
| A_AL_CO_PO     | aluminium occupation of cec                                                         | %           |  1e-01 |    25.0 |
| A_AL_OX        | aluminium extractable (oxalate)                                                     | mmol Al/kg  |  1e-01 |  1000.0 |
| A_AL_RT        | aluminium total content                                                             | g Al / kg   |  1e-03 |   210.0 |
| A_B_CC         | boron plant available (cacl2)                                                       | 0B5g B/ kg  |  1e-03 |  1000.0 |
| A_B_HW         | boron extractable (hot water)                                                       | mg/kg       |  0e+00 |  1000.0 |
| A_CACO3_IF     | carbonated lime                                                                     | %           |  0e+00 |   100.0 |
| A_CA_CO        | calcium extractable (bacl2)                                                         | mmol+/kg    |  1e-01 |   500.0 |
| A_CA_CO_PO     | calcium occupation of cec                                                           | %           |  1e+00 |   100.0 |
| A_CA_RT        | calcium total content                                                               | g Ca / kg   |  1e-03 |   350.0 |
| A_CC_BCS       | crop cover on the surface                                                           | unitless    |  0e+00 |     2.0 |
| A_CEC_CO       | cation exchange capacity                                                            | mmol+/kg    |  1e+00 |  2000.0 |
| A_CEC_CO_PO    | base saturation of cec                                                              | %           |  1e+00 |   100.0 |
| A_CLAY_MI      | clay content                                                                        | %           |  0e+00 |   100.0 |
| A_CL_WA        | chlorine extractable (water)                                                        | mg Cl/kg    |  1e-02 |   100.0 |
| A_CN_FR        | carbon to nitrogen ratio                                                            | ratio       |  5e+00 |    40.0 |
| A_COM_FR       | carbon to soil organic matter ratio                                                 | fraction    |  3e-01 |     0.8 |
| A_CO_AA        | cobalt extractable (aa)                                                             | 0B5g Co/ kg |  1e-01 |  1000.0 |
| A_CO_CC        | cobalt plant available (cacl2)                                                      | 0B5g Co/ kg |  1e-02 |  1000.0 |
| A_CS_FR        | carbon to sulphur ratio                                                             | ratio       |     NA |      NA |
| A_CU_CC        | copper plant available (cacl2)                                                      | 0B5g Cu/ kg |  1e-01 |  1000.0 |
| A_CU_HNO3      | copper extractable (hno3)                                                           | define_unit |     NA |      NA |
| A_CU_RT        | copper total content                                                                | mg Cu / kg  |  1e-03 |   450.0 |
| A_CU_EDTA      | copper in EDTA                                                                      | mg Cu / kg  |  0e+00 |   500.0 |
| A_C_BCS        | presence of visible cracks in the top layer                                         | unitless    |  0e+00 |     2.0 |
| A_C_FB         | funghal biomass                                                                     | mg C/kg     |  1e+01 |  1000.0 |
| A_C_IF         | inorganic carbon fraction                                                           | %           |  1e-01 |     0.5 |
| A_C_MB         | microbial biomass                                                                   | mg C/kg     |  1e+01 |  1000.0 |
| A_C_OF         | organic carbon content                                                              | g C/ kg     |  1e-01 |  1000.0 |
| A_C_RT         | total c content                                                                     | g C/ kg     |  0e+00 |   500.0 |
| A_DENSITY_SA   | dry bulk density                                                                    | g/cm3       |  5e-01 |     3.0 |
| A_DEPTH        | depth of the soil layer                                                             | m           |  0e+00 |     2.0 |
| A_EC_WA        | electroconductivity                                                                 | mS / cm     |  1e-02 |    15.0 |
| A_EW_BCS       | presence of earth worms                                                             | unitless    |  0e+00 |     2.0 |
| A_FB_FR        | funghal to bacterial mass ratio                                                     | ratio       |  1e-02 |    10.0 |
| A_FE_CC        | iron plant available (cacl2)                                                        | 0B5g Fe/ kg |  1e+03 | 50000.0 |
| A_FE_OX        | iron extractable (oxalate)                                                          | mmol Fe/kg  |  1e-01 |  1000.0 |
| A_FE_RT        | iron total content                                                                  | g Fe / kg   |  1e-03 |   150.0 |
| A_GS_BCS       | presence of watterlogged conditions and gley spots                                  | unitless    |  0e+00 |     2.0 |
| A_H_CO_PO      | hydrogen occupation of cec                                                          | %           |  1e-01 |    25.0 |
| A_K_AAA        | potassium extractable (acid ammonium acetate)                                       | mg K/kg     |  1e-01 | 10000.0 |
| A_K_AL         | potassium extractable (ammonium lactate)                                            | mg K/kg     |  1e-01 | 10000.0 |
| A_K_AN         | potassium extractable (ammonium nitrate)                                            | mg K/kg     |  1e-01 | 10000.0 |
| A_K_CAL        | potassium extractable (calcium ammonium lactate)                                    | mg K/kg     |  1e-01 | 10000.0 |
| A_K_CC         | potassium extractable (cacl2)                                                       | mg K /kg    |  1e+00 |  1000.0 |
| A_K_CO         | potassium extractable (bacl2)                                                       | mmol+/kg    |  1e-01 |   150.0 |
| A_K_CO_PO      | potassium occupation of cec                                                         | %           |  1e-01 |    50.0 |
| A_K_DL         | potassium extractable (double lactate)                                              | mg K/kg     |  1e-01 | 10000.0 |
| A_K_KG         | potassium number                                                                    | unitless    |  1e+00 |   100.0 |
| A_K_M3         | potassium extractable (mehlich 3)                                                   | mg K/kg     |  1e-01 | 10000.0 |
| A_K_NaAAA      | potassium extractable (sodium acetate acetic acid; Morgans solution)                | mg K/kg     |  1e-01 | 10000.0 |
| A_K_RT         | potassium total content                                                             | g K /kg     |  1e-03 |    80.0 |
| A_K_WA         | potassium extractable (water)                                                       | mg K /kg    |  1e+00 |   600.0 |
| A_MG_AAA       | magnesium extractable (acid ammonium acetate)                                       | mg Mg/kg    |  0e+00 |  1100.0 |
| A_MG_AL        | magnesium extractable (ammonium lactate)                                            | mg Mg/kg    |  0e+00 |  1100.0 |
| A_MG_AN        | magnesium extractable (ammonium nitrate)                                            | mg Mg/kg    |  0e+00 |  1100.0 |
| A_MG_CC        | magnesium extractable (cacl2)                                                       | mg Mg / kg  |  1e+00 |  1100.0 |
| A_MG_CO        | magnesium extractable (bacl2)                                                       | mmol+ / kg  |  1e-01 |   150.0 |
| A_MG_CO_PO     | magnesium occupation of cec                                                         | %           |  1e-01 |    50.0 |
| A_MG_DL        | magnesium extractable (double lactate)                                              | mg Mg/kg    |  0e+00 |  1100.0 |
| A_MG_KCL       | magnesium extractable (potassium chloride)                                          | mg Mg/kg    |  0e+00 |  1100.0 |
| A_MG_M3        | magnesium extractable (mehlich-III)                                                 | mg Mg/kg    |  0e+00 |  1100.0 |
| A_MG_NaAAA     | magnesium extractable (sodium acetate acetic acid; Morgans solution)                | mg Mg/kg    |  0e+00 |  1100.0 |
| A_MG_RT        | magnesium total content                                                             | g Mg / kg   |  1e-01 |   350.0 |
| A_MN_CC        | manganese plant available (cacl2)                                                   | 0B5g Mn/ kg |  1e-01 | 60000.0 |
| A_MN_RT        | manganese total content                                                             | mg Mn / kg  |  1e+01 | 30000.0 |
| A_MO_CC        | molybdenum plant available (cacl2)                                                  | 0B5g Mo/ kg |  1e-01 |   100.0 |
| A_NA_CC        | sodium plant available (cacl2)                                                      | mg Na/kg    |  1e+00 |  2500.0 |
| A_NA_CO        | sodium extractable (bacl2)                                                          | mmol+/kg    |  1e-01 |    50.0 |
| A_NA_CO_PO     | sodium occupation of cec                                                            | %           |  1e+00 |   100.0 |
| A_NA_HCL       | sodium extractable (hcl)                                                            | mg Na / kg  |  1e-01 |    50.0 |
| A_NA_RT        | sodium total content                                                                | mg Na / kg  |  1e-01 |    50.0 |
| A_NI_RT        | nickel total content                                                                | mg Ni / kg  |  1e-03 |  2500.0 |
| A_NMIN_CC_D30  | inorganic nitrogen topsoil (0 to 30cm depth)                                        | kg N/ha     |  0e+00 |   500.0 |
| A_NMIN_CC_D60  | inorganic nitrogen topsoil (0 to 60cm depth)                                        | kg N/ha     |  0e+00 |   500.0 |
| A_N_PMN        | potentially mineralizable nitrogen                                                  | mg N/ kg    |  1e+00 |   500.0 |
| A_N_RT         | nitrogen total content                                                              | mg N/ kg    |  1e+00 | 35000.0 |
| A_PH_CC        | acidity in cacl2                                                                    | \-          |  3e+00 |    10.0 |
| A_PH_KCL       | acidity in kcl                                                                      | \-          |  3e+00 |    10.0 |
| A_PH_WA        | acidity in water                                                                    | \-          |  3e+00 |    10.0 |
| A_P_AL         | phosphor extractable (ammonium lactate)                                             | mg P / kg   |  1e+00 |   500.0 |
| A_P_BCS        | presence of water puddles on the land or ponding                                    | unitless    |  0e+00 |     2.0 |
| A_P_CAL        | phosphor extractable (calcium ammonium lactate)                                     | mg P/kg     |  1e+00 |   500.0 |
| A_P_CC         | phosphor ectratable (cacl2)                                                         | mg P/kg     |  1e-01 |   100.0 |
| A_P_DL         | phosphor extractable (double lactate)                                               | mg P/kg     |  1e+00 |   500.0 |
| A_P_M3         | phosphor extractable (mehlich 3)                                                    | mg P/kg     |  0e+00 |  1000.0 |
| A_P_OX         | phosphor extractable (oxalate)                                                      | mmol P/kg   |  1e-01 |   100.0 |
| A_P_RT         | phosphor total content                                                              | g P / kg    |  1e-02 |    10.0 |
| A_P_SG         | phosphor saturation degree                                                          | %           |  1e-01 |   100.0 |
| A_P_WA         | phosphate extractable (water)                                                       | mg P / kg   |  1e+00 |   500.0 |
| A_P_OL         | phosphor extractable (method Olsen)                                                 | mg P / kg   |  0e+00 |   500.0 |
| A_P_AAA        | phosphor extractable (acid ammonium acetate)                                        | mg P / kg   |  0e+00 |  1000.0 |
| A_P_AAA_EDTA   | phosphor extractable (acid ammonium acetate plus EDTA)                              | mg P / kg   |  0e+00 |  1000.0 |
| A_RD           | root depth                                                                          | m           |  0e+00 |     2.0 |
| A_RD_BCS       | root depth                                                                          | m           |  0e+00 |     2.0 |
| A_RT_BCS       | presence of visible tracks or rutting or trampling on the land                      | unitless    |  0e+00 |     2.0 |
| A_SAND_M50     | median particle size of sand fraction                                               | um          |     NA |      NA |
| A_SAND_MI      | sand content                                                                        | %           |  0e+00 |   100.0 |
| A_SC_BCS       | presence of subsoil compaction                                                      | unitless    |  0e+00 |     2.0 |
| A_SE_CC        | selenium plant available (cacl2)                                                    | 0B5g Se/ kg |  1e-01 |    50.0 |
| A_SE_CO        | selenium extractable (bacl2)                                                        | define_unit |     NA |      NA |
| A_SILT_MI      | silt content                                                                        | %           |  0e+00 |   100.0 |
| A_SILT_MI_16MU | slib content                                                                        | %           |  0e+00 |   100.0 |
| A_SI_CC        | silicon plant available (cacl2)                                                     | 0B5g Si/ kg |  1e+03 | 20000.0 |
| A_SI_RT        | silicon total content                                                               | g Si / kg   |  1e+00 |   500.0 |
| A_SOM_LOI      | soil organic matter content                                                         | %           |  5e-01 |    75.0 |
| A_SS_BCS       | soil structure assessment score                                                     | unitless    |  0e+00 |     2.0 |
| A_S_CC         | sulphur plant available (cacl2)                                                     | mg S/kg     |  1e-01 |   100.0 |
| A_S_RT         | sulphur total content                                                               | mg S/kg     |  1e+00 | 10000.0 |
| A_ZN_AAA       | zinc extraction (acid amonium acetate)                                              | mg Zn / kg  |  0e+00 |   100.0 |
| A_ZN_CC        | zinc extractable (cacl2)                                                            | 0B5g Zn/ kg |  5e+00 | 50000.0 |
| A_ZN_CO        | zinc extractable (cohex)                                                            |             |  0e+00 |   100.0 |
| A_ZN_DTPA      | zinc extractable (DTPA)                                                             | mg Zn / kg  |  0e+00 |   100.0 |
| A_ZN_EDTA      | zinc extractable (EDTA)                                                             | mg Zn / kg  |  0e+00 |   100.0 |
| A_ZN_M3        | zinc extractable (mehlich-3)                                                        |             |  0e+00 |   100.0 |
| A_ZN_WA        | zinc extractable (water)                                                            |             |  0e+00 |   100.0 |
| A_ZN_RT        | zinc total content                                                                  | mg Zn / kg  |  1e-03 | 90000.0 |
| B_ALT_AHN_3    | elevation from ahn3                                                                 | m           | -8e+00 |   320.0 |
| B_AREA         | area of the field                                                                   | m^2         |  0e+00 |      NA |
| B_DER          | derogation permit                                                                   | \-          |     NA |      NA |
| B_EROSION      | soil erosion                                                                        | \-          |  0e+00 |      NA |
| B_FERT_NORM_FR | the fraction of the fertilizer norm applied on the field                            | \-          |  0e+00 |     1.0 |
| B_GWL_GHG      | averaged highest groundwater level                                                  | \-          |  0e+00 |      NA |
| B_GWL_GLG      | averaged lowest groundwater level                                                   | \-          |  0e+00 |      NA |
| B_GWL_ZCRIT    | distance between groundwater table and root zone (30 cm-mv) for delivering 2 mm/day | \-          |     NA |      NA |
| B_SC_WENR      | compaction risk                                                                     | \-          |     NA |      NA |
| B_SLOPE_DEGREE | slope of the field                                                                  | \-          |  0e+00 |    30.0 |
| B_CF           | coarse fragments                                                                    | %           |  0e+00 |   100.0 |
| B_PREC_SUM     | the total summer precipitation; ERA5                                                | mm          |  0e+00 |  2000.0 |
| B_PREC_WIN     | the total winter precipitation; ERA5                                                | mm          |  0e+00 |  2000.0 |
| B_PET_SUM      | the total summer evaporation; ERA5                                                  | mm          |  0e+00 |  6000.0 |
| B_PET_WIN      | the total winter evaporation; ERA5                                                  | mm          |  0e+00 |  6000.0 |
| B_TEMP_SUM     | the mean summer temperature; ERA5 (degrees C)                                       | C           | -1e+01 |    40.0 |
| B_TEMP_WIN     | the mean winter temperature; ERA5 (degrees C)                                       | C           | -1e+01 |    40.0 |
| B_SMC_SUM      | the mean soil moisture content in summer                                            | \-          |  1e-01 |     0.7 |
| B_SMC_WIN      | the mean soil moisture content in winter                                            | \-          |  1e-01 |     0.7 |
| B_RUSL_LS      | topography ls-factor                                                                | \-          |  0e+00 |     1.5 |
| B_RUSL_RE      | rainfall erositivity r-factor                                                       | \-          |  1e+02 |  6000.0 |
| B_RUSL_SE      | soil erodibility k-factor                                                           | \-          |  1e+00 |     4.0 |

All categorial input variables are described below:

| code                   | parameter                                                   | options                                                                                                                                                                                                                                                                                                                                                                     |
|:-----------------------|:------------------------------------------------------------|:----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| B_AER_FR               | region in France with different recommendations for P and K |                                                                                                                                                                                                                                                                                                                                                                             |
| B_AER_CBS              | agricultural economic region in NL cbs2016                  | LG14, LG13, LG12, LG11, LG10, LG09, LG08, LG07, LG06, LG05, LG04, LG03, LG02, LG01                                                                                                                                                                                                                                                                                          |
| B_AER_CBS_NAME         | name of agricultural economic region in NL cbs2016          |                                                                                                                                                                                                                                                                                                                                                                             |
| B_COUNTRY              | country code                                                | AX, AL, AD, AT, BY, BE, BA, BG, HR, CY, CZ, DK, EE, ET, FO, FI, FR, DE, GI, GR, GG, VA, HU, IS, IE, IM, IT, JE, LV, LI, LT, LU, MK, MT, MD, MC, ME, NL, NO, PL, PT, RO, SM, RS, SK, SI, ES, SJ, SE, CH, UA, GB                                                                                                                                                              |
| B_GWL_CLASS            | groundwater class                                           | II , IV , IIIb, V , VI , VII , Vb , - , Va , III , VIII, sVI , I , IIb , sVII, IVu , bVII, sV , sVb , bVI , IIIa                                                                                                                                                                                                                                                            |
| B_LU                   | cultivation code                                            | varies per country                                                                                                                                                                                                                                                                                                                                                          |
| B_SC_WENR_NAME         | compaction risk name in dutch                               |                                                                                                                                                                                                                                                                                                                                                                             |
| B_SOILTYPE_AGR         | agricultural soiltype                                       | moerige_klei, rivierklei , dekzand , zeeklei , dalgrond , veen , loess , duinzand , maasklei                                                                                                                                                                                                                                                                                |
| B_SOILTYPE_NLEACH_OBIC | soiltype classification according to nitrogen leaching risk | zand, klei, veen                                                                                                                                                                                                                                                                                                                                                            |
| B_TEXTURE_USDA         | soil texture class according to the usda classification     | sand , loamy sand , sandy loam , loam , silt loam , silt , sandy clay loam, clay loam , silty clay loam, sandy clay , silty clay , clay , Cl , SiCL , SaCL , SiClLo , LoSa , SaLo , SiLo , Sa , Lo , ClLo , SaCl , Si                                                                                                                                                       |
| B_TEXTURE_BE           | soil texture class according to Belgium classification      | U , zware klei , A , leem , Z , zand , S , lemig zand , P , licht zandleem, L , zandleem , E , klei                                                                                                                                                                                                                                                                         |
| B_TEXTURE_GEPPA        | soil texture class according to GEPPA classification        | SS , sable , LL , limon pur , Ls , limon sableux , Sl , sable limoneux , S , sableux , L , limon , Lsa , limon sablo-argileux , Sal , sable argilo-limoneux , Sa , sable argileux , La , limon argileux , LAS , limon argilo-sableux , AS , argilo-sableux , Al , argile limoneuse , Als , argile limono-sableuse, As , argile sableuse , A , argileux , AA , argile lourde |
| B_TEXTURE_HYPRES       | texture hypres european soil map                            | C , course , MF , medium fine, M , medium , F , fine , VF , very fine                                                                                                                                                                                                                                                                                                       |
| B_BGZ                  | biogeographical zone                                        | 1 , alpine , 2 , anatolian , 3 , arctic , 4 , atlantic , 5 , blackSea , 6 , boreal , 7 , continental , 8 , macaronesia , 9 , mediterranean , 10 , outside , 11 , pannonian , 12 , steppic                                                                                                                                                                                   |

## euosi output variables

When the function `osi_field` or `osi_field_dt` has been used to assess
the quality of the soil, then by default all soil indicators and BLN
soil quality scores are calculated. A soil indicator is a number varying
between zero (value 0) and one (value 1) expressing the distance to
target. The target is met when the distance equals to 1. There are
serious bottlenecks when the indicator value approaches zero. Similarly,
a high score represents a high soil quality. Soil quality scores are
basically a weighted mean of a subset of soil indicators, where the
weighing procedure accounts for the distance to target.

All output variables are listed in the table below:

| code             | parameter                                                                                                                             |
|:-----------------|:--------------------------------------------------------------------------------------------------------------------------------------|
| i_b_biodiv       | The soil indicator value reflecting distance to target for soil biodiversity in view of crop production                               |
| i_b_pmn          | The soil indicator value reflecting distance to target for soil microbial activity in view of crop production                         |
| i_c_b            | The soil indicator value reflecting distance to target for boron supply in view of crop production                                    |
| i_c_k            | The soil indicator value reflecting distance to target for potassium supply in view of crop production                                |
| i_c_mg           | The soil indicator value reflecting distance to target for magnesium supply in view of crop production                                |
| i_c_n            | The soil indicator value reflecting distance to target for nitrogen supply in view of crop production                                 |
| i_c_p            | The soil indicator value reflecting distance to target for phosphorus supply in view of crop production                               |
| i_c_ph           | The soil indicator value reflecting distance to target for soil pH in view of crop production                                         |
| i_c_zn           | The soil indicator value reflecting distance to target for zinc supply in view of crop production                                     |
| i_e_carbon       | The soil indicator value reflecting distance to target for carbon saturation in mineral soils in view carbon sequestration potential  |
| i_e_kexcess      | The soil indicator value reflecting distance to target for potassium use efficiency in view of nutrient recycling and reuse           |
| i_e_nleach       | The soil indicator value reflecting distance to target for nitrogen leaching risks in view of groundwater regulation and purification |
| i_e_pexcess      | The soil indicator value reflecting distance to target for phosphorus use efficiency in view of nutrient recycling and reuse          |
| i_e_watererosie  | The soil indicator value reflecting distance to target for water erodibility                                                          |
| i_p_cr           | The soil indicator value reflecting distance to target for crumbleability in view of crop production                                  |
| i_p_dens         | The soil indicator value reflecting distance to target for subsoil compaction in view of crop production                              |
| i_p_ksat         | The soil indicator value reflecting distance to target for water permeability in view of crop production                              |
| i_p_paw          | The soil indicator value reflecting distance to target for water stress in view of crop production                                    |
| i_p_wef          | The soil indicator value reflecting distance to target for wind erodibility in view of crop production                                |
| i_p_whc          | The soil indicator value reflecting distance to target for water retention in view of crop production                                 |
| s_euosi_ess_env  | The aggregated soil quality score for the ecosystem service Environment                                                               |
| s_euosi_ess_prod | The aggregated soil quality score for the ecosystem service Crop Production                                                           |
| s_euosi_prod_b   | The aggregated soil indicator value for biological soil functions supporting crop production                                          |
| s_euosi_prod_c   | The aggregated soil indicator value for chemical soil functions supporting crop production                                            |
| s_euosi_prod_p   | The aggregated soil indicator value for physical soil functions supporting crop production                                            |
| s_euosi_clim     | The aggregated soil indicator value for soil functions supporting carbon sequestration and climate regulation                         |
| s_euosi_nutcycle | The aggregated soil indicator value for soil functions supporting nutrient recycling and reuse                                        |
| s_euosi_water    | The aggregated soil indicator value for soil functions supporting water regulation and purification                                   |
| s_euosi_total    | The total EU OSI soil quality score                                                                                                   |

## Adding new variables

When one aims to add new input or output variables, the following steps
need to be taken:

- new input variables can be defined in the `dev/osi_parameters.csv`.
  Please follow the same definition style. When the file is updated, run
  the relevant section in `dev/osi_tables.R` for the tables `osi_parms`,
  `osi_vars_input` and `osi_vars_output`.
- ensure that the R code is updated for the country specific function as
  well the wrapper functions `osi_field` and `osi_field_dt`.

Regarding paramater naming:

- soil properties measured in the lab follow a fixed structure. It
  always start with “A”, followed by the chemical element like “P” for
  phosphorus, followed by the extraction method abbreviation like “AL”
  for ammonium lactate. The fourth position is free to use when the
  element has specific aspects not covered by the first three positions.
  So, for the given example, this leads to “A_P_AL”. For a full list of
  the current inputs, see `osi_vars_input`.
- field or site properties follow the same structure. It starts with “B”
  followed by the parameter determined (e.g. “TEXTURE”), followed by the
  methods (e.g. “HYPRES”).
