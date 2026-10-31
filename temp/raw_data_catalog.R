# Canonical paths for the reorganized release. Run scripts from project root.
# Existing models intentionally retain the legacy constructed R1--R5 file.
# The latest R1--R7 release changes historical values and is NOT a substitute.
raw_data_files <- c(
  constructed_legacy = "rawdata/legacy_constructed_r1_r5/ethiopia_constructed.dta",
  r3_oc = "rawdata/yl_r3/UKDA-6853-stata/stata/stata13/r3_oc/ethiopia/et_oc_childlevel.dta",
  r3_yc = "rawdata/yl_r3/UKDA-6853-stata/stata/stata13/r3_yc/ethiopia/et_yc_childlevel.dta",
  r4_oc_cognitive = "rawdata/yl_r4/UKDA-7931-stata/stata/stata13/r4_oc/ethiopia/oc_chcog/et_r4_occog_olderchild.dta",
  r4_yc_cognitive = "rawdata/yl_r4/UKDA-7931-stata/stata/stata13/r4_yc/ethiopia/yc_chcog/et_r4_yccog_youngerchild.dta",
  r5_oc_activity = "rawdata/yl_r5/UKDA-8357-stata/stata/stata13/ethiopia_r5/etoc_ch_anon/et_r5_occh_activity.dta",
  school_pupils_w1 = "rawdata/yl_school_survey_eth_2012-2013/UKDA-7823-stata/stata/stata11/wave_1/et_pupilleveldata_r2.dta",
  school_schools_w1 = "rawdata/yl_school_survey_eth_2012-2013/UKDA-7823-stata/stata/stata11/wave_1/et_schoolleveldata_r2.dta",
  school_literacy_w2 = "rawdata/yl_school_survey_eth_2012-2013/UKDA-7823-stata/stata/stata11/wave_2/etpupilliteracytest.dta",
  school_math_w2 = "rawdata/yl_school_survey_eth_2012-2013/UKDA-7823-stata/stata/stata11/wave_2/etpupilmathstest.dta",
  constructed_r1_r7 = "rawdata/yl_const_r1_r7/UKDA-9543-stata/stata/stata13/ethiopia_constructed_r1r7.dta",
  round7_ethiopia = "rawdata/yl_r7/UKDA-9538-stata/stata/stata13/ethiopia_r7"
)
