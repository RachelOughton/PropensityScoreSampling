# With replacement, not dropping

    Code
      matched_comparison(df = eg_data, propscore_df = eg_mix_exp100, cov_cols = c(
        "category", "risk_ass", "sus_age_bin"), arm_col = "Arm", intervention_level = "Intervention",
      seed = 20, replace = T, downsample = F, drop_int = F)
    Message
      There are 1 NA values in the category column. These rows will be lost.
      There are 1 NA values in the sus_age_bin column. These rows will be lost.
    Output
      # A tibble: 997 x 32
         victim_sex victim_age suspect_sex suspect_age reference_date      FM_equip
         <chr>           <dbl> <chr>             <dbl> <dttm>              <lgl>   
       1 F                  28 M                    35 2024-12-06 16:13:16 TRUE    
       2 F                  28 M                    35 2024-12-06 16:13:16 TRUE    
       3 F                  28 M                    35 2024-12-06 16:13:16 TRUE    
       4 F                  35 M                    35 2024-12-02 18:50:41 TRUE    
       5 F                  34 M                    37 2024-06-24 05:03:50 TRUE    
       6 F                  51 M                    54 2024-07-21 15:34:52 TRUE    
       7 F                  60 M                    41 2024-08-03 00:52:16 TRUE    
       8 F                  39 M                    23 2024-07-04 10:43:38 TRUE    
       9 F                  31 M                    28 2024-07-29 21:11:28 TRUE    
      10 F                  34 M                    36 2024-08-03 10:19:20 TRUE    
      # i 987 more rows
      # i 26 more variables: warning_YN <chr>, risk_ass <fct>,
      #   protection_order <lgl>, princ_crime <chr>, category_new <fct>, phase <dbl>,
      #   nprev_sus <dbl>, n_fo_sus <int>, n_fo_dyad <lgl>, susprev_stalking <lgl>,
      #   susprev_vs_high <lgl>, Arm <chr>, category <fct>, sus_age_bin <fct>,
      #   arm_given <tibble[,1]>, include <dbl>, p_int_risk_ass.category <dbl>,
      #   p_comp_risk_ass.category <dbl>, p_int_sus_age_bin <dbl>, ...

---

    Code
      matched_comparison(df = eg_data, propscore_df = eg_m_resc15, cov_cols = c(
        "category", "risk_ass", "sus_age_bin"), arm_col = "Arm", intervention_level = "Intervention",
      seed = 20, replace = T, downsample = F, drop_int = F)
    Message
      There are 1 NA values in the category column. These rows will be lost.
      There are 1 NA values in the sus_age_bin column. These rows will be lost.
      31 comparison cases are being repeated 2 times
    Output
      # A tibble: 997 x 34
         victim_sex victim_age suspect_sex suspect_age reference_date      FM_equip
         <chr>           <dbl> <chr>             <dbl> <dttm>              <lgl>   
       1 F                  28 M                    35 2024-12-06 16:13:16 TRUE    
       2 F                  28 M                    35 2024-12-06 16:13:16 TRUE    
       3 F                  28 M                    35 2024-12-06 16:13:16 TRUE    
       4 F                  35 M                    35 2024-12-02 18:50:41 TRUE    
       5 F                  34 M                    37 2024-06-24 05:03:50 TRUE    
       6 F                  51 M                    54 2024-07-21 15:34:52 TRUE    
       7 F                  60 M                    41 2024-08-03 00:52:16 TRUE    
       8 F                  39 M                    23 2024-07-04 10:43:38 TRUE    
       9 F                  31 M                    28 2024-07-29 21:11:28 TRUE    
      10 F                  34 M                    36 2024-08-03 10:19:20 TRUE    
      # i 987 more rows
      # i 28 more variables: warning_YN <chr>, risk_ass <fct>,
      #   protection_order <lgl>, princ_crime <chr>, category_new <fct>, phase <dbl>,
      #   nprev_sus <dbl>, n_fo_sus <int>, n_fo_dyad <lgl>, susprev_stalking <lgl>,
      #   susprev_vs_high <lgl>, Arm <chr>, category <fct>, sus_age_bin <fct>,
      #   arm_given <tibble[,1]>, include <dbl>, p_int_risk_ass <dbl>,
      #   p_comp_risk_ass <dbl>, p_int_category <dbl>, p_comp_category <dbl>, ...

# downsampling, dropping

    Code
      matched_comparison(df = eg_data, propscore_df = eg_m_resc15, cov_cols = c(
        "category", "risk_ass", "sus_age_bin"), arm_col = "Arm", intervention_level = "Intervention",
      seed = 20, replace = F, downsample = T, drop_int = T)
    Message
      There are 1 NA values in the category column. These rows will be lost.
      There are 1 NA values in the sus_age_bin column. These rows will be lost.
      27 intervention cases have beeen downsampled
    Output
      # A tibble: 997 x 34
         victim_sex victim_age suspect_sex suspect_age reference_date      FM_equip
         <chr>           <dbl> <chr>             <dbl> <dttm>              <lgl>   
       1 F                  28 M                    35 2024-12-06 16:13:16 TRUE    
       2 F                  28 M                    35 2024-12-06 16:13:16 TRUE    
       3 F                  28 M                    35 2024-12-06 16:13:16 TRUE    
       4 F                  35 M                    35 2024-12-02 18:50:41 TRUE    
       5 F                  34 M                    37 2024-06-24 05:03:50 TRUE    
       6 F                  51 M                    54 2024-07-21 15:34:52 TRUE    
       7 F                  60 M                    41 2024-08-03 00:52:16 TRUE    
       8 F                  39 M                    23 2024-07-04 10:43:38 TRUE    
       9 F                  31 M                    28 2024-07-29 21:11:28 TRUE    
      10 F                  34 M                    36 2024-08-03 10:19:20 TRUE    
      # i 987 more rows
      # i 28 more variables: warning_YN <chr>, risk_ass <fct>,
      #   protection_order <lgl>, princ_crime <chr>, category_new <fct>, phase <dbl>,
      #   nprev_sus <dbl>, n_fo_sus <int>, n_fo_dyad <lgl>, susprev_stalking <lgl>,
      #   susprev_vs_high <lgl>, Arm <chr>, category <fct>, sus_age_bin <fct>,
      #   arm_given <tibble[,1]>, include <dbl>, p_int_risk_ass <dbl>,
      #   p_comp_risk_ass <dbl>, p_int_category <dbl>, p_comp_category <dbl>, ...

