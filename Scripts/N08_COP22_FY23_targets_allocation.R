
# FUNCTION ----

  # Datim Login

  # Read PSNUxIM Data

  # Clean DP Indicatoros

  # Map R to T Indicators

  # Summarize Results to Match Targets

  # IM Indicators by Age/Sex Proportion

# Datim Authentication ----

  datim_session()

# Load Data ----

  # MSD - PSNU x IM ----

  df_psnu <- file_psnu_im %>% read_msd()

  ## Save look up tables
  wb_lookups <- createWorkbook()

  ## MSD Indicators ----
  df_msd_indicators_fy21 <- df_psnu %>% msd_indicators(fy = prev_fy)

  addWorksheet(wb_lookups, sheetName = "FY21 MSD Indicators")

  writeDataTable(wb_lookups,
                 sheet = "FY21 MSD Indicators",
                 x = df_msd_indicators_fy21)

  df_msd_indicators_fy22 <- df_psnu %>% msd_indicators(fy = curr_fy)

  addWorksheet(wb_lookups, sheetName = "FY22 MSD Indicators")

  writeDataTable(wb_lookups,
                 sheet = "FY22 MSD Indicators",
                 x = df_msd_indicators_fy22)

  ## MSD Mechanisms ----

  ## Valid mech psnu coverage
  df_mech_flags <- file_cop22_mechs %>%
    read_excel(sheet = 1)

  psnu_usaid <- df_mech_flags  %>%
    filter(fundingagency == "USAID") %>%
    distinct(psnu)

  df_mech_flags <- df_mech_flags %>%
    filter(valid == 1) %>%
    select(psnu, mech_code,
           mech_code_new = mech_code_fy23)

  # List of Mechanisms
  df_msd_mechanisms <- df_psnu %>%
    msd_mechanisms(fy = curr_fy) %>%
    mutate(closing = case_when(
      mech_code %in% mechs_closing ~ 1,
      TRUE ~ 0
    ))

  df_msd_mechanisms_fy21 <- df_psnu %>%
    msd_mechanisms(fy = prev_fy) %>%
    mutate(closing = case_when(
      mech_code %in% mechs_closing ~ 1,
      TRUE ~ 0
    ))

  df_msd_mechanisms <- df_msd_mechanisms %>%
    bind_rows(df_msd_mechanisms_fy21, .) %>%
    select(-fiscal_year) %>%
    distinct()

  addWorksheet(wb_lookups, sheetName = "FY22 MSD Mechanisms")

  writeDataTable(wb_lookups,
                 sheet = "FY22 MSD Mechanisms",
                 x = df_msd_mechanisms)

  ## MSD PSNU Mechanisms ----
  df_msd_psnu_mechs <- df_psnu %>%
    msd_mechanisms(fy = c(prev_fy, curr_fy), psnu) %>%
    relocate(psnu, .after = 1) %>%
    mutate(
      closing = case_when(
        mech_code %in% mechs_closing ~ 1,
        TRUE ~ 0
      ),
      fundingagency = case_when(
        psnu == "Lagos" ~ "HHS/CDC & USAID",
        TRUE ~ fundingagency
      )
    )

  # FY23 - Look up mechanisms
  df_msd_psnu_mechs <- df_mech_flags %>%
    left_join(df_msd_mechanisms, by = "mech_code") %>%
    select(fiscal_year, psnu, fundingagency,
           mech_code, mech_name, primepartner) %>%
    mutate(
      closing = case_when(
        mech_code %in% mechs_closing ~ 1,
        TRUE ~ 0
      ),
      fundingagency = case_when(
        psnu == "Lagos" ~ "HHS/CDC & USAID",
        TRUE ~ fundingagency
      )
    )

  ## OU Gen Pop & KP Mechanisms
  kp_mechs <- c(81860, 81861)

  gp_mechs <- df_msd_mechanisms %>%
    filter(fundingagency == "USAID",
           closing != 1,
           mech_code %ni% kp_mechs) %>%
    pull(mech_code)

  # NON USAID Mechs
  non_usaid_mechs <- df_msd_mechanisms %>%
    filter(fundingagency != "USAID") %>%
    distinct(mech_code) %>%
    pull()

  # KP Disaggs
  kp_disaggs <- c("FSW", "MSM",
                  "People in prisons and other enclosed settings",
                  "PWID", "TG")

  # Gen Pop
  df_msd_psnu_mechs_genpop <- df_msd_psnu_mechs %>%
    filter(closing != 1 & mech_code %ni% kp_mechs) %>%
    group_by(fundingagency, psnu) %>%
    mutate(
      share = case_when(
        psnu == "Lagos" & mech_code %in% non_usaid_mechs ~ 1 / 2,
        psnu == "Lagos" & mech_code %ni% non_usaid_mechs ~ (1 / 2 / (n_distinct(mech_code) - 1)),
        TRUE ~ 1 / n_distinct(mech_code)
      )) %>%
    ungroup()

  # KP Only
  df_msd_psnu_mechs_kp <- df_msd_psnu_mechs %>%
    filter(closing != 1 & mech_code %in% kp_mechs) %>%
    group_by(fundingagency, psnu) %>%
    mutate(share = 1 / n_distinct(mech_code)) %>%
    ungroup() %>%
    bind_rows(df_msd_psnu_mechs_share, .)

  # Add closing mechs
  df_msd_psnu_mechs <- df_msd_psnu_mechs %>%
    filter(closing == 1) %>%
    bind_rows(df_msd_psnu_mechs_share, .) %>%
    arrange(fundingagency, psnu, mech_code)

  addWorksheet(wb_lookups, sheetName = "FY22 MSD PSNU Mechanisms")

  writeDataTable(wb_lookups,
                 sheet = "FY22 MSD PSNU Mechanisms",
                 x = df_msd_psnu_mechs)

  ## ## MSD PSNU Mechanisms Shares ----
  # NOTE: RISE - 81858 Does some Military Community Work?
  df_msd_psnu_mechs_share <- df_msd_psnu_mechs %>%
    partners_label() %>%
    select(-c(mech_name, primepartner, closing)) %>%
    pivot_wider(
      names_from = c(mech_code, partner),
      #names_from = mech_code,
      names_sort = TRUE,
      values_from = share) %>%
    arrange(fundingagency, psnu)

  addWorksheet(wb_lookups, sheetName = "FY22 MSD PSNU Mechanisms Share")

  writeDataTable(wb_lookups,
                 sheet = "FY22 MSD PSNU Mechanisms Share",
                 x = df_msd_psnu_mechs_share)

  # Save lookup data
  file_lookup = file.path(
    dir_dataout,
    paste0("DP MSD Lookups - ", curr_date(), ".xlsx"))

  saveWorkbook(wb = wb_lookups,
               file = file_lookup,
               overwrite = TRUE)

  file_lookup %>% open_path()

  # MSD - Sites x IM ----

  df_sites <- file_site_im %>% read_msd()

  df_sites %>% glimpse()
  df_sites %>% distinct(fundingagency)
  df_sites %>% distinct(psnu) %>% prinf()

  # USAID Military Facility - RISE 81858 [Z1raiKJH6yv]
  df_sites %>%
    filter(fiscal_year == curr_fy,
           fundingagency != "Dedup",
           orgunituid == "Z1raiKJH6yv") %>%
    distinct(orgunituid, sitename, typemilitary, mech_code)

  # CASCADE ----
  # FY22 Targets ----
  df_obs_cascade1a <- df_psnu %>%
    filter(fiscal_year == curr_fy) %>%
    clean_indicator() %>%
    filter(indicator %in% inds_cascade,
           standardizeddisaggregate == "Total Numerator") %>%
    mutate(
      indicator = case_when(
        indicator == "PMTCT_HEI_POS" ~ "HTS_TST_POS",
        TRUE ~ indicator
      )) %>%
    group_by(psnu, indicator) %>%
    summarise(across(targets, sum, na.rm = TRUE), .groups = "drop") %>%
    rename(targets_currfy = targets)

  # FY21 Targets & Results
  df_obs_cascade1b <- df_psnu %>%
    filter(fiscal_year == prev_fy) %>%
    clean_indicator() %>%
    filter(indicator %in% inds_cascade,
           standardizeddisaggregate == "Total Numerator") %>%
    mutate(
      indicator = case_when(
        indicator == "PMTCT_HEI_POS" ~ "HTS_TST_POS",
        TRUE ~ indicator
      )) %>%
    group_by(psnu, indicator) %>%
    summarise(across(c(targets, cumulative, starts_with("qtr")),
                     sum, na.rm = TRUE), .groups = "drop") %>%
    rename(target_prevfy = targets)

  # FY21 T&R + FY22 T
  df_obs_cascade1c <- df_psnu %>%
    filter(fiscal_year %in% c(prev_fy, curr_fy)) %>%
    clean_indicator() %>%
    filter(
      indicator %in% inds_cascade,
      standardizeddisaggregate %in% disaggs_cascade,
      !is.na(ageasentered)
    ) %>%
    mutate(
      indicator = case_when(
        indicator == "PMTCT_HEI_POS" ~ "HTS_TST_POS",
        TRUE ~ indicator
      )) %>%
    group_by(psnu, indicator, ageasentered, sex) %>%
    summarise(
      observed = sum(cumulative[fiscal_year == prev_fy], na.rm = T),
      planned = sum(targets[fiscal_year == curr_fy], na.rm = T),
      .groups = "drop"
    )

  df_obs_cascade1c <- df_obs_cascade1c %>%
    group_by(psnu, indicator) %>%
    summarise(across(all_of(c("observed", "planned")), sum, na.rm = T), .groups = "drop") %>%
    mutate(ageasentered = NA_character_, sex = NA_character_) %>%
    bind_rows(df_obs_cascade1c, .) %>%
    pivot_wider(names_from = indicator,
                names_sep = "_",
                values_from = c(observed, planned)) %>%
    arrange(psnu, ageasentered, sex)


  # Datim Reference Datasets ----

  #datim_session()

  #cop_mechs <- datapackr::getMechanismView()

  #cop_datapack <- datapackr::cop22_data_pack_schema

  #cop_coc <- datapackr::getValidCategoryOptions()

  #cop_de_coc <- datapackr::cop22_map_DataPack_DATIM_DEs_COCs %>% clean_names()

  df_cop_de_coc <- datapackr::cop22_map_DataPack_DATIM_DEs_COCs %>%
    dp_indicators()

  cop_indicators <- df_cop_de_coc %>%
    filter(indicator %ni% c("PP_PREV", "KP_MAT", "VMMC_CIRC")) %>%
    distinct(indicator) %>%
    arrange(indicator) %>%
    pull()

  cop_cascade_indicators <- c("HTS_TST", "HTS_TST_POS", "TX_NEW", "TX_CURR", "TX_PVLS")

  # DP Read IM Allocations sheets ----

  df_dp_psnu_im <- dp_read(filename = file_cop22_dp, sheet = "PSNUxIM")

  # Extract Sources
  df_dp_source <- df_dp_psnu_im %>%
    select(-starts_with("...")) %>%
    rename_with(.cols = everything(),
                .fn = ~str_replace(., "...[:digit:]{3}$", "_value")) %>%
    rename_with(.cols = everything(),
                .fn = ~str_replace(., "...[:digit:]{1,2}$", "_share"))

  df_dp_source <- df_dp_source %>%
    select(PSNU:Rollup, ends_with("DSD_share")) %>%
    pivot_longer(cols = ends_with("share"),
                 names_to = "mech_code",
                 values_to = "value")

  df_dp_source <- df_dp_source %>%
    separate(col = mech_code,
             into = c("mech_code", "support_type", "value_type"))

  df_dp_source <- df_dp_source %>%
    mutate(snu = str_extract(PSNU, ".*(?=\\[#)"),
           snu = str_trim(snu, side = "both"))


  # DP Get Results Contributions
  #df_dp_msd_indicators %>% glimpse()
  #df_psnu %>% glimpse()

  ## DP Get IMs
  dp_mechs <- df_dp_psnu_im %>%
    dp_clean() %>%
    dp_extract_shares() %>%
    distinct(attribute) %>%
    arrange(attribute) %>%
    pull()


  ## IM Shares by Agency ----
  df_msd_agency_im_results <- df_psnu %>%
    filter(fiscal_year == prev_fy,
           fundingagency != "Dedup",
           !(str_detect(psnu, "_Mil") & fundingagency == "USAID")) %>%
    left_join(df_cop_de_coc,
              by = c("indicator", "numeratordenom", "disaggregate",
                     "indicatortype" = "indicator_type", "ageasentered", "sex")) %>%
    filter(!is.na(indicator_code)) %>%
    mutate(
      keypop = case_when(
        str_detect(indicator, "KP") ~ otherdisaggregate,
        TRUE ~ NA_character_
      )
    ) %>%
    select(fundingagency, psnu, mech_code, indicator_code, indicator,
           numeratordenom, disaggregate, keypop,
           starts_with("qtr"), cumulative, targets)

  df_msd_agency_im_results <- df_msd_agency_im_results %>%
    left_join(df_mech_flags, by = c("psnu", "mech_code")) %>%
    mutate(mech_code = case_when(
      mech_code_new != mech_code ~ mech_code_new,
      mech_code == "17747" ~ "70255",
      TRUE ~ mech_code
    )) %>%
    select(-mech_code_new)

  df_msd_agency_im_shares <- df_msd_agency_im_results %>%
    group_by(fundingagency, mech_code, indicator_code, indicator,
             numeratordenom, disaggregate, keypop) %>% #, statushiv, modality, otherdisaggregate
    summarise(across(c(cumulative), sum, na.rm = TRUE), .groups = "drop") %>%
    rename(results = cumulative) %>%
    group_by(indicator_code, indicator,
             numeratordenom, disaggregate, keypop) %>%
    mutate(results_share = results / sum(results, na.rm = TRUE),
           results_share = if_else(is.nan(results_share), 0, results_share)) %>%
    ungroup()

  ## Results Share by Agency, PSNU, IM - Age, Sex ----

  ## LAGOS TX_CURR Shares ----
  tx_curr_lagos <- df_psnu %>%
    filter(fiscal_year == curr_fy,
           fundingagency != "Dedup",
           psnu == "Lagos",
           indicator == "TX_CURR",
           standardizeddisaggregate == "Total Numerator") %>%
    msd_im_transition(df_mech_flags, "psnu") %>%
    group_by(psnu, mech_code) %>%
    summarise(across(qtr1, sum, na.rm = T), .groups = "drop") %>%
    filter(qtr1 > 0) %>%
    mutate(share = qtr1 / sum(qtr1, na.rm = T),
           mech_code = case_when(
             mech_code == "81856" ~ "160525",
             TRUE ~ mech_code
           ))

  ## Results share by PSNU & IM ----

  df_msd_agency_im_results <- df_psnu %>%
    filter(fiscal_year == prev_fy,
           fundingagency != "Dedup",
           !(str_detect(psnu, "_Mil") & fundingagency == "USAID")) %>%
    msd_to_dp_indicators(.df_de_coc = df_cop_de_coc) %>%
    msd_im_transition(.df_mechs = df_mech_flags, 'psnu') %>%
    group_by(psnu, mech_code, indicator_code) %>%
    summarise(results = sum(cumulative, na.rm = TRUE), .groups = "drop") %>%
    group_by(psnu, indicator_code) %>%
    mutate(results_share = results / sum(results, na.rm = T)) %>%
    ungroup()

  ## Results share by PSNU, IM, Age, Sex ----
  df_msd_agency_im_age_sex_results <- df_psnu %>%
    filter(fiscal_year == prev_fy,
           fundingagency != "Dedup",
           !(str_detect(psnu, "_Mil") & fundingagency == "USAID")) %>%
    msd_to_dp_indicators(.df_de_coc = df_cop_de_coc) %>%
    msd_im_transition(.df_mechs = df_mech_flags, 'psnu') %>%
    group_by(psnu, mech_code, indicator_code, ageasentered, sex, keypop) %>%
    summarise(results = sum(cumulative, na.rm = TRUE), .groups = "drop")

  ## Fill in zero cum with state avg
  df_msd_agency_im_age_sex_results <- df_msd_agency_im_age_sex_results %>%
    group_by(psnu, indicator_target, age, sex, keypop) %>%
    mutate(
      cumulative = case_when(
        cumulative == 0 ~ round(mean(cumulative, na.rm =T), 0),
        TRUE ~ cumulative
      )
    )

  df_msd_agency_im_age_sex_shares <- df_msd_agency_im_age_sex_results %>%
    filter(mech_code %ni% c(14505, 17747)) %>%
    group_by(psnu, indicator_target, age, sex, keypop) %>%
    mutate(
      results = sum(cumulative, na.rm = TRUE),
      cdc_results = sum(cumulative[mech_code == "18657"], na.rm =T),
      share = case_when(
        psnu == "Lagos" & mech_code == "18677" ~ tx_curr_lagos$share[tx_curr_lagos$mech_code == "18677"],
        psnu == "Lagos" &  mech_code == "81861" &
          str_detect(indicator_target, "KP_.*|.KP.*T$|.KP.T$") ~ 1,
        psnu == "Lagos" &  mech_code == "81861" &
          str_detect(indicator_target, "KP_.*|.KP.*T$|.KP.T$", negate = TRUE) ~ tx_curr_lagos$share[tx_curr_lagos$mech_code == "81861"],
        psnu == "Lagos" &  mech_code == "160525" ~ tx_curr_lagos$share[tx_curr_lagos$mech_code == "160525"],
        TRUE ~ cumulative / results
      ),
      share = case_when(
        is.infinite(share) | is.nan(share) ~ NA_real_,
        share == 0 ~ NA_real_,
        TRUE ~ share
      )
    ) %>%
    ungroup()



  df_dp_source %>% glimpse()

  non_usaid_mechs

  df_dp_out <- df_dp_source %>%
    left_join(df_msd_agency_im_age_sex_shares,
              by = c('snu' = 'psnu',
                     'indicator_code' = 'indicator_target',
                     'Age' = 'age',
                     'Sex' = 'sex',
                     'KeyPop' = 'keypop',
                     'mech_code')
    ) %>%
    mutate(
      shares = case_when(
        is.na(shares) ~ NA_real_,
        TRUE ~ as.numeric(shares)
      ),
      shares = case_when(
        snu %in% psnu_usaid ~ share,
        TRUE ~ shares
      )) %>%
    mutate(shares = share) %>%
    # mutate(
    #   shares = as.numeric(shares),
    #   shares = case_when(
    #     snu %in% psnu_usaid | snu == "Lagos" ~ share,
    #     TRUE ~ shares
    #   )
    # ) %>%
    select(-c(snu:share)) %>%
    pivot_wider(names_from = mech_code,
                names_sort = TRUE,
                values_from = shares) %>%
    mutate(`Not PEPFAR` = NA_character_) %>%
    relocate(`Not PEPFAR`, .after = Rollup)

  df_dp_out %>% glimpse()



  # Update files

  file_dest <- curr_date() %>%
    paste0("_bk_", ., "_v7") %>%
    str_replace(string = file_cop22_dp,
                pattern = "_v7",
                replacement = .)

  df_dp_out %>%
    select(`Not PEPFAR`:last_col()) %>%
    dp_write(.df_out = .,
             file_in = file_cop22_dp,
             file_out = file_dest)

  file_dest %>% open_path()













### ------

  # Read DP
  df_dp_psnu_im <- df_dp_psnu_im %>%
    select(-starts_with("...")) %>%
    rename_with(.cols = everything(),
                .fn = ~str_replace(., "...[:digit:]{3}$", "_value")) %>%
    rename_with(.cols = everything(),
                .fn = ~str_replace(., "...[:digit:]{1,2}$", "_share"))

  df_dp_psnu_im <- df_dp_psnu_im %>%
    select(PSNU:Rollup, ends_with("DSD_share")) %>%
    pivot_longer(cols = ends_with("share"),
                 names_to = "mech_code",
                 values_to = "shares")

  df_dp_psnu_im <- df_dp_psnu_im %>%
    mutate(mech_code = str_remove(mech_code, "_DSD_share"))

  df_dp_psnu_im <- df_dp_psnu_im %>%
    mutate(snu = str_extract(PSNU, ".*(?=\\[#)"),
           snu = str_trim(snu, side = "both"))


  # Update Allocations -----
  df_dp_psnu_im_prop <- df_dp_psnu_im %>%
    left_join(
      df_msd_psnu_mechs_share,
      by = c("snu" = "psnu", "mech_code" = "mech_code")
    ) %>%
    mutate(
      shares = share,
      shares = case_when(
        KeyPop %in% kp_disaggs & mech_code %in% gp_mechs ~ NA_real_,
        is.na(KeyPop) & mech_code %in% kp_mechs ~ NA_real_,
        TRUE ~ shares
      ),
      mech_code = paste0(mech_code, "_DSD")) %>%
    select(-c(snu:share)) %>%
    pivot_wider(names_from = mech_code, values_from = shares) %>%
    mutate(`Not PEPFAR` = NA_character_) %>%
    relocate(`Not PEPFAR`, .after = Rollup)




  # Save a copy ----
  wb_allocations = createWorkbook()

  addWorksheet(wb = wb_allocations, sheetName = "PSNUxIM")

  writeDataTable(wb = wb_allocations,
                 sheet = "PSNUxIM",
                 x = df_dp_psnu_im_prop)

  saveWorkbook(wb = wb_allocations,
               file = file.path(dir_dataout, "DP IM Allocations.xlsx"),
               overwrite = TRUE)

  file.path(dir_dataout, "DP IM Allocations.xlsx") %>% open_path()

  # Load DP Workbook ----
  ws_psnu_im <- readWorkbook(xlsxFile = file_cop22_dp,
                             sheet = "PSNUxIM",
                             startRow = 8)

  wb_dp <- loadWorkbook(file = file_cop22_dp)

  saveWorkbook(
    wb = wb_dp,
    file = str_replace(file_cop22_dp, "_v5", "_bk_v5")
  )

  wb_dp2 <- loadWorkbook(file = str_replace(file_cop22_dp, "_v5", "_bk_v5"))

  writeData(
    wb = wb_dp2,
    sheet = "PSNUxIM",
    x = df_dp_psnu_im_prop %>% select(`Not PEPFAR`, ends_with("_DSD")),
    startCol = "I",
    startRow = 15,
    colNames = F
  )

  saveWorkbook(
    wb = wb_dp2,
    file = str_replace(file_cop22_dp, "_v5", "_bk_allocations_v5"),
    overwrite = TRUE
  )

  open_path(str_replace(file_cop22_dp, "_v5", "_bk_allocations_v5"))










  # TameDP Data Pack Processing ----

  df_dp <- file_cop22_dp %>% tame_dp(type = "PSNUxIM")

  #df_dp <- df_dp %>% filter(mech_code != "00000")

  ## Append Mechanism

  df_dp <- df_dp %>%
    get_names(df = .,
              cntry = cntry,
              datim_user = datim_user(),
              datim_password = datim_pwd())

  df_dp %>% glimpse()

  ## Check and update missing values

  ## SNUs
  df_dp %>% distinct(snu1, psnu) %>% prinf()

  df_dp <- df_dp %>% mutate(snu1 = ifelse(is.na(snu1), psnu, snu1))

  ## Agencies
  df_dp %>% distinct(fundingagency, psnu) %>% prinf()

  df_dp <- df_dp %>%
    mutate(fundingagency = case_when(
      str_detect(fundingagency, "Dedupe") ~ "Dedup",
      TRUE ~ fundingagency
    ))

  df_dp %>%
    filter(fundingagency != "Dedup") %>%
    distinct(fundingagency, psnu) %>%
    arrange(fundingagency, psnu) %>%
    prinf()

  ## Indicators

  df_dp %>% glimpse()

  df_dp_indicators <- df_dp %>%
    select(indicator, numeratordenom, indicatortype,
           standardizeddisaggregate, modality, statushiv, otherdisaggregate) %>%
    distinct() %>%
    arrange(indicator)

  df_dp_indicators %>% prinf()

  df_dp %>%
    distinct(indicator, numeratordenom, standardizeddisaggregate) %>%
    arrange(indicator) %>%
    prinf()

  ## Remove Dedup and Re-order columns
  df_dp <- df_dp %>%
    filter(fundingagency != "Dedup") %>%
    select(fiscal_year,
           operatingunit, countryname,
           snu1, psnu, psnuuid, snuprioritization,
           fundingagency, mech_code, mech_name, primepartner,
           indicator, numeratordenom, indicatortype,
           standardizeddisaggregate, ageasentered, sex,
           statushiv, modality, otherdisaggregate, targets)

  ## Summarise agencies geographies

  df_dp %>% distinct(fundingagency, psnu, mech_code) %>%
    arrange(fundingagency, psnu) %>%
    prinf()

  df_dp %>%
    group_by(fundingagency) %>%
    summarise(
      psnus = n_distinct(psnu),
      .groups = "drop")

  df_dp %>%
    select(psnu, fundingagency) %>%
    distinct(fundingagency, psnu) %>%
    arrange(fundingagency, psnu) %>%
    gt()

  df_dp_agencies_geo <- df_dp %>%
    distinct(fundingagency, psnu) %>%
    group_by(fundingagency) %>%
    summarise(
      psnus = paste(sort(psnu), collapse = ", "),
      .groups = "drop")

  df_dp_agencies_mechs <- df_dp %>%
    distinct(fundingagency, mech_code, mech_name, primepartner) %>%
    update_mechs() %>%
    clean_mechs() %>%
    clean_partners() %>%
    group_by(fundingagency) %>%
    arrange(mech_code) %>%
    summarise(
      mechanisms = paste(
        paste0("[", mech_code, "] - ", mech_name),
        collapse = ", "),
      .groups = "drop")

  df_dp_agencies_psnu_mechs <- df_dp %>%
    distinct(fundingagency, psnu, mech_code, mech_name, primepartner) %>%
    update_mechs() %>%
    clean_mechs() %>%
    clean_partners() %>%
    group_by(fundingagency, psnu) %>%
    arrange(mech_code) %>%
    summarise(
      mechanisms = paste(
        paste0("[", mech_code, "] - ", mech_name, " - ", primepartner),
        collapse = ", "),
      .groups = "drop")

  df_dp_agencies_psnu_mechs %>%
    gt() %>%
    gtsave(filename = file.path(dir_graphics, "Nigeria - FY23 Agency PSNU Mechnisms.png"))

  # Summarize targets by Agency

  df_dp_agency <- df_dp %>%
    mutate(pop_type = case_when(
      str_detect(standardizeddisaggregate, "KeyPop") ~ "KeyPop",
      TRUE ~ NA_character_
    )) %>%
    group_by(fundingagency, indicator, numeratordenom,
             pop_type, statushiv, modality, otherdisaggregate) %>%
    summarise(targets = sum(targets, na.rm = TRUE), .groups = "drop") %>%
    clean_agency() %>%
    pivot_wider(names_from = fundingagency, values_from = targets) %>%
    rowwise() %>%
    mutate(
      OU = sum(DOD, CDC, USAID, na.rm = T),
      `DOD Share` = percent(DOD/OU, 1),
      `CDC Share` = percent(CDC/OU, 1),
      `USAID Share` = percent(USAID/OU, 1)
    )



































  ## OLD Ref ----


  df_cop22 <- file_cop22_dp %>%
    read_dp() %>%
    select(-starts_with("...")) %>% #glimpse()
    rename_with(.cols = everything(),
                .fn = ~str_replace(., "...[:digit:]{3}$", "_value")) %>%
    rename_with(.cols = everything(),
                .fn = ~str_replace(., "...[:digit:]{1,2}$", "_share"))

  df_cop22 %>% glimpse()

  # COP IM Targets
  df_cop22_values <- df_cop22 %>%
    select(-ends_with("share")) %>%
    rename_with(.cols = ends_with("value"),
                .fn = ~str_remove(., "_value")) %>%
    pivot_longer(cols = ends_with("DSD"),
                 names_to = "Attribute",
                 values_to = "Value")

  # COP IM Allocations
  df_cop22_shares <- df_cop22 %>%
    select(PSNU:Rollup, ends_with("share")) %>%
    rename_with(.cols = ends_with("share"),
                .fn = ~str_remove(., "_share")) %>%
    pivot_longer(cols = ends_with("DSD"),
                 names_to = "Attribute",
                 values_to = "Share")

  # COP21 IM Allocations
  df_cop22_data <- df_cop22 %>% dp_extract_data()

  # COP21 Indicator / Disaggs ----

  df_cop_indicators0 <- df_cop22 %>%
    mutate(indicator = str_extract(indicator_code, "[^\\.]+"),
           tech_area = str_extract(indicator, "[^\\_]+")) %>%
    distinct(tech_area, indicator) %>%
    arrange(tech_area, indicator)

  df_cop_indicators1 <- df_cop22 %>%
    mutate(indicator = str_extract(indicator_code, "[^\\.]+"),
           tech_area = str_extract(indicator, "[^\\_]+")) %>%
    select(indicator_code, tech_area, indicator, Age, Sex, KeyPop) %>%
    distinct()

  df_cop_indicators2 <- df_cop22 %>%
    mutate(indicator = str_extract(indicator_code, "[^\\.]+"),
           tech_area = str_extract(indicator, "[^\\_]+")) %>%
    select(indicator_code, tech_area, indicator, Age, Sex, KeyPop) %>%
    clean_names() %>%
    left_join(df_cop_de_coc,
              by = c("indicator_code", "indicator", "age", "sex", "key_pop"))

  df_cop_indicators2 %>% glimpse()


# MUNGING ----

  df_psnu %>% glimpse()

  df_psnu %>% distinct(fundingagency)

  # USAID/Military Sites => These are community work reported above psnu
  df_sites %>%
    filter(fundingagency == agency,
           str_detect(psnu, "_Mil")) %>%
    distinct(fundingagency, psnu, community, sitename, facility, sitetype) %>%
    transpose() %>%
    unlist() %>%
    as_tibble(.name_repair = "unique")

  df_usaid_mil <- df_sites %>% filter(fundingagency == agency, str_detect(psnu, "_Mil"))

  df_usaid_mil %>%
    distinct(indicator) %>%
    prinf()

  # Mechanisms
  df_mechs <- df_sites %>%
    filter(fundingagency != "Dedup",
           !(fundingagency == agency & str_detect(psnu, "_Mil"))) %>%
    select(fiscal_year, fundingagency, psnu, mech_code, mech_name, primepartner) %>%
    distinct() %>%
    update_mechs() %>%
    partners_label()

  df_im_cov <- df_psnu %>%
    filter(fiscal_year == curr_fy,
           fundingagency != "Dedup",
           !(fundingagency == agency & str_detect(psnu, "_Mil"))) %>%
    distinct(fundingagency, psnu, mech_code) %>%
    clean_agency() %>%
    arrange(psnu) %>%
    group_by(psnu) %>%
    mutate(n_mech = n_distinct(mech_code)) %>%
    ungroup() %>%
    pivot_wider(names_from = mech_code,
                values_from = fundingagency,
                names_sort = TRUE)

  df_sites %>% glimpse()

  df_sites %>%
    filter(fiscal_year == curr_fy,
           fundingagency != "Dedup",
           !(fundingagency == agency & str_detect(psnu, "_Mil")),
           community != "Data reported above Community Level") %>%
    distinct(psnu, community, sitename)


  df_im_cov <- df_sites %>%
    filter(fiscal_year == curr_fy,
           fundingagency != "Dedup",
           !(fundingagency == agency & str_detect(psnu, "_Mil"))
           #,community != "Data reported above Community Level"
           ) %>%
    distinct(fundingagency, psnu, community, sitename, mech_code) %>%
    clean_agency() %>%
    arrange(psnu) %>%
    group_by(fundingagency, psnu, mech_code) %>%
    summarise(im_comms = n_distinct(community),
              im_sites = n_distinct(sitename)) %>%
    ungroup() %>%
    group_by(fundingagency, psnu) %>%
    mutate(share = im_sites / n_distinct(sitename) * 100) %>%
    ungroup() %>%
    select(-c(community, sitename, im_sites)) %>%
    pivot_wider(names_from = mech_code,
                values_from = im_share,
                names_sort = TRUE)


# VIZ ----

  im_closing <- c("81857",
                  "81857",
                  "18655",
                  "81856",
                  "18655",
                  "100222")

  im_incoming <- c("160521",
                   "160522",
                   "160524",
                   "160525",
                   "160523",
                   "160527")

  # USAID Mechs - list of States
  df_mechs %>%
    filter(fundingagency == agency) %>%
    gt()

  # IM Coverage ----
  plot_ims_cov <-
    df_mechs %>%
    filter(fundingagency == agency,
           primepartner != "TBD",
           fiscal_year == curr_fy) %>%
    mutate(
      mech_code = case_when(
        mech_code %in% im_closing ~ paste0(mech_code, "*"),
        mech_code %in% im_incoming ~ paste0(mech_code, "**"),
        TRUE ~ mech_code
    )) %>%
    select(-c(primepartner, mech_name)) %>%
    arrange(desc(fundingagency), partner, psnu) %>%
    group_by(mech_code, partner) %>%
    summarise(psnus = paste0(psnu, collapse = ", "), .groups = "drop") %>%
    arrange(partner) %>%
    mutate(id = row_number()) %>%
    relocate(id, .before = 1) %>%
    gt() %>%
    cols_label(id = "",
               mech_code = "MECH. Code",
               partner = "IP & MECH. NAME",
               psnus = "STATES") %>%
    tab_header(title = md("**USAID/NIGERIA - IMPLEMENTING MECHANISMS**")) %>%
    fmt_missing(columns = everything(), missing_text = "....") %>%
    tab_style(
      style = list(
        cell_text(weight = "bold"),
        cell_borders(side = c("top", "bottom"), color = grey70k, weight = px(2))
      ),
      locations = cells_column_labels(
        columns = everything()
      )
    ) %>%
    tab_style(
      style = list(
        cell_text(weight = "bold"),
        cell_fill(color = grey10k),
        cell_borders(side = "right", color = grey70k, weight = px(2))
      ),
      locations = cells_body(
        columns = "mech_code",
        rows = everything()
      )
    ) %>%
    tab_style(
      style = cell_text(color = usaid_red),
      locations = cells_body(
        columns = mech_code,
        rows = gt::matches("\\d*$")
      )
    ) %>%
    tab_source_note(source_note = "*  Mechanisms closing out") %>%
    tab_source_note(source_note = "** Mechanisms starting this year")

  gtsave(data = plot_ims_cov,
         filename = paste0(dir_graphics, "/Nigeria - Mechanisms State Coverage.png"),
         zoom = .6)

  gtsave(data = plot_ims_cov,
         filename = paste0(dir_graphics, "/Nigeria - Mechanisms State Coverage.pdf"),
         zoom = .6)

  ## Mechs - State COVERAGE
  plot_psnu_cov <-
    df_mechs %>%
      filter(fundingagency == agency,
             primepartner != "TBD",
             fiscal_year == curr_fy) %>%
      mutate(
        mech_code = case_when(
          mech_code %in% im_closing ~ paste0("<b>", mech_code, "*</b>"),
          mech_code %in% im_incoming ~ paste0("<b>", mech_code, "**</b>"),
          TRUE ~ paste0("<b>", mech_code, "</b>")
        ),
        partner = paste0(mech_code, " - ", partner)) %>%
      select(-c(primepartner, mech_name)) %>%
      arrange(desc(fundingagency), partner, psnu) %>%
      group_by(psnu) %>%
      summarise(partners = paste0(partner, collapse = ";  "), .groups = "drop") %>%
      arrange(psnu) %>%
      mutate(id = row_number()) %>%
      relocate(id, .before = 1) %>%
      gt() %>%
      cols_label(id = "",
                 psnu = "STATE",
                 partners = "MECHANISMS") %>%
      tab_header(title = md("**USAID/NIGERIA - IMPLEMENTING MECHANISMS**")) %>%
      fmt_missing(columns = everything(), missing_text = "....") %>%
      fmt_markdown(columns = partners, rows = everything()) %>%
      tab_style(
        style = list(
          cell_text(weight = "bold"),
          cell_borders(side = c("top", "bottom"), color = grey70k, weight = px(2))
        ),
        locations = cells_column_labels(
          columns = everything()
        )
      ) %>%
      tab_style(
        style = list(
          cell_text(weight = "bold"),
          cell_fill(color = grey10k),
          cell_borders(side = "right", color = grey70k, weight = px(2))
        ),
        locations = cells_body(
          columns = psnu,
          rows = everything()
        )
      ) %>%
      tab_source_note(source_note = "*  Mechanisms closing out") %>%
      tab_source_note(source_note = "** Mechanisms starting this year")

  gtsave(data = plot_psnu_cov,
         filename = paste0(dir_graphics, "/Nigeria - States Mechanisms Coverage.png"),
         zoom = .7)

  gtsave(data = plot_psnu_cov,
         filename = paste0(dir_graphics, "/Nigeria - States Mechanisms Coverage.pdf"),
         zoom = .7)


  # IM List ----
  plot_ims <- df_mechs %>%
    filter(primepartner != "TBD") %>%
    mutate(partner = paste0(mech_code, " - ", partner)) %>%
    select(-c(primepartner, mech_name)) %>%
    arrange(desc(fundingagency), psnu, partner) %>%
    pivot_wider(names_from = fiscal_year,
                names_sort = TRUE,
                values_from = partner) %>%
    mutate(
      mech_code = case_when(
        mech_code %in% im_closing ~ paste0("<b>", mech_code, "*</b>"),
        mech_code %in% im_incoming ~ paste0("<b>", mech_code, "**</b>"),
        TRUE ~ paste0("<b>", mech_code, "</b>")
      )
    ) %>%
    group_by(fundingagency, psnu) %>%
    gt() %>%
    cols_label(mech_code = "MECHANISM") %>%
    tab_header(title = md("**USAID/NIGERIA - LIST of MECHANISMS**")) %>%
    fmt_missing(columns = everything(), missing_text = "....") %>%
    fmt_markdown(columns = mech_code, rows = everything()) %>%
    tab_style(
      style = list(
        cell_text(weight = "bold"),
        cell_borders(side = c("top", "bottom"), color = grey70k, weight = px(2))
      ),
      locations = cells_column_labels(
        columns = everything()
      )
    ) %>%
    tab_style(
      style = list(
        cell_text(weight = "bold"),
        cell_borders(side = "right", color = grey70k, weight = px(2))
      ),
      locations = cells_body(
        columns = "mech_code",
        rows = everything()
      )
    ) %>%
    tab_style(
      style = list(
        cell_fill(color = grey10k),
        cell_text(style = "italic", weight = "bold")
      ),
      locations = cells_body(
        columns = c(fundingagency, mech_code),
        rows = everything()
      )
    ) %>%
    tab_source_note(source_note = "*  Mechanisms closing out") %>%
    tab_source_note(source_note = "** Mechanisms starting this year")

  gtsave(data = plot_ims,
         filename = paste0(dir_graphics, "/Nigeria - List of All Mechanisms.png"),
         zoom = .7)

  gtsave(data = plot_ims,
         filename = paste0(dir_graphics, "/Nigeria - List of All Mechanisms.pdf"),
         zoom = .7)






