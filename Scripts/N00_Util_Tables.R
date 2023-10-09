

options(digits = 7)

## OVC SERV
df_ovc <- si_path() %>%
  return_latest("PSNU_IM_FY21.*_Nig") %>%
  read_psd() %>%
  filter(fiscal_year == meta$curr_fy,
         operatingunit == cntry,
         funding_agency == agency,
         str_detect(indicator, "OVC_"))

df_ovc %>% glimpse()

df_ovc %>% distinct(indicator)

df_ovc_serv <- df_ovc %>%
  filter(standardizeddisaggregate == "Total Numerator") %>%
  summarise(across(cumulative, \(x) sum(x, na.rm = T)),
            .by = c(fiscal_year, funding_agency, country, indicator))


# HTS

  df_hts <- si_path() %>%
    return_latest("PSNU_IM_FY21.*_Nig") %>%
    read_psd() %>%
    filter(#fiscal_year == meta$curr_fy,
           operatingunit == cntry,
           funding_agency %ni% c("Dedup", "Default"),
           str_detect(indicator, "HTS_"))


  df_hts %>% distinct(indicator)
  df_hts %>% distinct(indicator, standardizeddisaggregate)


  df_hts %>% distinct(modality) %>% pull()

  df_hts_idx <- df_hts %>%
    filter(indicator %in% c("HTS_TST", "HTS_TST_POS"),
           modality %in% c("Index", "IndexMod"),
           standardizeddisaggregate == "Modality/Age/Sex/Result") %>%
    summarise(across(c(cumulative, targets), \(x) sum(x, na.rm = T)),
              .by = c(fiscal_year, funding_agency, indicator, modality))

  df_hts_idx <- df_hts_idx %>%
    summarise(across(c(cumulative, targets), \(x) sum(x, na.rm = T)),
              .by = c(fiscal_year, indicator, modality)) %>%
    mutate(funding_agency = "OU") %>%
    bind_rows(df_hts_idx, .)

  df_hts_idx <- df_hts_idx %>%
    rowwise() %>%
    mutate(achievement = cumulative / targets * 100) %>%
    ungroup() %>%
    pivot_longer(names_to = "metric", values_to = "value",
                 cols = c(cumulative, targets, achievement))

  df_hts_idx %>% glimpse()

  df_hts_idx <- df_hts_idx %>%
    filter(metric == 'cumulative') %>%
    group_by(fiscal_year, funding_agency, modality) %>%
    summarise(positivity = (value[indicator == 'HTS_TST_POS'] /
                         value[indicator == 'HTS_TST'] * 100)) %>%
    ungroup() %>%
    rename(value = positivity) %>%
    mutate(metric = "positivity", indicator = NA) %>%
    bind_rows(df_hts_idx, .) %>%
    select(fiscal_year, funding_agency, modality, metric, indicator, value) %>%
    arrange(fiscal_year, funding_agency, modality, metric) %>%
    pivot_wider(names_from = fiscal_year, values_from = value)

  df_hts_idx %>%
    clean_agency() %>%
    write_csv(na = "",
              file = file.path(dir_dataout, "Nigeria - HTS Historical Performance.csv"))




