
df_psnu %>%
  filter(str_detect(indicator, "HTS_TST_POS")) %>%
  distinct(indicator, standardizeddisaggregate, otherdisaggregate) %>%
  prinf()

df_hts <- df_psnu %>%
  filter(fiscal_year == curr_fy,
         fundingagency != "Dedup",
         indicator == "HTS_TST_POS",
         standardizeddisaggregate %in% c("Modality/Age/Sex/Result",
                                         "KeyPop/Result")) %>%
  mutate(
    otherdisaggregate = case_when(
      otherdisaggregate == "People in prisons and other enclosed settings" ~ "Prisons",
      TRUE ~ otherdisaggregate
    ),
    otherdisaggregate = case_when(
      !otherdisaggregate %in% c("FSW", "MSM", "PWID", "TG", "Prisons") ~ NA_character_,
      TRUE ~ otherdisaggregate
    ),
    otherdisaggregate = case_when(
      is.na(otherdisaggregate) ~ paste(sex, trendscoarse),
      TRUE ~ otherdisaggregate
    )) %>%
  group_by(psnu, indicator, otherdisaggregate) %>%
  summarise(across(cumulative, sum, na.rm = T), .groups = "drop")

df_hts <- df_hts %>%
  group_by(psnu, indicator) %>%
  summarise(cumulative = sum(cumulative[otherdisaggregate %in%
                                     c("Female 15+", "FSW")], na.rm = T),
            .groups = "drop") %>%
  mutate(otherdisaggregate = "Female 15++") %>%
  bind_rows(df_hts, .)

df_hts <- df_hts %>%
  group_by(psnu, indicator) %>%
  summarise(cumulative = sum(cumulative[otherdisaggregate %in% c("Male 15+", "MSM", "PWID")], na.rm = T), .groups = "drop") %>%
  mutate(otherdisaggregate = "Male 15++") %>%
  bind_rows(df_hts, .)

df_hts <- df_hts %>%
  pivot_wider(names_from = otherdisaggregate,
              names_sort = TRUE,
              values_from = cumulative) %>%
  rowwise() %>%
  mutate(
    `FSW Share` = percent(FSW / `Female 15++`),
    `MSM Share` = percent(MSM / `Male 15++`),
    `PWID Share` = percent(PWID / `Male 15++`)
  ) %>%
  relocate(`FSW Share`, .after = FSW) %>%
  relocate(`MSM Share`, .after = MSM) %>%
  relocate(`PWID Share`, .after = PWID)

df_hts %>%
  write_csv(file = file.path(dir_dataout, "NIGERIA COP22 HTS KP Share.csv"),
            na = "")

open_path(file.path(dir_dataout, "NIGERIA COP22 HTS KP Share.csv"))

