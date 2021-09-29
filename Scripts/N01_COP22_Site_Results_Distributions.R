#### OLD # ----
#### ####

# Partner / Mechs
df_sites_tx %>%
  distinct(mech_name)

df_sites_tx %>%
  distinct(primepartner)

df_sites_tx %>%
  distinct(mech_name, primepartner, partner) %>%
  arrange(partner)

df_sites_tx %>%
  filter(str_detect(period, "FY21"),
         partner == "Heartland Alliance - IMHIPP")

ggplot(diamonds, aes(x = price, y = cut)) +
  geom_density_ridges(scale = 4) +
  scale_y_discrete(expand = c(0, 0)) +     # will generally have to set the `expand` option
  scale_x_continuous(expand = c(0, 0)) +   # for both axes to remove unneeded padding
  coord_cartesian(clip = "off") +          # to avoid clipping of the very top of the top ridgeline
  theme_ridges()



df_sites_tx %>%
  filter(str_detect(period, "FY21")) %>%
  distinct(partner) %>%
  pull() %>%
  #first() %>%
  last() %>%
  map2("TX_NEW", function(.x, .y){
    df_sites_tx %>%
      filter(str_detect(period, "FY21"),
             partner == .x,
             indicator == .y) %>%
      ggplot(aes(partner, value)) +
      geom_boxplot() +
      geom_jitter() +
      #coord_flip() +
      facet_grid(indicator ~ period) +
      labs(x = "", y = "", title = paste0(.x, " (TX_NEW)")) +
      si_style()
  })

df_sites_tx %>%
  filter(str_detect(period, "FY21")) %>%
  filter(period == "FY21Q3") %>%
  ggplot(aes(partner, value)) +
  geom_boxplot() +
  facet_grid(mech_name ~ period) +
  si_style()

df_sites_tx <- df_sites_tx %>%
  group_by(fiscal_year, fundingagency, primepartner, mech_name, indicator) %>%
  summarise(across(starts_with("qtr"), sum, na.rm = T)) %>%
  ungroup() %>%

  select(-period_type) %>%
  filter(period %in% c("FY19Q4", "FY21Q1") | str_detect(period, "FY20"))

df_sites_tx %>%
  group_by(fundingagency, primepartner, mech_name, indicator) %>%
  summarise(total = sum(value, na.rm = TRUE),
            average = round(mean(value, na.rm = TRUE), 0),
            increase = total / average / 100) %>%
  ungroup() %>%
  pivot_longer(cols = total:increase,
               names_to = "period",
               values_to = "value") %>%
  bind_rows(df_sites_tx) %>%
  pivot_wider(names_from = period, values_from = value)





df_psnu %>%
  filter(str_to_lower(fundingagency) != "dedup",
         indicator %in% tx_inds,
         standardizeddisaggregate == "Total Numerator") %>%
  group_by(fiscal_year, fundingagency, indicator) %>%
  summarise(across(starts_with("qtr"), sum, na.rm = T)) %>%
  ungroup() %>%
  reshape_msd() %>%
  select(-period_type) %>%
  filter(period %in% c("FY19Q4", "FY21Q1") | str_detect(period, "FY20")) %>%
  pivot_wider(names_from = 'period', values_from = 'value') %>%
  rowwise() %>%
  mutate(increase = (FY21Q1 - FY19Q4) / FY19Q4) %>%
  ungroup()
