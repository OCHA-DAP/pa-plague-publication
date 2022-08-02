###############
#### SETUP ####
###############

source("01_data_prep.R")
library(gghdx)
library(sf)

# set the HDX theme for plotting
gghdx()

# shapefile for mapping
fp_codab <- file.path(
  dir_aa,
  "public",
  "raw",
  "mdg",
  "cod_ab"
)

sf_adm3 <- read_sf(
  file.path(
    fp_codab,
    "mdg_admbnda_adm3_BNGRC_OCHA_20181031.shp"
  )
)

sf_adm0 <- read_sf(
  file.path(
    fp_codab,
    "mdg_admbnda_adm0_BNGRC_OCHA_20181031.shp"
  )
)

#####################
#### URBAN CASES ####
#####################

p_urb <- ggplot(
  data = df_urb
) +
  geom_area(
    aes(
      x = date,
      y = cases
    ),
    fill = hdx_hex("tomato-light")
  ) +
  labs(
    title = "Weekly cases of pneumonic plague in urban areas of Madagascar",
    y = "Total cases",
    x = ""
  ) +
  scale_y_continuous_hdx()

ggsave(
  plot = p_urb,
  filename = here(
    dir_plot_save,
    "urban_cases.png"
  ),
  height = 2.5,
  width = 7,
  units = "in"
)

#######################
#### COUNTRY CASES ####
#######################

p_country <- ggplot(
  data = df_country
) +
  geom_area(
    aes(
      x = date,
      y = cases
    ),
    fill = hdx_hex("tomato-light")
  ) +
  labs(
    title = "Weekly cases of pneumonic and bubonic plague in Madagascar",
    y = "Total cases",
    x = ""
  ) +
  scale_y_continuous_hdx()

ggsave(
  plot = p_country,
  filename = here(
    dir_plot_save,
    "country_cases.png"
  ),
  height = 2.5,
  width = 7,
  units = "in"
)

################################
#### 3 WEEK ROLLING COUNTRY ####
################################

df_area_country <- df_country %>%
  filter(
    year != 2017
  ) %>%
  mutate(
    year_flag = ifelse(year < 2017, "2012 - 2016", "2018 - 2022")
  ) %>%
  group_by(
    year_flag,
    week
  ) %>%
  summarize(
    cases_min = min(cases_3_weeks, na.rm = TRUE),
    cases_max = max(cases_3_weeks, na.rm = TRUE),
    .groups = "drop"
  )

df_line_country <- df_country %>%
  filter(
    year == 2017
  )

p_rolling <- ggplot() +
  geom_area(
    data = df_area_country,
    mapping = aes(
      x = week,
      y = cases_max,
      fill = year_flag
    ),
    alpha = 0.5
  ) +
  geom_line(
    data = df_line_country,
    mapping = aes(
      x = week,
      y = cases_3_weeks
    ),
    color = hdx_hex("gray-dark"),
    lwd = 1
  ) +
  scale_x_continuous(
    breaks = seq(1, 53, 13),
    labels = c("January", "April", "July", "October", "December")
  ) +
  geom_text_hdx(
    data = data.frame(
      x = 44.1,
      y = 200,
      label = "2017"
    ),
    mapping = aes(
      x = x,
      y = y,
      label = label
    ),
    angle = 278,
    fontface = "bold"
  ) +
  scale_y_continuous_hdx() +
  labs(
    y = "Total cases",
    x = "",
    fill = "",
    title = "Pneumonic and bubonic plague cases in Madagascar",
    subtitle = "Rolling sum of cases in the past 3 weeks"
  )

ggsave(
  plot = p_rolling,
  filename = here(
    dir_plot_save,
    "rolling_cases.png"
  ),
  height = 4,
  width = 8
)

##########################
#### PLOT URBAN AREAS ####
##########################

# create urban areas
sf_urb <- sf_adm3 %>%
  mutate(
    adm3_pcode = str_replace(ADM3_PCODE, "MG", "MDG")
  ) %>%
  left_join(
    df_urb_classification,
    by = "adm3_pcode"
  ) %>%
  filter(urban)

# labels for the map
sf_urb_label <- tribble(
  ~x, ~y, ~label,
  47.8, -18.6, "Antananarivo",
  48.8, -17.9, "Toamisina",
  47.1, -20.02, "Antsirabe",
  48.1, -22.35, "Manakara",
  47.5, -23, "Farafangana",
  49.4, -13.2, "Vohemar",
  46.5, -15.8, "Mahajanga",
  47.7, -15.4, "Boriziny",
  49, -15.95, "Mandritsara",
  43.9, -23.55, "Toliara",
  47.25, -21.7, "Fianarantsoa",
  43.6, -20.1, "Morondava",
  48.6, -12.1, "Antsirana",
  47.8, -13.2, "Nosy Be",
  47.3, -20.75, "Ambositra",
  47.6, -19.55, "Ambatolampy",
  48.8, -17.2, "Mahambo"
) %>%
  st_as_sf(
    coords = c("x", "y"),
    crs = 4326
  )

p_urb_class <- ggplot() +
  geom_sf(
    data = sf_adm0,
    fill = hdx_hex("gray-light"),
    lwd = 0
  ) +
  geom_sf(
    data = sf_urb,
    lwd = 0,
    fill = hdx_hex("mint-dark")
  ) +
  coord_sf(
    datum = NA
  ) +
  geom_sf_text(
    data = sf_urb_label,
    mapping = aes(
      label = label
    )
  ) +
  labs(
    title = "Communes (admin3) in Madagascar classified as urban",
    subtitle = "Based on Global Human Settlements Degree of Urbanisation methodology",
    x = "",
    y = ""
  )

ggsave(
  plot = p_urb_class,
  filename = here(
    dir_plot_save,
    "urban_classification.png"
  ),
  height = 10,
  width = 6.5,
  units = "in"
)

###############################
#### TRIGGER VISUALIZATION ####
###############################

min_date = "2017-08-01"
max_date = "2017-12-01"

df_urb_trigger <- df_urb %>%
  filter(
    date >= min_date,
    date <= max_date
  ) %>%
  mutate(
    type = "urban"
  )

df_country_trigger <- df_country %>%
  filter(
    date >= min_date,
    date <= max_date
  ) %>%
  mutate(
    type = "country"
  ) %>%
  select(
    -cases
  ) %>%
  rename(
    cases = cases_3_weeks
  )

df_triggers <- bind_rows(
  df_urb_trigger,
  df_country_trigger
)

lbls <- c(
  "urban" = "Urban cases, pneumonic plague, weekly",
  "country" = "Countrywide cases, pneumonic and bubonic plague, 3 week rolling sum"
)

# Plot the cases together and the trigger thresholds
p_triggers <- df_triggers %>%
  ggplot(
    aes(
      x = date,
      y = cases
    )
  ) +
  geom_area(
    fill = hdx_hex("tomato-ultra-light")
  ) +
  facet_wrap(
    ~type,
    ncol = 1,
    scales = "free_y",
    labeller = as_labeller(lbls)
  ) +
  labs(
    x = "",
    y = "Cases",
    title = "Plague cases in 2017 and potential triggering points"
  ) +
  geom_hline(
    data = data.frame(
      cases = c(5, 50),
      type = c("urban", "country")
    ),
    mapping = aes(
      yintercept = cases
    ),
    color = hdx_hex("gray-medium")
  ) +
  geom_point(
    data = data.frame(
      cases = c(5, 52),
      date = as.Date(c("2017-09-11", "2017-09-18")),
      type = c("urban", "country")
    ),
    color = hdx_hex("gray-dark"),
    size = 3
  ) +
  geom_text_hdx(
    data = data.frame(
      cases = c(10, 65),
      date = as.Date(c("2017-07-10", "2017-07-10")),
      type = c("urban", "country"),
      label = c(
        "Threshold: 5 cases in a single week",
        "Threshold: 50 cases in the past 3 weeks"
      )
    ),
    mapping = aes(
      label = label
    ),
    hjust = 0,
    size = 3
  ) +
  geom_segment(
    data = data.frame(
      x = as.Date(c("2017-09-11", "2017-09-18")),
      xend = as.Date(c("2017-09-11", "2017-09-18")),
      y = c(50, 200),
      yend = c(5, 52),
      type = c("urban", "country")
    ),
    mapping = aes(
      x = x,
      xend = xend,
      y = y, 
      yend = yend
    ),
    color = hdx_hex("gray-dark")
  ) +
  geom_label_hdx(
    data = data.frame(
      cases = c(50, 200),
      date = as.Date(c("2017-09-12", "2017-09-19")),
      type = c("urban", "country"),
      label = c(
        str_wrap("Trigger based on urban caseloads would have triggered a week earlier around the 11th of September.", width = 40),
        str_wrap("Country-level trigger would have activated around the 18th of September.", width = 40)
      )
    ),
    mapping = aes(
      label = label
    ),
    hjust = 1,
    size = 3
  ) +
  scale_y_continuous_hdx()

ggsave(
  plot = p_triggers,
  filename = file.path(
    dir_plot_save,
    "trigger_points.png"
  ),
  width = 8,
  height = 4,
  units = "in"
)

#####################################
#### WHY NOT SINGLE WEEK TRIGGER ####
#####################################

# get data frame of consecutive periods of triggering using weekly cases
df_cases_flag <- df_country %>%
  filter(
    cases >= 15
  ) %>%
  mutate(
    date_groups = cumsum(date - lag(date, default = as.Date("2011-12-31")) != 7)
  ) %>%
  group_by(
    date_groups
  ) %>%
  summarize(
    min_date = min(date) - 3, # padding dates so area is visible in graph
    max_date = max(date) + 3
  ) %>%
  mutate(
    name = "cases"
  )

# get data frame of consecutive periods of triggering using rolling cases
df_rolling_flag <- df_country %>%
  filter(
    cases_3_weeks >= 50
  ) %>%
  mutate(
    date_groups = cumsum(date - lag(date, default = as.Date("2011-12-31")) != 7)
  ) %>%
  group_by(
    date_groups
  ) %>%
  summarize(
    min_date = min(date) - 3, # padding dates so area is visible in graph
    max_date = max(date) + 3
  ) %>%
  mutate(
    name = "cases_3_weeks"
  )

df_flags <- bind_rows(
  df_cases_flag,
  df_rolling_flag
)
 
# plot with long data

case_lbls <- c(
  "cases" = "Countrywide cases, pneumonic and bubonic plague, trigger on 15 or more weekly cases",
  "cases_3_weeks" = "Countrywide cases, pneumonic and bubonic plague, trigger on 50 or more cases over past 3 weeks"
)

p_flags <- df_country %>%
  pivot_longer(
    cases:cases_3_weeks
  ) %>%
  ggplot() +
  geom_rect(
    data = df_flags,
    mapping = aes(
      xmin = min_date,
      xmax = max_date
    ),
    ymin = 0,
    ymax = Inf,
    fill = hdx_hex("gray-dark")
  ) +
  geom_area(
    aes(
      x = date,
      y = value
    ),
    fill = hdx_hex("tomato-light")
  ) +
  facet_wrap(
    ~name,
    ncol = 1,
    scales = "free_y",
    labeller = as_labeller(case_lbls)
  ) +
  labs(
    x = "",
    y = "Cases",
    title = "Frequency of triggering using weekly vs. 3-week rolling sum of cases",
    subtitle = "Triggers set to ensure warning would occur on September 18 2017"
  ) +
  geom_label_hdx(
    data = data.frame(
      x = as.Date("2018-06-01"),
      y = 175,
      name = "cases_3_weeks"
    ),
    aes(
      x = x,
      y = y
    ),
    label = str_wrap("3-week rolling sum trigger is much less sensitive in years other than 2017 when compared to triggering on weekly cases.", width = 40),
    hjust = 0
  ) +
  geom_text_hdx(
    data = data.frame(
      x = as.Date("2015-11-01"),
      y = 75,
      name = "cases"
    ),
    aes(
      x = x,
      y = y
    ),
    label = "Trigger\nperiods",
    hjust = 0
  ) +
  scale_y_continuous_hdx()

ggsave(
  plot = p_flags,
  filename = file.path(
    dir_plot_save,
    "flag_comparisons.png"
  ),
  height = 4,
  width = 8,
  units = "in"
)

  
