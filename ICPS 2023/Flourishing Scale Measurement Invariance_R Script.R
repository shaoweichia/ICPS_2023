############################################
# Flourishing Scale Measurement Invariance #
# Last updated: 2022-01-03                 #
############################################

##### PREP ##### ---------------------------
#### Load required packages
suppressPackageStartupMessages({
  library(tidyverse)
  library(dplyr)
  library(data.table)
  library(broom)
  library(foreign)
  library(psych)
  library(sem)
  library(lavaan)
  library(semPlot)
  library(xlsx)
  library(CTT)
  library(leaps)
  library(car)
  library(MASS)
})

#### Initialize file directory for export
file.dir <- paste0(getwd(), "/Floushing Scale Measurement Invariance.xlsx")

#### Initialize functions
### Convert demographic variable from plus system to MECE
PlusToMece <- function(data, demog.var, new.col.name) {
  dt.copy <- data.table(data)
  
  dt.copy[, var.count := apply(.SD, 1, sum, na.rm = TRUE),
          .SDcols = demog.var]
  
  dt.copy[var.count > 1, {{new.col.name}} := "multi"]
  dt.copy[var.count == 0, {{new.col.name}} := NA]
  
  dt.list <- apply(data.table(demog.var), 1, function(x) {
    dt.copy[get(x) == 1 &
              var.count == 1, {{new.col.name}} := x]
  })
  
  final.dt <- last(dt.list)
  final.dt[, var.count := NULL]
  final.df <- data.frame(final.dt)
  return(final.df)
}

#### Load data
dat.16.17 <- read.csv("2016-2017 HMS plus inst and vars.csv")
dat.17.18 <- read.csv("2017-2018 HMS plus inst and vars.csv")
dat.18.19 <- read.csv("2018-2019 HMS plus inst and vars.csv")
dat.19.20 <- read.csv("2019-2020 HMS plus inst and vars.csv")
dat.20.21 <- read.csv("HMS_national20202021_withinstvars_PUBLIC.csv")

#### Subsetting data
dat.16.17.sub <- dat.16.17 %>%
  filter(international == 0) %>%
  dplyr::select(
    responseid,
    survey,
    starts_with("gender"),
    starts_with("race"),
    starts_with("diener"),
    starts_with("phq9"),
    starts_with("gad7"),
    starts_with("stress"),
    starts_with("AAQ"),
    starts_with("BRS")
  )

dat.17.18.sub <- dat.17.18 %>%
  filter(international == 0) %>%
  dplyr::select(
    responseid,
    survey,
    starts_with("gender"),
    starts_with("race"),
    starts_with("diener"),
    starts_with("phq9"),
    starts_with("gad7"),
    starts_with("stress"),
    starts_with("AAQ"),
    starts_with("BRS")
  )

dat.18.19.sub <- dat.18.19 %>%
  filter(international == 0) %>%
  dplyr::select(
    responseid,
    survey,
    starts_with("gender"),
    starts_with("race"),
    starts_with("diener"),
    starts_with("phq9"),
    starts_with("gad7"),
    starts_with("stress"),
    starts_with("AAQ"),
    starts_with("BRS")
  )

dat.19.20.sub <- dat.19.20 %>%
  filter(international == 0) %>%
  dplyr::select(
    responseid,
    survey,
    starts_with("gender"),
    starts_with("race"),
    starts_with("diener"),
    starts_with("phq9"),
    starts_with("gad7"),
    starts_with("stress"),
    starts_with("AAQ"),
    starts_with("BRS")
  )

dat.20.21.sub <- dat.20.21 %>%
  filter(international == 0) %>%
  dplyr::select(
    responseid,
    survey,
    starts_with("gender"),
    starts_with("race"),
    starts_with("diener"),
    starts_with("phq9"),
    starts_with("gad7"),
    starts_with("stress"),
    starts_with("AAQ"),
    starts_with("BRS")
  )

##### DATA WRANGLING + MANIPULATION ##### ---------------------------
#### Initialize tables for demographic variables
gender <- tribble(
  ~gender, ~gender_code, ~code,
  "male", "gender_male", 1,
  "female", "gender_female", 2,
  "trans.male", "gender_transm", 3,
  "trans.female", "gender_transf", 4,
  "genderqueer.nonconforming", "gender_queernc", 5,
  "self.identify", "gender_selfID", 6,
  "non.binary", "gender_nonbin", 7,
)

race <- tribble(
  ~race, ~race_code, ~code,
  "black", "race_black", 1,
  "native.american", "race_ainaan", 2,
  "asian", "race_asian", 3,
  "hispanic.latinx", "race_his_temp", 4,
  "pacific.islander", "race_pi", 5,
  "middle.eastern", "race_mides", 6,
  "white", "race_white", 7,
  "other", "race_other", 8,
  "multi", "multi", 9
)

#### Save race column name (from data) as a vector
gender.var <- gender %>% pull(gender_code)
race.var <- race %>%
  pull(race_code) %>%
  str_subset("multi", negate = TRUE)

#### Clean up demographic variables
dat.16.17.clean <- dat.16.17.sub %>%
  PlusToMece(., race.var, "race_mece") %>%
  left_join(gender[c("gender", "code")],
            by = c("gender" = "code")) %>%
  left_join(race[c("race", "race_code")],
            by = c("race_mece" = "race_code")) %>%
  dplyr::select(
    !contains(c("gender_", "race_")),
    -gender
  ) %>% 
  plyr::rename(c("gender.y" = "gender"))

dat.17.18.clean <- dat.17.18.sub %>%
  PlusToMece(., race.var, "race_mece") %>%
  left_join(gender[c("gender", "code")],
            by = c("gender" = "code")) %>%
  left_join(race[c("race", "race_code")],
            by = c("race_mece" = "race_code")) %>%
  dplyr::select(
    !contains(c("gender_", "race_")),
    -gender
  ) %>% 
  plyr::rename(c("gender.y" = "gender"))

dat.18.19.clean <- dat.18.19.sub %>%
  PlusToMece(., race.var, "race_mece") %>%
  left_join(gender[c("gender", "code")],
            by = c("gender" = "code")) %>%
  left_join(race[c("race", "race_code")],
            by = c("race_mece" = "race_code")) %>%
  dplyr::select(
    !contains(c("gender_", "race_")),
    -gender
  ) %>% 
  plyr::rename(c("gender.y" = "gender"))

dat.19.20.clean <- dat.19.20.sub %>%
  PlusToMece(., race.var, "race_mece") %>%
  left_join(gender[c("gender", "code")],
            by = c("gender" = "code")) %>%
  left_join(race[c("race", "race_code")],
            by = c("race_mece" = "race_code")) %>%
  dplyr::select(
    !contains(c("gender_", "race_")),
    -gender
  ) %>% 
  plyr::rename(c("gender.y" = "gender"))

dat.20.21.clean <- dat.20.21.sub %>%
  PlusToMece(., gender.var, "gender_mece") %>%
  PlusToMece(., race.var, "race_mece") %>%
  left_join(gender[c("gender", "gender_code")],
            by = c("gender_mece" = "gender_code")) %>%
  left_join(race[c("race", "race_code")],
            by = c("race_mece" = "race_code")) %>%
  dplyr::select(!contains(c("gender_", "race_")))

#### Combine all data
dat.clean <- dplyr::bind_rows(
  dat.16.17.clean,
  dat.17.18.clean,
  dat.18.19.clean,
  dat.19.20.clean,
  dat.20.21.clean
) %>%
  dplyr::select(-Stress_Mindset) %>%
  mutate(
    gender = as.factor(gender),
    race = as.factor(race),
    race_gender = case_when(
      !is.na(gender) & gender != "other" &
        !is.na(race) & race != "other" ~ paste(race, gender, sep = "."))
  )
)

##### ANALYSIS ##### ---------------------------
#### Get descriptives
race.freq <- dat.clean %>%
  filter(!is.na(race),
         race != "other") %>%
  count(race)

gender.freq <- dat.clean %>%
  filter(!is.na(gender),
         gender != "other") %>%
  count(gender)

race.gender.freq <- dat.clean %>%
  filter(!is.na(race_gender),
         race_gender != "other",
         str_detect(race_gender, "male"),
         str_detect(race_gender, "trans", negate = TRUE),
         str_detect(race_gender, "multi", negate = TRUE)) %>%
  count(race_gender) %>%
  filter(n >= 100)

#### Specify the model
fs.model <- '
  fs =~ diener1 + diener2 + diener3 + diener4 +
    diener5 + diener6 + diener7 + diener8
'

#### Run model and check fit measures
fit.cfa.fs.overall <- sem(fs.model,
                          data = dat.clean,
                          std.lv = TRUE)

fit.cfa.fs.overall.summ <- tidy(fit.cfa.fs.overall)

fit.cfa.fs.overall.stats <- glance(fit.cfa.fs.overall)

#### Gender
### Subset data
gender.dat <- dat.clean %>%
  filter(gender == "male" |
           gender == "female")

### Test for configural invariance
fit.cfa.fs.gender <- sem(fs.model,
                         data = gender.dat,
                         group = "gender",
                         group.equal = c(''),
                         std.lv = TRUE)

fit.cfa.fs.gender.summ <- tidy(fit.cfa.fs.gender)

fit.cfa.fs.gender.stats <- glance(fit.cfa.fs.gender)

### Test for metric invariance
fit.cfa.fs.gender.metric <- sem(fs.model,
                                data = gender.dat,
                                group = "gender",
                                group.equal = c("loadings"),
                                std.lv = TRUE)

fit.cfa.fs.gender.metric.summ <- tidy(fit.cfa.fs.gender.metric)

fit.cfa.fs.gender.metric.stats <- glance(fit.cfa.fs.gender.metric)

gender.metric.invariance <- anova(fit.cfa.fs.gender,
                                  fit.cfa.fs.gender.metric) %>% tidy()

### Test for scalar invariance
fit.cfa.fs.gender.scalar <- sem(fs.model,
                                data = gender.dat,
                                group = "gender",
                                group.equal = c("loadings", "intercepts"),
                                std.lv = TRUE)

fit.cfa.fs.gender.scalar.summ <- tidy(fit.cfa.fs.gender.scalar)

fit.cfa.fs.gender.scalar.stats <- glance(fit.cfa.fs.gender.scalar)

gender.scalar.invariance <- anova(fit.cfa.fs.gender.metric,
                                  fit.cfa.fs.gender.scalar) %>% tidy()

### Export results
## Prep output for export
gender.fit.stats <- bind_rows(
  config = fit.cfa.fs.gender.stats,
  metric = fit.cfa.fs.gender.metric.stats,
  scalar = fit.cfa.fs.gender.scalar.stats,
  .id = "MI test"
)

gender.anova <- bind_rows(
  metric = gender.metric.invariance,
  scalar = gender.scalar.invariance,
  .id = "MI test"
)

write.xlsx(x = data.frame(gender.fit.stats),
           file = file.dir,
           sheetName = "Gender MI Fit Stats",
           row.names = FALSE,
           append = TRUE)

write.xlsx(x = data.frame(gender.anova),
           file = file.dir,
           sheetName = "Gender MI Chi-sq Tests",
           row.names = FALSE,
           append = TRUE)

#### Race
#### Subset data
race.dat <- dat.clean %>%
  filter(!is.na(race),
         race != "multi",
         race != "other")

### Test for configural invariance
fit.cfa.fs.race <- sem(fs.model,
                       data = race.dat,
                       group = "race",
                       std.lv = TRUE)

fit.cfa.fs.race.summ <- tidy(fit.cfa.fs.race)

fit.cfa.fs.race.stats <- glance(fit.cfa.fs.race)

### Test for metric invariance
fit.cfa.fs.race.metric <- sem(fs.model,
                              data = race.dat,
                              group = "race",
                              group.equal = c("loadings"),
                              std.lv = TRUE)

fit.cfa.fs.race.metric.summ <- tidy(fit.cfa.fs.race.metric)

fit.cfa.fs.race.metric.stats <- glance(fit.cfa.fs.race.metric)

race.metric.invariance <- anova(fit.cfa.fs.race,
                                fit.cfa.fs.race.metric) %>% tidy()

### Test for scalar invariance
fit.cfa.fs.race.scalar <- sem(fs.model,
                              data = race.dat,
                              group = "race",
                              group.equal = c("loadings", "intercepts"),
                              std.lv = TRUE)

fit.cfa.fs.race.scalar.summ <- tidy(fit.cfa.fs.race.scalar)

fit.cfa.fs.race.scalar.stats <- glance(fit.cfa.fs.race.scalar)

race.scalar.invariance <- anova(fit.cfa.fs.race.metric,
                                fit.cfa.fs.race.scalar) %>% tidy()

### Export results
## Prep output for export
race.fit.stats <- bind_rows(
  config = fit.cfa.fs.race.stats,
  metric = fit.cfa.fs.race.metric.stats,
  scalar = fit.cfa.fs.race.scalar.stats,
  .id = "MI test"
)

race.anova <- bind_rows(
  metric = race.metric.invariance,
  scalar = race.scalar.invariance,
  .id = "MI test"
)

write.xlsx(x = data.frame(race.fit.stats),
           file = file.dir,
           sheetName = "Race MI Fit Stats",
           row.names = FALSE,
           append = TRUE)

write.xlsx(x = data.frame(race.anova),
           file = file.dir,
           sheetName = "Race MI Chi-sq Tests",
           row.names = FALSE,
           append = TRUE)

#### Race-Gender
### Subset data
race.gender.dat <- dat.clean %>%
  filter(!is.na(race_gender),
         gender == "male" |
           gender == "female",
         race != "multi",
         race != "other",
         race_gender != "other")

### Test for configural invariance
fit.cfa.fs.race.gender <- sem(fs.model,
                              data = race.gender.dat,
                              group = "race_gender",
                              std.lv = TRUE)

fit.cfa.fs.race.gender.summ <- tidy(fit.cfa.fs.race.gender)

fit.cfa.fs.race.gender.stats <- glance(fit.cfa.fs.race.gender)

### Test for metric invariance
fit.cfa.fs.race.gender.metric <- sem(fs.model,
                                     data = race.gender.dat,
                                     group = "race_gender",
                                     group.equal = c("loadings"),
                                     std.lv = TRUE)

fit.cfa.fs.race.gender.metric.summ <- tidy(fit.cfa.fs.race.gender.metric)

fit.cfa.fs.race.gender.metric.stats <- glance(fit.cfa.fs.race.gender.metric)

race.gender.metric.invariance <- anova(fit.cfa.fs.race.gender,
                                       fit.cfa.fs.race.gender.metric) %>% tidy()

### Test for scalar invariance
fit.cfa.fs.race.gender.scalar <- sem(fs.model,
                                     data = race.gender.dat,
                                     group = "race_gender",
                                     group.equal = c("loadings", "intercepts"),
                                     std.lv = TRUE)

fit.cfa.fs.race.gender.scalar.summ <- tidy(fit.cfa.fs.race.gender.scalar)

fit.cfa.fs.race.gender.scalar.stats <- glance(fit.cfa.fs.race.gender.scalar)

race.gender.scalar.invariance <- anova(fit.cfa.fs.race.gender.metric,
                                       fit.cfa.fs.race.gender.scalar) %>% tidy()

### Export results
## Prep output for export
race.gender.fit.stats <- bind_rows(
  config = fit.cfa.fs.race.gender.stats,
  metric = fit.cfa.fs.race.gender.metric.stats,
  scalar = fit.cfa.fs.race.gender.scalar.stats,
  .id = "MI test"
)

race.gender.anova <- bind_rows(
  metric = race.gender.metric.invariance,
  scalar = race.gender.scalar.invariance,
  .id = "MI test"
)

write.xlsx(x = data.frame(race.gender.fit.stats),
           file = file.dir,
           sheetName = "Race-Gender MI Fit Stats",
           row.names = FALSE,
           append = TRUE)

write.xlsx(x = data.frame(race.gender.anova),
           file = file.dir,
           sheetName = "Race-Gender MI Chi-sq Tests",
           row.names = FALSE,
           append = TRUE)
