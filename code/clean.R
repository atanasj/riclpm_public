### ============================================================================
### SOURCE SCRIPTS
### ============================================================================
rm(list = ls(all = TRUE))
source("./code/func.R")
source("./code/load.R")
load("./data/proc/draft.RData")
ls(all.names = TRUE)

### ============================================================================
### LOAD DATA
### ============================================================================
.phd_fin <-
  readRDS("./data/proc/phd_sp_v2.rds")

## create 3st vars
phd_fin <-
  .phd_fin %>%
  .pro_mean("pa", pa_1:pa_13) %>%
  .pro_mean("scs_d", scs_1:scs_2) %>%
  .pro_mean("scs_a", scs_3:scs_4) %>%
  .pro_mean("scs_p", scs_5:scs_6) %>%
  .pro_mean("scs", scs_1:scs_6) %>%
  .pro_mean("isas_intra", num_range(
    "isas_8.",
    ## intrapersonal-influence
    c(
      1, 3, 5, 6, 11, 14, 16, 18,
      19, 24, 27, 29, 31, 32, 37
    )
  )) %>%
  ## isas social-factors
  .pro_mean("isas_social", num_range(
    "isas_8.",
    ## social-interpersonal
    c(
      2, 4, 7, 8, 9, 10, 12, 13, 15, 17,
      20, 21, 22, 23, 25, 26, 28, 30,
      33, 34, 35, 36, 38, 39
    )
  )) %>%
  ## TODO add isas_social and isas_intra factors
  group_by(ID) %>%
  filter(max(session) > 1) %>%
  mutate(
    ## make isas on same scale divide by N factors
    isas_intra = isas_intra / 5,
    isas_social = isas_social / 8,
    age = min(age),
    cov_sess = max(session), .after = session,
    hxsa = case_when(
      suicide_attempts <= 0 ~ 0,
      suicide_attempts >= 1 ~ 1,
      TRUE ~ NA_real_
    ),
    sex_mf = case_when(
      sex == 0 ~ 0,
      sex == 1 ~ 1,
      TRUE ~ NA_real_
    ),
    ses_int = diff(date) %>%
      median(na.rm = TRUE) %>%
      str_extract("[0-9]*") %>%
      as.numeric(),
    ses_mean =
      interval(min(date), max(date)) /
      duration(n = 1, units = "days") /
      cov_sess,
    planned_final = max(planned_final),
    ) %>%
  ungroup() %>%
  ## create centred ipt interaction vars
  group_by(session) %>%
  mutate(
    across(
      c(pb, tb, bhs),
      ~ .x - mean(.x, na.rm = TRUE),
      .names = "{.col}_c"
    ),
    ## create interaction term
    ptb = pb_c * tb_c,
    pbhs = pb_c * bhs_c,
    tbhs = tb_c * bhs_c,
    ptbhs = pb_c * tb_c * bhs_c,
  ) %>%
  ungroup() %>%
  ## replace missing session with 5 for ID with missing final session
  mutate(
    session = if_else(is.na(session), 5, session),
    ## centre to mitigate multicollinearity
    across(
      c(
        pb, tb, bhs, ac, dep, anx, str, si,
        pa, scs_d, scs_a, scs_p, scs,
        isas_intra, isas_social,
        age, sex_mf,
        cov_sess, ses_int, ses_mean
      ),
      ~ log10(1 + .x),
      .names = "{.col}_log"
    ),
    sex = case_when(
      sex %in% 0 ~ "female",
      sex %in% 1 ~ "male",
      sex %in% 2 ~ "other",
      TRUE ~ NA_character_
    ),
    atsi = case_when(
      ## correct atsi status from old sp_db
      sp_db %in% "old" & atsi %in% 4 ~ "neither",
      ## collapse categories as cell sizes too small
      atsi %in% 0                    ~ "neither",
      atsi %in% c(1, 2, 3)           ~ "ATSI",
      TRUE ~ NA_character_
    ),
    cald = case_when(
      cald %in% 0 ~ "no",
      cald %in% 1 ~ "yes",
      TRUE ~ NA_character_
    ),
    living = case_when(
      living %in% 1 ~ "family",
      living %in% 2 ~ "other",
      living %in% 3 ~ "other",
      living %in% 4 ~ "other",
      TRUE ~ NA_character_
    ),
    liv_fam = case_when(
      living %in% "family" ~ 1,
      living %in% "other"  ~ 0,
      TRUE ~ NA_real_
    ),
    relation = case_when(
      relation %in% 1 ~ "single/separated/divorced",
      relation %in% 2 ~ "relationship/married/defacto",
      relation %in% 3 ~ "relationship/married/defacto",
      relation %in% 4 ~ "single/separated/divorced",
      TRUE ~ NA_character_
    ),
    rel_stat = case_when(
      relation %in% "relationship/married/defacto" ~ 1,
      relation %in% "single/separated/divorced" ~ 0,
      TRUE ~ NA_real_
    ),
    income = case_when(
      income %in% 1 ~ "working",
      income %in% 2 ~ "working",
      income %in% 3 ~ "not-working",
      income %in% 4 ~ "not-working",
      income %in% 5 ~ "not-working",
      income %in% 6 ~ "student",
      TRUE ~ NA_character_
    ),
    inc_stat = case_when(
      income %in% "not-working" ~ 0,
      income %in% "student"     ~ 1,
      income %in% "working"     ~ 2,
      TRUE ~ NA_real_
    ),
    across(starts_with("diag"), ~ case_when(
      .x %in% 1  ~ "no diagnosis warranted",
      .x %in% 2  ~ "depression",
      .x %in% 3  ~ "generalised anxiety",
      .x %in% 4  ~ "phobia",
      .x %in% 5  ~ "adjustment disorder",
      .x %in% 6  ~ "acute stress disorder",
      .x %in% 7  ~ "ptsd",
      .x %in% 8  ~ "obsessive-compulsive",
      .x %in% 9  ~ "eating disorder",
      .x %in% 13 ~ "bipolar",
      .x %in% 15 ~ "personality disorder (e.g. BPD)",
      .x %in% 17 ~ "neurological disorder",
      .x %in% 18 ~ "other",
      TRUE ~ NA_character_
    )),
  )

### ============================================================================
### LONG TO WIDE
### ============================================================================

phd_fin_wide <-
  phd_fin %>%
  panelr::panel_data(
    id = ID,
    wave = session,
    ) %>%
  panelr::widen_panel(
    separator = "_",
    ignore.attributes = TRUE,
    varying = NULL
  )

### ============================================================================
### RELIABILITY
### ============================================================================

## STANDARD POTENTIALLY BIASED APPROACH
## -----------------------------------------------------------------------------

.pb_alpha <-
  phd_fin %>%
  filter(session < 6) %>%
  select(session, inq_1:inq_6)
.tb_alpha <-
  phd_fin %>%
  filter(session < 6) %>%
  select(session, inq_7r:inq_15r, inq_9, inq_11:inq_12)
.bhs_20 <-
  phd_fin %>%
  filter(session < 6) %>%
  select(session, starts_with("bhs_") & ends_with("r"))
.bhs_sf <-
  phd_fin %>%
  filter(session < 6) %>%
  select(session, starts_with("bhsSF_") & ends_with("lessone"))
.bhs <-
  phd_fin %>%
  filter(session < 6) %>%
  select(session, bhs)
.si_alpha <-
  phd_fin %>%
  filter(session < 6) %>%
  select(session, mssi_1:mssi_4, starts_with("rc_mssi"))
.ac_alpha <-
  phd_fin %>%
  filter(session == 1) %>%
  select(session, c(acss_8r:acss_13r, acss_7, acss_11, acss_14, acss_19))
.dep_alpha <-
  phd_fin %>%
  filter(session < 6) %>%
  select(session, num_range("dass_", c(3, 5, 10, 13, 16, 17, 21)))
.anx_alpha <-
  phd_fin %>%
  filter(session < 6) %>%
  select(session, num_range("dass_", c(2, 4, 7, 9, 15, 19, 20)))
.str_alpha <-
  phd_fin %>%
  filter(session < 6) %>%
  select(session, num_range("dass_", c(1, 6, 8, 11, 12, 14, 18)))
.isas_intra_alpha <-
  phd_fin %>%
  filter(session == 1) %>%
  select(
    session,
    num_range("isas_8.", c(
      1, 3, 5, 6, 11, 14, 16, 18,
      19, 24, 27, 29, 31, 32, 37
    ))
  )
.isas_social_alpha <-
  phd_fin %>%
  filter(session == 1) %>%
  select(
    session,
    num_range(
      "isas_8.",
      c(
        2, 4, 7, 8, 9, 10, 12, 13, 15, 17,
        20, 21, 22, 23, 25, 26, 28, 30,
        33, 34, 35, 36, 38, 39
      )
    )
  )

meas_rel_fin <-
  list(
    pb = .pb_alpha,
    tb = .tb_alpha,
    bhs_20 = .bhs_20,
    bhs_sf = .bhs_sf,
    si = .si_alpha,
    ac = .ac_alpha,
    dep = .dep_alpha,
    anx = .anx_alpha,
    str = .str_alpha,
    intrap = .isas_intra_alpha,
    interp = .isas_social_alpha
  ) %>%
  map(
    ~ .x %>%
      split(.$session) %>%
      map(
        ~ .x %>%
          select(-session) %>%
          as.data.frame() %>%
          psych::alpha()
      )
  ) %>%
  unlist(recursive = FALSE) %>%
  map(`[`, "total") %>%
  unlist(recursive = FALSE) %>%
  map(`[`, "std.alpha") %>%
  unlist() %>%
  as.data.frame() %>%
  rownames_to_column() %>%
  rename(val = 2) %>%
  mutate(
    rowname = str_remove(rowname, "\\.total\\.std\\.alpha"),
    var = str_extract(rowname, "pb|tb|bhs_20|bhs_sf|dep|si|ac|anx|str|intrap|interp"), # nolint
    session = str_extract(rowname, "\\d$"),
  ) %>%
  select(var, session, val) %>%
  group_by(var) %>%
  summarise(
    min = min(val),
    max = max(val)
  ) %>%
  mutate(
    across(
      c(min, max),
      ~ str_remove(sprintf("%.2f", round(.x, 2)), "^0")
    )
  )

meas_icc_fin <-
  list(
    pb = .pb_alpha,
    tb = .tb_alpha,
    bhs_20 = .bhs_20,
    bhs_sf = .bhs_sf,
    si = .si_alpha,
    ac = .ac_alpha,
    dep = .dep_alpha,
    intrap = .isas_intra_alpha,
    interp = .isas_social_alpha
  ) %>%
  map(
    ~ .x %>%
      ICC(missing = FALSE, lmer = TRUE)
  ) %>%
  map(
    `[`, "results"
  ) %>%
  map(
    ~ .x %>%
      as.data.frame(.) %>%
      clean_names() %>%
      as_tibble() %>%
      rename_with(
        ~ str_remove_all(.x, "results_"),
        everything()
      ) %>%
      filter(type %in% "ICC1") %>%
      ## filter(!str_detect(type, "k$")) %>%
      mutate(
        across(ends_with("bound"), ~ str_remove(sprintf("%.2f", .x), "^0")),
        iccp = str_remove(sprintf("%.2f", icc), "^0"),
        ci = paste0("[", lower_bound, ", ", upper_bound, "]"),
        bwp = paste0(sprintf("%.0f", 100 * round(icc, 2)), "%"),
        wip = paste0(sprintf("%.0f", 100 * (1 - round(icc, 2))), "%"),
        )
  )


## MCFA WITH LAVAAN
## -----------------------------------------------------------------------------
## make mcfa alpa dfs
.pb_alpha_2 <-
  phd_fin %>%
  filter(session < 6) %>%
  select(ID, inq_1:inq_6) %>%
  rename_with(~ str_replace(.x, "inq", "pb"))
.tb_alpha_2 <-
  phd_fin %>%
  filter(session < 6) %>%
  select(ID, inq_7r:inq_15r, inq_9, inq_11:inq_12) %>%
  rename_with(~ str_remove_all(.x, "r$")) %>%
  ## NOTE names of these vars need to start from 1 for the mcfa_alpha function
  select(ID, num_range("inq_", 7:15)) %>%
  rename_with(~ paste0("tb_", 1:9), starts_with("inq_"))
.bhs_20_2 <-
  phd_fin %>%
  filter(session < 6, protocol %in% "1") %>%
  select(ID, starts_with("bhs_") & ends_with("r")) %>%
  rename_with(~ str_remove_all(.x, "r$")) %>%
  ## NOTE names of these vars need to be ordered for the mcfa_alpha function
  select(ID, num_range("bhs_", 1:20))
.bhs_sf_2 <-
  phd_fin %>%
  filter(session < 6, protocol %in% "2") %>%
  select(ID, starts_with("bhsSF_") & ends_with("lessone")) %>%
  rename_with(~ paste0("bhsSF_", rep(1:4)), 2:5)
.si_alpha_2 <-
  phd_fin %>%
  filter(session < 6) %>%
  select(ID, mssi_1:mssi_4, starts_with("rc_mssi")) %>%
  rename_with(~ str_remove_all(.x, "^rc_")) %>%
  select(ID, num_range("mssi_", 1:18))
.dep_alpha_2 <-
  phd_fin %>%
  filter(session < 6) %>%
  select(ID, num_range("dass_", c(3, 5, 10, 13, 16, 17, 21))) %>%
  rename_with(~ paste0("dep_", rep(1:7)), 2:8)

.mcfa_alpha <-
  list(
    pb = mcfa_alpha(.pb_alpha_2, "ID"),
    tb = mcfa_alpha(.tb_alpha_2, "ID"),
    bhs_20 = mcfa_alpha(.bhs_20_2, "ID"),
    bhs_sf = mcfa_alpha(.bhs_sf_2, "ID"),
    si = mcfa_alpha(.si_alpha_2, "ID"),
    dep = mcfa_alpha(.dep_alpha_2, "ID")
  )

tt <-
  multilevel_alpha(.tb_alpha_2, "ID")

mcfa_alpha <-
  .mcfa_alpha %>%
  map(
    ~ .x %>%
      parameterEstimates() %>%
      as.data.frame() %>%
      filter(label %in% c("alpha2l", "alphab", "tilalpw")) %>%
      select(label, est, se, starts_with("ci")) %>%
      mutate(
        label = case_when(
          label %in% "alpha2l" ~ "overall",
          label %in% "alphab" ~ "between",
          label %in% "tilalpw" ~ "within"
        ),
        across(
          -label,
          ~ str_remove(sprintf("%.2f", .x), "^0")
        )
      )
  )

### ============================================================================
### LAVAAN MODELS
### ============================================================================

## CONSTRAINED MODEL: AUX SEMTOOLS
## -----------------------------------------------------------------------------
## NOTE constrained model here:
## The RI-CLPM addresses this limitation through distinguishing the
## between-person (time-invariant or trait-like) and within-person (time-variant
## or state-like) variance by creating one latent variable for each construct
## across the measurement timepoints, with factor loadings constrained to 1
## [@hamaker2015; @mulder2021].
## Constraining factor loadings in this way assumes that the random intercepts
## have the exact same influence on the observed variables at each occasion
## [@mulder2021].[^1]
## [^1]: Whether this assumption is tenable can be tested emprirically [cf.
##       @mulder2021]. In the current study, this assumption was tested, however
##       the model failed to converge.

riclpm_mod <- '
  ## Create between components (random intercepts)
  RIpb  =~ 1*pb_1 + 1*pb_2 + 1*pb_3 + 1*pb_4 + 1*pb_5
  RItb  =~ 1*tb_1 + 1*tb_2 + 1*tb_3 + 1*tb_4 + 1*tb_5
  RIbhs =~ 1*bhs_1 + 1*bhs_2 + 1*bhs_3 + 1*bhs_4 + 1*bhs_5
  RIdep =~ 1*dep_1 + 1*dep_2 + 1*dep_3 + 1*dep_4 + 1*dep_5
  RIsi  =~ 1*si_1 + 1*si_2 + 1*si_3 + 1*si_4 + 1*si_5
  ## =======
  ## Create within-person centered variables
  ## =======
  wpb_1 =~ 1*pb_1
  wpb_2 =~ 1*pb_2
  wpb_3 =~ 1*pb_3
  wpb_4 =~ 1*pb_4
  wpb_5 =~ 1*pb_5
  ## ==
  wtb_1 =~ 1*tb_1
  wtb_2 =~ 1*tb_2
  wtb_3 =~ 1*tb_3
  wtb_4 =~ 1*tb_4
  wtb_5 =~ 1*tb_5
  ## ==
  wbhs_1 =~ 1*bhs_1
  wbhs_2 =~ 1*bhs_2
  wbhs_3 =~ 1*bhs_3
  wbhs_4 =~ 1*bhs_4
  wbhs_5 =~ 1*bhs_5
  ## ==
  wdep_1 =~ 1*dep_1
  wdep_2 =~ 1*dep_2
  wdep_3 =~ 1*dep_3
  wdep_4 =~ 1*dep_4
  wdep_5 =~ 1*dep_5
  ## ==
  wsi_1 =~ 1*si_1
  wsi_2 =~ 1*si_2
  wsi_3 =~ 1*si_3
  wsi_4 =~ 1*si_4
  wsi_5 =~ 1*si_5
  ## =======
  ## Estimate the lagged effects between the within-person centered variables.
  ## =======
  wpb_2 + wtb_2 + wbhs_2 + wdep_2 + wsi_2 ~ wpb_1 + wtb_1 + wbhs_1 + wdep_1 + wsi_1 # nolint
  wpb_3 + wtb_3 + wbhs_3 + wdep_3 + wsi_3 ~ wpb_2 + wtb_2 + wbhs_2 + wdep_2 + wsi_2 # nolint
  wpb_4 + wtb_4 + wbhs_4 + wdep_4 + wsi_4 ~ wpb_3 + wtb_3 + wbhs_3 + wdep_3 + wsi_3 # nolint
  wpb_5 + wtb_5 + wbhs_5 + wdep_5 + wsi_5 ~ wpb_4 + wtb_4 + wbhs_4 + wdep_4 + wsi_4 # nolint
  ## =======
  ## Estimate the covariance between the within-person centered variables
  ## at the first wave.
  ## =======
  wpb_1  ~~ wtb_1 # Covariance
  wpb_1  ~~ wbhs_1
  wpb_1  ~~ wdep_1
  wpb_1  ~~ wsi_1
  wtb_1  ~~ wbhs_1
  wtb_1  ~~ wdep_1
  wtb_1  ~~ wsi_1
  wbhs_1 ~~ wsi_1
  wbhs_1 ~~ wdep_1
  wdep_1 ~~ wsi_1
  ## Estimate the covariances between the residuals of the within-person
  ## centered variables (the innovations).
  ## time_2
  wpb_2  ~~ wtb_2
  wpb_2  ~~ wbhs_2
  wpb_2  ~~ wdep_2
  wpb_2  ~~ wsi_2
  wtb_2  ~~ wbhs_2
  wtb_2  ~~ wdep_2
  wtb_2  ~~ wsi_2
  wbhs_2 ~~ wsi_2
  wbhs_2 ~~ wdep_2
  wdep_2 ~~ wsi_2
  ## time_3
  wpb_3  ~~ wtb_3
  wpb_3  ~~ wbhs_3
  wpb_3  ~~ wdep_3
  wpb_3  ~~ wsi_3
  wtb_3  ~~ wbhs_3
  wtb_3  ~~ wdep_3
  wtb_3  ~~ wsi_3
  wbhs_3 ~~ wsi_3
  wbhs_3 ~~ wdep_3
  wdep_3 ~~ wsi_3
  ## time_4
  wpb_4  ~~ wtb_4
  wpb_4  ~~ wbhs_4
  wpb_4  ~~ wdep_4
  wpb_4  ~~ wsi_4
  wtb_4  ~~ wbhs_4
  wtb_4  ~~ wdep_4
  wtb_4  ~~ wsi_4
  wbhs_4 ~~ wsi_4
  wbhs_4 ~~ wdep_4
  wdep_4 ~~ wsi_4
  ## time_5
  wpb_5  ~~ wtb_5
  wpb_5  ~~ wbhs_5
  wpb_5  ~~ wdep_5
  wpb_5  ~~ wsi_5
  wtb_5  ~~ wbhs_5
  wtb_5  ~~ wdep_5
  wtb_5  ~~ wsi_5
  wbhs_5 ~~ wsi_5
  wbhs_5 ~~ wdep_5
  wdep_5 ~~ wsi_5
  ## Estimate the variance and covariance of the random intercepts.
  RIpb  ~~ RIpb
  RItb  ~~ RItb
  RIbhs ~~ RIbhs
  RIdep ~~ RIdep
  RIsi  ~~ RIsi
  ## ==
  RIpb  ~~ RItb
  RIpb  ~~ RIbhs
  RIpb  ~~ RIdep
  RIpb  ~~ RIsi
  RItb  ~~ RIbhs
  RItb  ~~ RIdep
  RItb  ~~ RIsi
  RIbhs ~~ RIsi
  RIbhs ~~ RIdep
  RIdep ~~ RIsi
  ## Estimate the (residual) variance of the within-person centered variables.
  wpb_1  ~~ wpb_1 # Variances
  wtb_1  ~~ wtb_1
  wbhs_1 ~~ wbhs_1
  wdep_1 ~~ wdep_1
  wsi_1  ~~ wsi_1
  ## time_2
  wpb_2  ~~ wpb_2 # Residual variances
  wtb_2  ~~ wtb_2
  wbhs_2 ~~ wbhs_2
  wdep_2 ~~ wdep_2
  wsi_2  ~~ wsi_2
  ## time_3
  wpb_3  ~~ wpb_3 # Residual variances
  wtb_3  ~~ wtb_3
  wbhs_3 ~~ wbhs_3
  wdep_3 ~~ wdep_3
  wsi_3  ~~ wsi_3
  ## time_4
  wpb_4  ~~ wpb_4 # Residual variances
  wtb_4  ~~ wtb_4
  wbhs_4 ~~ wbhs_4
  wdep_4 ~~ wdep_4
  wsi_4  ~~ wsi_4
  ## time_5
  wpb_5  ~~ wpb_5 # Residual variances
  wtb_5  ~~ wtb_5
  wbhs_5 ~~ wbhs_5
  wdep_5 ~~ wdep_5
  wsi_5  ~~ wsi_5
'

aux_vars <-
  c(
    "cov_sess", "ses_mean_log",
    "ac_1", "isas_intra", "isas_social",
    "age", "sex_mf",
    "liv_fam", "pastmed", "pastpsyc",
    "hxdsh", "hxtrauma", "hxsa"
  )

## UNCONSTRATED MODEL TO TEST
## -----------------------------------------------------------------------------

riclpm_free_mod <- '
  ## Create between components (random intercepts)
  RIpb  =~ pb_1 + pb_2 + pb_3 + pb_4 + pb_5
  RItb  =~ tb_1 + tb_2 + tb_3 + tb_4 + tb_5
  RIbhs =~ bhs_1 + bhs_2 + bhs_3 + bhs_4 + bhs_5
  RIdep =~ dep_1 + dep_2 + dep_3 + dep_4 + dep_5
  RIsi  =~ si_1 + si_2 + si_3 + si_4 + si_5
  ## =======
  ## Create within-person centered variables
  ## =======
  wpb_1 =~ 1*pb_1
  wpb_2 =~ 1*pb_2
  wpb_3 =~ 1*pb_3
  wpb_4 =~ 1*pb_4
  wpb_5 =~ 1*pb_5
  ## ==
  wtb_1 =~ 1*tb_1
  wtb_2 =~ 1*tb_2
  wtb_3 =~ 1*tb_3
  wtb_4 =~ 1*tb_4
  wtb_5 =~ 1*tb_5
  ## ==
  wbhs_1 =~ 1*bhs_1
  wbhs_2 =~ 1*bhs_2
  wbhs_3 =~ 1*bhs_3
  wbhs_4 =~ 1*bhs_4
  wbhs_5 =~ 1*bhs_5
  ## ==
  wdep_1 =~ 1*dep_1
  wdep_2 =~ 1*dep_2
  wdep_3 =~ 1*dep_3
  wdep_4 =~ 1*dep_4
  wdep_5 =~ 1*dep_5
  ## ==
  wsi_1 =~ 1*si_1
  wsi_2 =~ 1*si_2
  wsi_3 =~ 1*si_3
  wsi_4 =~ 1*si_4
  wsi_5 =~ 1*si_5
  ## =======
  ## Estimate the lagged effects between the within-person centered variables.
  ## =======
  wpb_2 + wtb_2 + wbhs_2 + wdep_2 + wsi_2 ~ wpb_1 + wtb_1 + wbhs_1 + wdep_1 + wsi_1 # nolint
  wpb_3 + wtb_3 + wbhs_3 + wdep_3 + wsi_3 ~ wpb_2 + wtb_2 + wbhs_2 + wdep_2 + wsi_2 # nolint
  wpb_4 + wtb_4 + wbhs_4 + wdep_4 + wsi_4 ~ wpb_3 + wtb_3 + wbhs_3 + wdep_3 + wsi_3 # nolint
  wpb_5 + wtb_5 + wbhs_5 + wdep_5 + wsi_5 ~ wpb_4 + wtb_4 + wbhs_4 + wdep_4 + wsi_4 # nolint
  ## =======
  ## Estimate the covariance between the within-person centered variables
  ## at the first wave.
  ## =======
  wpb_1  ~~ wtb_1 # Covariance
  wpb_1  ~~ wbhs_1
  wpb_1  ~~ wdep_1
  wpb_1  ~~ wsi_1
  wtb_1  ~~ wbhs_1
  wtb_1  ~~ wdep_1
  wtb_1  ~~ wsi_1
  wbhs_1 ~~ wsi_1
  wbhs_1 ~~ wdep_1
  wdep_1 ~~ wsi_1
  ## Estimate the covariances between the residuals of the within-person
  ## centered variables (the innovations).
  ## time_2
  wpb_2  ~~ wtb_2
  wpb_2  ~~ wbhs_2
  wpb_2  ~~ wdep_2
  wpb_2  ~~ wsi_2
  wtb_2  ~~ wbhs_2
  wtb_2  ~~ wdep_2
  wtb_2  ~~ wsi_2
  wbhs_2 ~~ wsi_2
  wbhs_2 ~~ wdep_2
  wdep_2 ~~ wsi_2
  ## time_3
  wpb_3  ~~ wtb_3
  wpb_3  ~~ wbhs_3
  wpb_3  ~~ wdep_3
  wpb_3  ~~ wsi_3
  wtb_3  ~~ wbhs_3
  wtb_3  ~~ wdep_3
  wtb_3  ~~ wsi_3
  wbhs_3 ~~ wsi_3
  wbhs_3 ~~ wdep_3
  wdep_3 ~~ wsi_3
  ## time_4
  wpb_4  ~~ wtb_4
  wpb_4  ~~ wbhs_4
  wpb_4  ~~ wdep_4
  wpb_4  ~~ wsi_4
  wtb_4  ~~ wbhs_4
  wtb_4  ~~ wdep_4
  wtb_4  ~~ wsi_4
  wbhs_4 ~~ wsi_4
  wbhs_4 ~~ wdep_4
  wdep_4 ~~ wsi_4
  ## time_5
  wpb_5  ~~ wtb_5
  wpb_5  ~~ wbhs_5
  wpb_5  ~~ wdep_5
  wpb_5  ~~ wsi_5
  wtb_5  ~~ wbhs_5
  wtb_5  ~~ wdep_5
  wtb_5  ~~ wsi_5
  wbhs_5 ~~ wsi_5
  wbhs_5 ~~ wdep_5
  wdep_5 ~~ wsi_5
  ## Estimate the variance and covariance of the random intercepts.
  RIpb  ~~ RIpb
  RItb  ~~ RItb
  RIbhs ~~ RIbhs
  RIdep ~~ RIdep
  RIsi  ~~ RIsi
  ## ==
  RIpb  ~~ RItb
  RIpb  ~~ RIbhs
  RIpb  ~~ RIdep
  RIpb  ~~ RIsi
  RItb  ~~ RIbhs
  RItb  ~~ RIdep
  RItb  ~~ RIsi
  RIbhs ~~ RIsi
  RIbhs ~~ RIdep
  RIdep ~~ RIsi
  ## Estimate the (residual) variance of the within-person centered variables.
  wpb_1  ~~ wpb_1 # Variances
  wtb_1  ~~ wtb_1
  wbhs_1 ~~ wbhs_1
  wdep_1 ~~ wdep_1
  wsi_1  ~~ wsi_1
  ## time_2
  wpb_2  ~~ wpb_2 # Residual variances
  wtb_2  ~~ wtb_2
  wbhs_2 ~~ wbhs_2
  wdep_2 ~~ wdep_2
  wsi_2  ~~ wsi_2
  ## time_3
  wpb_3  ~~ wpb_3 # Residual variances
  wtb_3  ~~ wtb_3
  wbhs_3 ~~ wbhs_3
  wdep_3 ~~ wdep_3
  wsi_3  ~~ wsi_3
  ## time_4
  wpb_4  ~~ wpb_4 # Residual variances
  wtb_4  ~~ wtb_4
  wbhs_4 ~~ wbhs_4
  wdep_4 ~~ wdep_4
  wsi_4  ~~ wsi_4
  ## time_5
  wpb_5  ~~ wpb_5 # Residual variances
  wtb_5  ~~ wtb_5
  wbhs_5 ~~ wbhs_5
  wdep_5 ~~ wdep_5
  wsi_5  ~~ wsi_5
'

## run without aux
## NOTE this failed to converge
riclpm_free_fit_na <-
  lavaan(
    riclpm_free_mod,
    data = phd_fin_wide,
    missing = "ML",
    estimator = "MLR",
    meanstructure = TRUE,
    int.ov.free = TRUE,
    ## bounds = TRUE,
    fixed.x = FALSE
  )

## run with aux
riclpm_free_fit <-
  lavaan.auxiliary(
    riclpm_free_mod,
    aux = aux_vars,
    data = phd_fin_wide,
    missing = "ML",
    estimator = "MLR",
    meanstructure = TRUE,
    int.ov.free = TRUE,
    ## bounds = TRUE,
    fixed.x = FALSE
  )


## NOTE bring aux vars into mode as per advice below
## https://groups.google.com/g/lavaan/c/kr0JWacZLT8/m/rfGquwzSCgAJ
## check
riclpm_mod_auxin <- '
  ## Create between components (random intercepts)
  RIpb  =~ 1*pb_1 + 1*pb_2 + 1*pb_3 + 1*pb_4 + 1*pb_5
  RItb  =~ 1*tb_1 + 1*tb_2 + 1*tb_3 + 1*tb_4 + 1*tb_5
  RIbhs =~ 1*bhs_1 + 1*bhs_2 + 1*bhs_3 + 1*bhs_4 + 1*bhs_5
  RIdep =~ 1*dep_1 + 1*dep_2 + 1*dep_3 + 1*dep_4 + 1*dep_5
  RIsi  =~ 1*si_1 + 1*si_2 + 1*si_3 + 1*si_4 + 1*si_5
  ## =======
  ## Create within-person centered variables
  ## =======
  wpb_1 =~ 1*pb_1
  wpb_2 =~ 1*pb_2
  wpb_3 =~ 1*pb_3
  wpb_4 =~ 1*pb_4
  wpb_5 =~ 1*pb_5
  ## ==
  wtb_1 =~ 1*tb_1
  wtb_2 =~ 1*tb_2
  wtb_3 =~ 1*tb_3
  wtb_4 =~ 1*tb_4
  wtb_5 =~ 1*tb_5
  ## ==
  wbhs_1 =~ 1*bhs_1
  wbhs_2 =~ 1*bhs_2
  wbhs_3 =~ 1*bhs_3
  wbhs_4 =~ 1*bhs_4
  wbhs_5 =~ 1*bhs_5
  ## ==
  wdep_1 =~ 1*dep_1
  wdep_2 =~ 1*dep_2
  wdep_3 =~ 1*dep_3
  wdep_4 =~ 1*dep_4
  wdep_5 =~ 1*dep_5
  ## ==
  wsi_1 =~ 1*si_1
  wsi_2 =~ 1*si_2
  wsi_3 =~ 1*si_3
  wsi_4 =~ 1*si_4
  wsi_5 =~ 1*si_5
  ## =======
  ## Estimate the lagged effects between the within-person centered variables.
  ## =======
  wpb_2 + wtb_2 + wbhs_2 + wdep_2 + wsi_2 ~ wpb_1 + wtb_1 + wbhs_1 + wdep_1 + wsi_1 # nolint
  wpb_3 + wtb_3 + wbhs_3 + wdep_3 + wsi_3 ~ wpb_2 + wtb_2 + wbhs_2 + wdep_2 + wsi_2 # nolint
  wpb_4 + wtb_4 + wbhs_4 + wdep_4 + wsi_4 ~ wpb_3 + wtb_3 + wbhs_3 + wdep_3 + wsi_3 # nolint
  wpb_5 + wtb_5 + wbhs_5 + wdep_5 + wsi_5 ~ wpb_4 + wtb_4 + wbhs_4 + wdep_4 + wsi_4 # nolint
  ## =======
  ## Estimate the covariance between the within-person centered variables
  ## at the first wave.
  ## =======
  wpb_1  ~~ wtb_1 # Covariance
  wpb_1  ~~ wbhs_1
  wpb_1  ~~ wdep_1
  wpb_1  ~~ wsi_1
  wtb_1  ~~ wbhs_1
  wtb_1  ~~ wdep_1
  wtb_1  ~~ wsi_1
  wbhs_1 ~~ wsi_1
  wbhs_1 ~~ wdep_1
  wdep_1 ~~ wsi_1
  ## Estimate the covariances between the residuals of the within-person
  ## centered variables (the innovations).
  ## time_2
  wpb_2  ~~ wtb_2
  wpb_2  ~~ wbhs_2
  wpb_2  ~~ wdep_2
  wpb_2  ~~ wsi_2
  wtb_2  ~~ wbhs_2
  wtb_2  ~~ wdep_2
  wtb_2  ~~ wsi_2
  wbhs_2 ~~ wsi_2
  wbhs_2 ~~ wdep_2
  wdep_2 ~~ wsi_2
  ## time_3
  wpb_3  ~~ wtb_3
  wpb_3  ~~ wbhs_3
  wpb_3  ~~ wdep_3
  wpb_3  ~~ wsi_3
  wtb_3  ~~ wbhs_3
  wtb_3  ~~ wdep_3
  wtb_3  ~~ wsi_3
  wbhs_3 ~~ wsi_3
  wbhs_3 ~~ wdep_3
  wdep_3 ~~ wsi_3
  ## time_4
  wpb_4  ~~ wtb_4
  wpb_4  ~~ wbhs_4
  wpb_4  ~~ wdep_4
  wpb_4  ~~ wsi_4
  wtb_4  ~~ wbhs_4
  wtb_4  ~~ wdep_4
  wtb_4  ~~ wsi_4
  wbhs_4 ~~ wsi_4
  wbhs_4 ~~ wdep_4
  wdep_4 ~~ wsi_4
  ## time_5
  wpb_5  ~~ wtb_5
  wpb_5  ~~ wbhs_5
  wpb_5  ~~ wdep_5
  wpb_5  ~~ wsi_5
  wtb_5  ~~ wbhs_5
  wtb_5  ~~ wdep_5
  wtb_5  ~~ wsi_5
  wbhs_5 ~~ wsi_5
  wbhs_5 ~~ wdep_5
  wdep_5 ~~ wsi_5
  ## Estimate the variance and covariance of the random intercepts.
  RIpb  ~~ RIpb
  RItb  ~~ RItb
  RIbhs ~~ RIbhs
  RIdep ~~ RIdep
  RIsi  ~~ RIsi
  ## ==
  RIpb  ~~ RItb
  RIpb  ~~ RIbhs
  RIpb  ~~ RIdep
  RIpb  ~~ RIsi
  RItb  ~~ RIbhs
  RItb  ~~ RIdep
  RItb  ~~ RIsi
  RIbhs ~~ RIsi
  RIbhs ~~ RIdep
  RIdep ~~ RIsi
  ## Estimate the (residual) variance of the within-person centered variables.
  wpb_1  ~~ wpb_1 # Variances
  wtb_1  ~~ wtb_1
  wbhs_1 ~~ wbhs_1
  wdep_1 ~~ wdep_1
  wsi_1  ~~ wsi_1
  ## time_2
  wpb_2  ~~ wpb_2 # Residual variances
  wtb_2  ~~ wtb_2
  wbhs_2 ~~ wbhs_2
  wdep_2 ~~ wdep_2
  wsi_2  ~~ wsi_2
  ## time_3
  wpb_3  ~~ wpb_3 # Residual variances
  wtb_3  ~~ wtb_3
  wbhs_3 ~~ wbhs_3
  wdep_3 ~~ wdep_3
  wsi_3  ~~ wsi_3
  ## time_4
  wpb_4  ~~ wpb_4 # Residual variances
  wtb_4  ~~ wtb_4
  wbhs_4 ~~ wbhs_4
  wdep_4 ~~ wdep_4
  wsi_4  ~~ wsi_4
  ## time_5
  wpb_5  ~~ wpb_5 # Residual variances
  wtb_5  ~~ wtb_5
  wbhs_5 ~~ wbhs_5
  wdep_5 ~~ wdep_5
  wsi_5  ~~ wsi_5
  ## ==========
  ## cov variances
  ## ==========
  cov_sess ~~ cov_sess
  ses_mean_log ~~ ses_mean_log
  ac_1 ~~ ac_1
  isas_intra ~~ isas_intra
  isas_social ~~ isas_social
  age ~~ age
  sex_mf ~~ sex_mf
  liv_fam ~~ liv_fam
  pastmed ~~ pastmed
  pastpsyc ~~ pastpsyc
  hxdsh ~~ hxdsh
  hxtrauma ~~ hxtrauma
  hxsa ~~ hxsa
  ## ==========
  ## cov covariances
  ## ==========
  cov_sess ~~ ac_1 + isas_intra + isas_social + age + sex_mf + liv_fam + pastmed + pastpsyc + hxdsh + hxtrauma + hxsa + ses_mean_log # nolint
  ac_1 ~~ isas_intra + isas_social + age + sex_mf + liv_fam + pastmed + pastpsyc + hxdsh + hxtrauma + hxsa + ses_mean_log # nolint
  isas_intra ~~ isas_social + age + sex_mf + liv_fam + pastmed + pastpsyc + hxdsh + hxtrauma + hxsa + ses_mean_log # nolint
  isas_social ~~ age + sex_mf + liv_fam + pastmed + pastpsyc + hxdsh + hxtrauma + hxsa + ses_mean_log # nolint
  age ~~ sex_mf + liv_fam + pastmed + pastpsyc + hxdsh + hxtrauma + hxsa + ses_mean_log # nolint
  sex_mf ~~ liv_fam + pastmed + pastpsyc + hxdsh + hxtrauma + hxsa + ses_mean_log # nolint
  liv_fam ~~ pastmed + pastpsyc + hxdsh + hxtrauma + hxsa + ses_mean_log # nolint
  pastmed ~~ pastpsyc + hxdsh + hxtrauma + hxsa + ses_mean_log # nolint
  pastpsyc ~~ hxdsh + hxtrauma + hxsa + ses_mean_log # nolint
  hxdsh ~~ hxtrauma + hxsa + ses_mean_log # nolint
  hxtrauma ~~ hxsa + ses_mean_log # nolint
  hxsa ~~ ses_mean_log # nolint
  ## ==========
  ## cov covs
  ## ==========
  wpb_1 + wpb_2 + wpb_3 + wpb_4 + wpb_5 ~~ cov_sess + ac_1 + isas_intra + isas_social + age + sex_mf + liv_fam + pastmed + pastpsyc + hxdsh + hxtrauma + hxsa + ses_mean_log # nolint
  wtb_1 + wtb_2 + wtb_3 + wtb_4 + wtb_5 ~~ cov_sess + ac_1 + isas_intra + isas_social + age + sex_mf + liv_fam + pastmed + pastpsyc + hxdsh + hxtrauma + hxsa + ses_mean_log # nolint
  wbhs_1 + wbhs_2 + wbhs_3 + wbhs_4 + wbhs_5 ~~ cov_sess + ac_1 + isas_intra + isas_social + age + sex_mf + liv_fam + pastmed + pastpsyc + hxdsh + hxtrauma + hxsa + ses_mean_log # nolint
  wdep_1 + wdep_2 + wdep_3 + wdep_4 + wdep_5 ~~ cov_sess + ac_1 + isas_intra + isas_social + age + sex_mf + liv_fam + pastmed + pastpsyc + hxdsh + hxtrauma + hxsa + ses_mean_log # nolint
  wsi_1 + wsi_2 + wsi_3 + wsi_4 + wsi_5 ~~ cov_sess + ac_1 + isas_intra + isas_social + age + sex_mf + liv_fam + pastmed + pastpsyc + hxdsh + hxtrauma + hxsa + ses_mean_log # nolint
'


## run with aux in model
riclpm_fit_auxin <-
  lavaan(
    riclpm_mod_auxin,
    data = phd_fin_wide,
    missing = "ML",
    estimator = "MLR",
    meanstructure = TRUE,
    int.ov.free = TRUE,
    ## bounds = TRUE,
    fixed.x = FALSE
  )


## no warnings
summary(
  riclpm_fit_auxin,
  ## fit.measures = TRUE,
  ci = TRUE
)

riclpm_fit_na <-
  lavaan(
    riclpm_mod,
    data = phd_fin_wide,
    missing = "ML",
    estimator = "MLR",
    meanstructure = TRUE,
    int.ov.free = TRUE,
    ## bounds = TRUE,
    fixed.x = FALSE
  )

lavInspect(riclpm_fit_na, "cov.lv")
lavInspect(riclpm_fit_na, "est")$psi

any(lavInspect(riclpm_fit_na, "cor.lv") > 1)  # marginal
any(lavInspect(riclpm_fit_na, "est")$psi > 1) # residual
## https://groups.google.com/g/lavaan/c/AaH-AhmOTeo/m/EcKumNOURmEJ
eigen(inspect(riclpm_fit_na, "cov.lv"))$values
eigen(inspect(riclpm_fit, "cov.lv"))$values

## run sat cor with aux
riclpm_fit <-
  lavaan.auxiliary(
    riclpm_mod,
    aux = aux_vars,
    data = phd_fin_wide,
    missing = "ML",
    estimator = "MLR",
    meanstructure = TRUE,
    int.ov.free = TRUE,
    ## bounds = TRUE,
    fixed.x = FALSE
  )

## NOTE inspection of covariance matrix of the residuals of the observed
## variables (theta) is not positive definite;
## https://groups.google.com/g/lavaan/c/kr0JWacZLT8/m/rfGquwzSCgAJ
.insp_theta_fit <-
  lavInspect(riclpm_fit, "theta") %>%
  as.data.frame()

## run sat cor with aux
riclpm_fit_hisi <-
  lavaan.auxiliary(
    riclpm_mod,
    aux = aux_vars,
    data = phd_fin_wide[phd_fin_wide$si_1 > 8, ],
    missing = "ML",
    estimator = "MLR",
    meanstructure = TRUE,
    int.ov.free = TRUE,
    ## bounds = TRUE,
    fixed.x = FALSE
  )

## https://psu-psychology.github.io/psy-597-sem-sp2019/09_model_comparison/model_comparison.html # nolint
## In the LRT approach, we can compare two nested models (using the anova
## function in lavaan) to test the null hypothesis that one model (with fewer
## parameters – the nested model) fit the data equally well. If we reject this
## null (typically p<.05), then we conclude that the nested model fits
## significantly worse than the full model.
## modindices(riclpm_fit) %>% filter(mi > 2) %>% arrange(desc(mi)) %>% head(n = 20)              # nolint

## NOTE despite NPD warning of latent variable, all CIs cross zero
summary(riclpm_fit_na, ci = TRUE)

lavInspect(riclpm_fit_na, "post.check")
lavInspect(riclpm_fit_na, "est")$psi # actual estimates
## You could check the correlation matrix for values > 1
any(lavInspect(riclpm_fit_na, "cor.lv") > 1)  # marginal, delivers FALSE
any(lavInspect(riclpm_fit_na, "est")$psi > 1) # residual, delivers TRUE


summary(riclpm_fit, ci = TRUE)

lavCor(riclpm_fit, "vcov")

lavInspect(riclpm_fit, "theta")

standardizedSolution(riclpm_fit)

fitMeasures(riclpm_fit, fit.measures = c("cfi", "tli"))
lavInspect(riclpm_fit@external$baseline.model, "fit")

fitMeasures(riclpm_fit@external$baseline.model, fit.measures = c("cfi", "tli"))
summary(riclpm_fit@external$baseline.model, fit.measures = TRUE)

anova(
  riclpm_fit_na,
  riclpm_fit
)

capture.output(
  summary(
    riclpm_fit_na,
    fit.measures = TRUE,
    standardized = TRUE,
    ci = TRUE
  ),
  file = "./output/draft/riclmp_fit_na.txt"
)

capture.output(
  summary(
    riclpm_fit,
    fit.measures = TRUE,
    standardized = TRUE,
    ci = TRUE
  ),
  file = "./output/draft/riclmp_fit_ses_freq.txt"
)

capture.output(
  summary(
    riclpm_fit_auxin,
    fit.measures = TRUE,
    standardized = TRUE,
    ci = TRUE
  ),
  file = "./output/draft/riclmp_fit_auxin_ses_freq.txt"
)

## https://groups.google.com/g/lavaan/c/ASycfFKS56g/m/6_2dxt7oAQAJ
lavInspect(riclpm_fit, "optim.gradient")
lavInspect(riclpm_fit, "options")$optim.dx.tol
lavInspect(riclpm_fit, "fit")
lavInspect(riclpm_fit, "dx.all")

lavInspect(riclpm_fit_na, "cov.lv")

lavInspect(riclpm_fit, "optim.gradient") %>%
  as.data.frame() %>%
  rownames_to_column(var = "rowname") %>%
  rename(value = 2) %>%
  str()

## https://www.statistical-thinking.com/posts/2018-11-14-fiml-in-lavaan-descriptive-statistics/ # nolint
.tt <-
  inspect(
    riclpm_fit,
    "patterns"
  ) %>%
  as.data.frame() %>%
  mutate(
    across(everything(), ~ .x * 1L),
    across(everything(), ~ if_else(.x == 1, 0, 1))
  ) %>%
  adorn_totals(c("row", "col"))

inspect(
  riclpm_fit,
  "coverage"
)

## CONTRAINED MODEL: ALL
## -----------------------------------------------------------------------------
clpm_mod <- '
  ## Estimate the lagged effects between the within-person centered variables.
  pb_2 + tb_2 + bhs_2 + dep_2 + si_2 ~ pb_1 + tb_1 + bhs_1 + dep_1 + si_1 # nolint
  pb_3 + tb_3 + bhs_3 + dep_3 + si_3 ~ pb_2 + tb_2 + bhs_2 + dep_2 + si_2 # nolint
  pb_4 + tb_4 + bhs_4 + dep_4 + si_4 ~ pb_3 + tb_3 + bhs_3 + dep_3 + si_3 # nolint
  pb_5 + tb_5 + bhs_5 + dep_5 + si_5 ~ pb_4 + tb_4 + bhs_4 + dep_4 + si_4 # nolint
  ## =======
  ## Estimate the covariance between the observed variable
  ## at the first wave.
  pb_1  ~~ tb_1 # Covariance
  pb_1  ~~ bhs_1
  pb_1  ~~ dep_1
  pb_1  ~~ si_1
  tb_1  ~~ bhs_1
  tb_1  ~~ dep_1
  tb_1  ~~ si_1
  bhs_1 ~~ si_1
  bhs_1 ~~ dep_1
  dep_1 ~~ si_1
  ## Estimate the covariances between the residuals of the observed
  ## variables (the innovations).
  ## time_2
  pb_2  ~~ tb_2
  pb_2  ~~ bhs_2
  pb_2  ~~ dep_2
  pb_2  ~~ si_2
  tb_2  ~~ bhs_2
  tb_2  ~~ dep_2
  tb_2  ~~ si_2
  bhs_2 ~~ si_2
  bhs_2 ~~ dep_2
  dep_2 ~~ si_2
  ## time_3
  pb_3  ~~ tb_3
  pb_3  ~~ bhs_3
  pb_3  ~~ dep_3
  pb_3  ~~ si_3
  tb_3  ~~ bhs_3
  tb_3  ~~ dep_3
  tb_3  ~~ si_3
  bhs_3 ~~ si_3
  bhs_3 ~~ dep_3
  dep_3 ~~ si_3
  ## time_4
  pb_4  ~~ tb_4
  pb_4  ~~ bhs_4
  pb_4  ~~ dep_4
  pb_4  ~~ si_4
  tb_4  ~~ bhs_4
  tb_4  ~~ dep_4
  tb_4  ~~ si_4
  bhs_4 ~~ si_4
  bhs_4 ~~ dep_4
  dep_4 ~~ si_4
  ## time_5
  pb_5  ~~ tb_5
  pb_5  ~~ bhs_5
  pb_5  ~~ dep_5
  pb_5  ~~ si_5
  tb_5  ~~ bhs_5
  tb_5  ~~ dep_5
  tb_5  ~~ si_5
  bhs_5 ~~ si_5
  bhs_5 ~~ dep_5
  dep_5 ~~ si_5
  ## Estimate the (residual) variance of the ithin-person centered variables.
  pb_1  ~~ pb_1 # Variances
  tb_1  ~~ tb_1
  bhs_1 ~~ bhs_1
  dep_1 ~~ dep_1
  si_1  ~~ si_1
  ## time_2
  pb_2  ~~ pb_2 # Residual variances
  tb_2  ~~ tb_2
  bhs_2 ~~ bhs_2
  dep_2 ~~ dep_2
  si_2  ~~ si_2
  ## time_3
  pb_3  ~~ pb_3 # Residual variances
  tb_3  ~~ tb_3
  bhs_3 ~~ bhs_3
  dep_3 ~~ dep_3
  si_3  ~~ si_3
  ## time_4
  pb_4  ~~ pb_4 # Residual variances
  tb_4  ~~ tb_4
  bhs_4 ~~ bhs_4
  dep_4 ~~ dep_4
  si_4  ~~ si_4
  ## time_5
  pb_5  ~~ pb_5 # Residual variances
  tb_5  ~~ tb_5
  bhs_5 ~~ bhs_5
  dep_5 ~~ dep_5
  si_5  ~~ si_5
'

clpm_fit_na <-
  lavaan(
    clpm_mod,
    data = phd_fin_wide,
    missing = "ML",
    estimator = "MLR",
    meanstructure = TRUE,
    int.ov.free = TRUE,
    ## bounds = TRUE,
    fixed.x = FALSE
  )


clpm_fit <-
  lavaan.auxiliary(
    clpm_mod,
    aux = aux_vars,
    data = phd_fin_wide,
    missing = "ML",
    estimator = "MLR",
    meanstructure = TRUE,
    int.ov.free = TRUE,
    ## bounds = TRUE,
    fixed.x = FALSE
  )

summary(
  clpm_fit_na,
  fit.measures = TRUE
)

summary(
  clpm_fit,
  fit.measures = TRUE
)

## If the chi-square difference test indicates that this constraint cannot be
## imposed, this implies that (some of) the lagged coefficients differ across
## the groups: The lagged effects of the variables on each other depend on the
## level of the grouping variable. In contrast, when the equality constraints on
## the lagged parameters across the groups hold, this implies there is no
## moderation effect significan difference, meaning contraints goes up, so drop
## which implies that imposing the constraints is tenable: The lagged effects
## for individuals with different levels of neuroticism appear to be the same.

## COMPARE RICLPM AND CLPM
## =============================================================================
mod_diff <-
  anova(
    clpm_fit,
    riclpm_fit
  ) %>%
  tidy() %>%
  clean_names() %>%
  mutate(
    across(
      where(is.double) & -p_value,
      ~ sprintf("%.2f", round(.x, 2))
    ),
    p_value = if_else(
      p_value < 0.001,
      "< 0.001",
      paste0(
        "=",
        round(p_value, 3),
        sep = " "
      )
    )
  )

anova(
  ## clpm_fit_na,
  ## riclpm_fit_na,
  clpm_fit,
  riclpm_fit
)

### ============================================================================
### REPORTING
### ============================================================================

## DESCRIPTIVES
## =============================================================================
riclpm_des_t15 <-
  phd_fin %>%
  filter(session < 6) %>%
  split(.$session) %>%
  map(
    ~.x %>%
      select(
        ID,
        pb, tb, bhs, dep, si,
        ) %>%
      pivot_longer(
        -ID
      ) %>%
      group_by(name) %>%
      summarise(
        across(
          value,
          list(
            mean = ~ mean(.x, na.rm = TRUE),
            sd = ~ sd(.x, na.rm = TRUE),
            range = ~ paste0(range(.x, na.rm = TRUE), collapse = "–"),
            N = ~ sum(!is.na(.x)),
            miss = ~ round((
              n_distinct(phd_fin_wide$ID) -
                sum(!is.na(.x))) /
                n_distinct(phd_fin_wide$ID) * 100, 1)
          ),
          .names = "{.col}_{.fn}"
        )
      ) %>%
      mutate(
        order = case_when(
          name == "pb"  ~ 1,
          name == "tb"  ~ 2,
          name == "bhs" ~ 3,
          name == "dep" ~ 4,
          name == "si"  ~ 5
        ), .before = name
      ) %>%
      rename_with(~ str_remove(.x, "value_"), starts_with("value")) %>%
      mutate(
        across(c(mean, sd), ~ sprintf("%.2f", .x)),
        m_sd = paste0(mean, "(", sd, ")"), .before = N,
        name = toupper(name)
      )
  ) %>%
  bind_rows(.id = "session") %>%
  arrange(session, order) %>%
  pivot_wider(
    -c(mean, sd, order),
    names_from = session,
    values_from = c(m_sd, range, N, miss)
  ) %>%
  select(name, ends_with(as.character(1:5)))

.riclpm_des_t1 <-
  phd_fin %>%
  filter(session == 1) %>%
  select(
    ID, age, ac, isas_social, isas_intra,
    ses_mean, cov_sess
  ) %>%
  mutate(
    across(
      starts_with("isas_"), ~ as.numeric(sprintf("%.2f", .x)))
  ) %>%
  pivot_longer(
    -ID
  ) %>%
  group_by(name) %>%
  summarise(
    across(
      value,
      list(
        mean = ~ mean(.x, na.rm = TRUE),
        sd = ~ sd(.x, na.rm = TRUE),
        range = ~ paste0(range(.x, na.rm = TRUE), collapse = "–"),
        N = ~ sum(!is.na(.x)),
        miss = ~ round((
          n_distinct(phd_fin_wide$ID) -
            sum(!is.na(.x))) /
          n_distinct(phd_fin_wide$ID) * 100, 1)
      ),
      .names = "{.col}_{.fn}"
    ),
  ) %>%
  mutate(
    order = case_when(
      name == "age"         ~ 1,
      name == "ac"          ~ 2,
      name == "isas_social" ~ 3,
      name == "isas_intra"  ~ 4,
      name == "ses_mean"    ~ 5,
      name == "cov_sess"    ~ 6
    ),
    session = 0
  ) %>%
  rename_with(~ str_remove(.x, "value_"), starts_with("value_")) %>%
  mutate(
    across(c(mean, sd), ~ sprintf("%.2f", .x)),
    m_sd = paste0(mean, "(", sd, ")"), .before = range
  ) %>%
  select(session, order, everything()) %>%
  arrange(session, order)

.riclpm_des_t1

riclpm_des_t1 <-
  phd_fin %>%
  filter(session == 1) %>%
  select(
    ID, sex_mf,
    hxdsh, hxtrauma, hxsa,
    liv_fam, rel_stat,
    pastmed, pastpsyc
  ) %>%
  pivot_longer(
    -ID
  ) %>%
  group_by(name, value) %>%
  summarise(
    n = n()
  ) %>%
  ungroup() %>%
  pivot_wider(
    names_from = value,
    values_from = n
  ) %>%
  clean_names() %>%
  mutate(
    session = 1,
    order = 0,
    mean = case_when(
      name %in% "sex_mf" ~ x0,
      TRUE ~ x1
    ),
    miss = round(na / n_distinct(phd_fin_wide$ID) * 100, 1),
    range = NA_character_,
    sd = round(mean / n_distinct(phd_fin_wide$ID) * 100, 1),
    m_sd = paste0(mean, "(", sd, ")"), .before = range,
    across(c(mean, sd), ~ as.character(.x)),
    N = n_distinct(phd_fin_wide$ID) - na
  ) %>%
  select(session, order, name, mean, sd, m_sd, range, N, miss) %>%
  bind_rows(.riclpm_des_t1) %>%
  ## select(session, order, name, mean, sd, m_sd, range, N, miss) %>%
  mutate(
    order = case_when(
      name %in% "sex_mf"      ~ 1,
      name %in% "age"         ~ 2,
      name %in% "ac"          ~ 3,
      name %in% "hxsa"        ~ 4,
      name %in% "hxtrauma"    ~ 5,
      name %in% "hxdsh"       ~ 6,
      name %in% "isas_social" ~ 7,
      name %in% "isas_intra"  ~ 8,
      name %in% "liv_fam"     ~ 9,
      name %in% "rel_stat"    ~ 10,
      name %in% "pastpsyc"    ~ 11,
      name %in% "pastmed"     ~ 12,
      name %in% "ses_mean"    ~ 13,
      name %in% "cov_sess"    ~ 14
    ),
    namep = case_when(
      name %in% "cov_sess"    ~ "Total sessions <i>[M(SD)]</i>",
      name %in% "ses_mean"    ~ "Mean days between sessions <i>[M(SD)]</i>",
      name %in% "ac"          ~ "Acquired capability <i>[M(SD)]</i>",
      name %in% "isas_social" ~ "ISAS—Interpersonal <i>[M(SD)]</i>",
      name %in% "isas_intra"  ~ "ISAS—Intrapersonal <i>[M(SD)]</i>",
      name %in% "age"         ~ "Age <i>[M(SD)]</i>",
      name %in% "sex_mf"      ~ "Gender<sup>a</sup> (Female)",
      name %in% "liv_fam"     ~ "Living with family",
      name %in% "hxdsh"       ~ "History of deliberate self-injury",
      name %in% "hxtrauma"    ~ "History of personal trauma",
      name %in% "hxsa"        ~ "One or more suicide attempts",
      name %in% "pastpsyc"    ~ "Previous psychological intervention",
      name %in% "pastmed"     ~ "Previous / current psychiatric medication",
      name %in% "rel_stat"    ~ "In a relationship",
      TRUE ~ toupper(name)
    ), .after = name
  ) %>%
  arrange(order)

## COEFFICIENT TABLES
## =============================================================================
## TODO add back the statndardizedsolution(fit, type = "std.lv") for the CI
riclpm_coef <-
  summary(
    riclpm_fit,
    standardized = TRUE
  ) %>%
  as.data.frame() %>%
  rename_all(~ str_remove(.x, "PE\\.")) %>%
  clean_names()

.fit_stat <-
  lavInspect(riclpm_fit, "fit")

fit_stat <-
  .fit_stat %>%
  as.data.frame() %>%
  rownames_to_column(var = "stat") %>%
  rename(val = 2) %>%
  mutate(valpr = sprintf("%.2f", round(val, 2)))

## AR AND CL PATHWAYS
## =============================================================================
.fit_arcl_mod <-
  ## parameterestimates(riclpm_fit, standardized = TRUE) %>%
  standardizedsolution(riclpm_fit, type = "std.lv") %>%
  ## select(lhs, op, rhs, std.all, se, pvalue) %>%
  filter(op %in% "~")

within_fit <-
  .fit_arcl_mod %>%
  separate(lhs, sep = "_", into = c("lhs", "time")) %>%
  separate(rhs, sep = "_", into = c("rhs", "timer")) %>%
  mutate(
    across(c(lhs, rhs), ~ str_remove(.x, "^w")),
    ar = if_else(lhs == rhs, "ar", "cl"),
    ) %>%
  filter(pvalue < 0.05) %>%
  arrange(rhs) %>%
  split(.$ar) %>%
  map(
    ~.x %>%
      group_by(rhs, lhs) %>%
      mutate(
        across(c(timer, time),
               ~ str_c("T", .x)
               )
      ) %>%
      unite("spread", timer, time, sep = "→") %>%
      mutate(
        across(
          c(pvalue, est.std),
          list(
            min = ~ case_when(
              min(.x) < 0.001 ~ "<.001",
              min(.x) < 0.01 ~ "<.01",
              TRUE ~ str_replace(sprintf("%.2f", round(min(.x), 2)), "0\\.", "\\."),       # nolint
              ),
            max = ~ case_when(
              max(.x) < 0.001 ~ "<.001",
              max(.x) < 0.01 ~ "<.01",
              TRUE ~ str_replace(sprintf("%.2f", round(max(.x), 2)), "0\\.", "\\."),       # nolint
              )
          ),
          .names = "{.col}_{.fn}"
        ),
        spread = paste0(spread, collapse = ", "),
        spread = case_when(
          nchar(spread) > 16 ~ stringi::stri_replace_last_fixed(spread, ", T", ", and T"), # nolint
          TRUE ~ stringi::stri_replace_last_fixed(spread, ", T", " and T")
        )
      ) %>%
      rename_with(
        ~ str_replace(.x, "pvalue_", "p_"), starts_with("pvalue_")) %>%
      rename_with(
        ~ str_replace(.x, "est\\.std_", "B_"), starts_with("est.std_")) %>%
      slice(n = 1) %>%
      select(rhs, lhs, spread, ends_with(c("min", "max"))) %>%
      ungroup()
  )

riclpm_arcl <-
  .fit_arcl_mod %>%
  separate(lhs, sep = "_", into = c("lhs", "time")) %>%
  separate(rhs, sep = "_", into = c("rhs", "timer")) %>%
  mutate(
    across(c(lhs, rhs), ~ str_remove(.x, "^w")),
    across(c(lhs, rhs), ~ toupper(.x)),
    var = str_c(rhs, "→", lhs), .before = 1,
    ar = if_else(lhs == rhs, "ar", "cl"),
    B = if_else(
      est.std > 0,
      paste0(" ", str_remove(sprintf("%.2f", est.std), "^0")),
      str_replace(sprintf("%.2f", est.std), "0\\.", "\\.")),
    Bsig = case_when(
      pvalue < 0.001 ~ paste0("<b>", B, "<sup>***</sup></b>"),
      pvalue < 0.01  ~ paste0("<b>", B, "<sup>** </sup></b>"),
      pvalue < 0.05  ~ paste0("<b>", B, "<sup>*  </sup></b>"),
      pvalue >= 0.05 ~ paste0(B, "<sup>   </sup>")
    ),
    se = str_remove(sprintf("%.2f", se), "^0"),
    across(
      ci.lower,
      ~ if_else(
        .x > 0,
        paste0(" ", str_remove(sprintf("%.2f", .x), "^0")),
        str_replace(sprintf("%.2f", .x), "0\\.", "\\.")),
      ),
    ci.upper = str_remove(sprintf("%.2f", ci.upper), "^0"),
    ci = paste0("[", ci.lower, ", ", ci.upper, " ]"),
    ) %>%
  select(ar, var, time, Bsig, se, ci) %>%
  split(.$ar) %>%
  map(
    ~ .x %>%
      pivot_wider(
        names_from = time,
        values_from = c(Bsig, se, ci)
      ) %>%
      select(var, ends_with(as.character(2:5))) %>%
      mutate(
        order = case_when(
          str_detect(var, "^PB")  ~ 1,
          str_detect(var, "^TB")  ~ 2,
          str_detect(var, "^BHS") ~ 3,
          str_detect(var, "^DEP") ~ 4,
          str_detect(var, "^SI")  ~ 5
        ), .before = var
      )
  ) %>%
  bind_rows(.id = "path") %>%
  arrange(path, order) %>%
  mutate(
    pathp = if_else(path %in% "ar", "Autoregressive", "Cross-lagged"),
    .after = path)


## CORR TABLES
## =============================================================================

## CORRELATION INDICIES
## -----------------------------------------------------------------------------
.fit_corr_mod <-
  ## standardizedsolution(riclpm_fit, type = "std.lv") %>%
  parameterestimates(riclpm_fit, standardized = TRUE) %>%
  select(lhs, op, rhs, std.all, pvalue) %>%
  filter(op %in% "~~")

## ri cor vals
ri_cor <-
  .fit_corr_mod %>%
  filter(str_detect(lhs, "^RI") & pvalue < 0.05) %>%
  mutate(
    across(c(lhs, rhs), ~ str_remove(.x, "^RI")),
    var = str_c(lhs, "–", rhs), .before = 1,
    std.all = if_else(std.all < 1,  std.all, NA_real_),
    r = str_remove(sprintf("%.2f", std.all), "^0"),
    p = case_when(
      pvalue < 0.001 ~ "<.001",
      pvalue < 0.01 ~ "<.01",
      TRUE ~ str_remove(sprintf("%.2f", pvalue), "^0"),
      ),
    si = if_else(rhs %in% "si", "yes", "no")
  ) %>%
  filter(lhs != rhs) %>%
  split(.$si) %>%
  map(
    ~ .x %>%
      select(var, r, p, pvalue, std.all)
  )

## between person and within person correlations
riclpm_cor <-
  .fit_corr_mod %>%
  filter(str_detect(lhs, "^w|^RI")) %>%
  separate(lhs, sep = "_", into = c("varl", "time")) %>%
  separate(rhs, sep = "_", into = c("varr", "timer")) %>%
  mutate(
    time = if_else(is.na(time), "ri", time),
    across(c(varl, varr), ~ str_remove(.x, "^w|^RI")),
    var = str_c(varl, "–", varr), .before = 1,
    std.all = if_else(std.all < 1,  std.all, NA_real_),
    r = case_when(
      pvalue < 0.001 ~ paste0(str_remove(sprintf("%.2f", std.all), "^0"), "<sup>***</sup>"), # nolint
      pvalue < 0.01  ~ paste0(str_remove(sprintf("%.2f", std.all), "^0"), "<sup>** </sup>"), # nolint
      pvalue < 0.05  ~ paste0(str_remove(sprintf("%.2f", std.all), "^0"), "<sup>*  </sup>"), # nolint
      pvalue >= 0.05 ~ paste0(str_remove(sprintf("%.2f", std.all), "^0"), "<sup>   </sup>")  # nolint
    ),
  ) %>%
  filter(varl != varr) %>%
  select(var, time, r) %>%
  pivot_wider(
    names_from = time,
    values_from = r
  ) %>%
  select(var, ri, ends_with(as.character(1:5))) %>%
  mutate(
    across(where(is.numeric),
           ~ sprintf("%.2f", .x)
           ),
    var = toupper(var)
  ) %>%
  clean_names() %>%
  rename_with(~ str_replace(.x, "^x", "t"), starts_with("x"))

## CORRELATION TABLE
## -----------------------------------------------------------------------------
.cor_tbl <-
  function(df) {
    df <- as.matrix(df)
    diag(df) <- "--"
    df[upper.tri(df)] <- ""
    df <- as.data.frame(df)
    return(df)
  }

.cor_des <-
  phd_fin_wide %>%
  select(
    pb_1, pb_2, pb_3, pb_4, pb_5,
    tb_1, tb_2, tb_3, tb_4, tb_5,
    bhs_1, bhs_2, bhs_3, bhs_4, bhs_5,
    dep_1, dep_2, dep_3, dep_4, dep_5,
    si_1, si_2, si_3, si_4, si_5,
    ac_1, age, sex_mf, cov_sess,
    isas_social, isas_intra,
    hxdsh, hxtrauma, hxsa,
    liv_fam,
    pastmed, pastpsyc
  )

fit_corr <-
  lavCor(
    .cor_des,
    se = "robust",
    output = "fit",
    missing = "ml",
    estimator = "mlr",
    fixed.x = FALSE
  )

summary(fit_corr)

lavCor(fit_corr, output = "vcov")
lavCor(fit_corr, "vcov")

.fit_corr_pe <-
  parameterestimates(fit_corr, standardized = TRUE) %>%
  select(lhs, op, rhs, std.all, pvalue) %>%
  filter(op %in% "~~")

corr_mat <-
  .fit_corr_pe %>%
  mutate(
    r = if_else(
      std.all > 0,
      paste0(" ", str_remove(sprintf("%.2f", std.all), "^0")),
      str_replace(sprintf("%.2f", std.all), "0\\.", "\\.")),
    rsig = case_when(
      pvalue < 0.001 ~ paste0(r, "<sup>***</sup>"),
      pvalue < 0.01  ~ paste0(r, "<sup>** </sup>"),
      pvalue < 0.05  ~ paste0(r, "<sup>*  </sup>"),
      pvalue >= 0.05 ~ paste0(r, "<sup>   </sup>")
    ),
    id = row_number()
  ) %>%
  select(id, lhs, rhs, rsig) %>%
  pivot_wider(
    names_from = rhs,
    values_from = rsig
  ) %>%
  select(-id) %>%
  group_by(lhs) %>%
  summarise_all(coalesce_by_column) %>%
  ungroup()

corr_mat_n <-
  corr_mat %>%
  select(-lhs) %>%
  names()

corr_mat[match(corr_mat_n, corr_mat$lhs), ]

corr_mat <-
  corr_mat %>%
  column_to_rownames(var = "lhs") %>%
  t() %>%
  as.data.frame() %>%
  select(all_of(corr_mat_n)) %>%
  .cor_tbl() %>%
  rownames_to_column()

## COR PLOTS
## -----------------------------------------------------------------------------
.cor_plot <-
  .cor_des %>%
  as.matrix() %>%
  Hmisc::rcorr()

cp_r <- .cor_plot$r
cp_p <- .cor_plot$P

cor_plot <-
  corrplot::corrplot(
    cp_r,
    p.mat = cp_p,
    method = "color",
    diag = FALSE,
    type = "lower",
    insig = "blank",
    tl.cex = 0.7,
  )

### ============================================================================
### WRITE IMAGE AND DATA
### ============================================================================
save.image("./data/proc/draft.RData")
saveRDS(mget(ls()), "./data/proc/riclpm_df.rds")

### ============================================================================
### END
### ============================================================================
