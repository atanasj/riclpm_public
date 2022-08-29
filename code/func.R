### ============================================================================
### LIBRARIES
### ============================================================================

## set seed
addTaskCallback(function(...) {
  set.seed(46810)
  TRUE
})

## load libraries
library(aj.HelpRs)   # load my helpRs
## these librares test multivariate normality also see
## http://dwoll.de/rexrepos/posts/normality.html
library(MVN)
library(glue)
library(here)
library(janitor)
library(psych)       # for general analysis
library(lubridate)
library(tidyverse)
library(hrbrthemes)  # themes for ggplot2
library(patchwork)   # combine multiple plots
library(knitr)
library(kableExtra)
library(lavaan)
library(blavaan)
## library(lmerTest)
## library(lme4)
## library(brms)
library(broom.mixed)
library(semTools)
library(DiagrammeR)
library(DiagrammeRsvg)
library(rsvg)
## library(conflicted)
## for the clmp_graph.R functions
## library(directlabels)
## library(ggfittext)
## conflict_prefer("select", "dplyr")
## conflict_prefer("filter", "dplyr")
## conflict_prefer("lmer", "lme4")
## conflict_prefer("chisq.test", "stats")

### ============================================================================
### GGPLOT THEMES
### ============================================================================

## ggplot <-
##   function(...)
##     ggplot2::ggplot(...) +
      ## scale_color_brewer(palette = "Set1") +
      ## scale_fill_brewer(palette = "Set1")

### ============================================================================
### KABLE EXTRA OPTION
### ============================================================================
kbl <-
  function(...) {
    kableExtra::kbl(
      ...,
      escape = FALSE,
      format = getOption("knitr.table.format")
      ) %>%
      kableExtra::kable_styling(
        latex_options = c("striped", "scale_down"),
        bootstrap_options = c(
          "striped", "hover", "condensed", "responsive"
        )
      )
  }

### ============================================================================
### LOCAL FUNCTIONS
### ============================================================================

## function to quickly create tmp files based on time of save
.tmp_csv <-
  function(data, x) {
    readr::write_csv(data,
      paste0(
        "./data/tmp/",
        ## name writer as string i.e., "name"
        x,
        format(Sys.time(), "_%Y-%m-%d_%H.%M.%S"),
        ".csv"
      ),
      na = ""
    )
  }

## function to quickly final rds for later analysis
.final_rds <-
  function(object, x) {
    saveRDS(object,
      paste0(
        "./data/proc/",
        ## name writer as string i.e., "name"
        x,
        format(Sys.time(), "_%Y_%m_%d"),
        ".rds"
      )
    )
  }

## function to collapse duplicates entries
.coalesce_by_column <-
  function(df) {
    return(dplyr::coalesce(!!!as.list(df)))
  }

## NOTE usage, do not run
## df <-
##   df %>%
##   group_by(ID) %>%
##   summarise_all(coalesce_by_column) %>%
##   ungroup()

## calculated pro rated means and percent missing
.pro_mean_miss <-
  function(data, new_name, ...) {
    data <-
      data %>%
      dplyr::mutate(
        ## mean of rows
        UQ(paste(rlang::syms(c(new_name)), "mean", sep = "_")) :=
          round(
            base::rowMeans(across(...), na.rm = TRUE) *
              ## scaled up to base measures
              base::rowSums(!is.na(across(...))),
            digits = 2
          ),
        ## percent missing of rows
        UQ(paste(rlang::syms(c(new_name)), "miss", sep = "_")) :=
          base::rowMeans(is.na(across(...)))
      )
    return(data)
  }


## calculated pro rated means and percent missing
.pro_mean <-
  function(data, new_name, ...) {
    data <-
      data %>%
      dplyr::mutate(
        ## mean of rows
        UQ(paste(rlang::syms(new_name))) :=
          if_else(
            ## if less than or equal to 20% missing
            base::rowMeans(is.na(across(...))) <= 0.2,
            ## calucate prorated mean
            round(
              base::rowMeans(across(...), na.rm = TRUE) *
                ## scaled up to base measures
                (base::rowSums(is.na(across(...))) +
                base::rowSums(!is.na(across(...)))),
              digits = 2
            ),
            ## else return NA
            NA_real_
          )
      )
    return(data)
  }

## RELIABLE AND SIGNIFICANT CHANGE
## =============================================================================
## reliable change
.rci_jt <-
  function(time_1, time_2) {
    x <-
      ## (time_1 - time_2)/Sdiff
      (time_2 - time_1) /
      ## Sdiff = sqrt(2 * SE^2)
      sqrt(
        2 * (
          ## SE = sd(time_1) * sqrt(1 - test-retest reliability)
          ## SE = sd(time_1) * sqrt(1 - cor(time_1, time_2))
          exp(
            sd(time_1, na.rm = TRUE) * sqrt(1 - cor(time_1, time_2)))
        )
      )
    return(x)
  }

.sigch_jt <-
  function(time_2, var_rc, var_co) {
    x <-
      case_when(
        var_rc <  -1.96 & time_2 <= var_co ~ "rec",
        var_rc >= -1.96 & time_2 <= var_co ~ "imp",
        ## TRUE ~ "noimp_det"
        var_rc <  -1.96 & time_2 > var_co ~ "norec_sigch",
        var_rc >= -1.96 & time_2 > var_co ~ "noimp_det",
      )
    return(x)
  }

## EFA SCREE PLOT
## -----------------------------------------------------------------------------
## extract relavent data & prepare dataframe for plot
## NOTE requires the output of an mplus object e.g.,
## efa_summary <- readModels(here("efa_dir", "mplus_file.out"))
.efa_scree <-
  function(efa_summary, .title = "EFA Scree Plot") {
    efa_title <- .title
    x <-
      list(
        EFA = efa_summary[["gh5"]][["efa"]][["eigenvalues"]],
        Parallel = efa_summary[["gh5"]][["efa"]][["parallel_average"]]
      )
    ## this
    plot_data <- as_data_frame(x)
    plot_data <- cbind(Factor = paste0(1:nrow(plot_data)), plot_data)
    ## then this
    plot_data <-
      plot_data %>%
      mutate(Factor = fct_inorder(Factor))
    ## pivot the dataframe to “long” format
    plot_data_long <-
      plot_data %>%
      pivot_longer(
        EFA:Parallel,             # The columns I'm gathering together
        names_to = "Analysis",    # new column name for existing names
        values_to = "Eigenvalues" # new column name to store values
      )
    ## plot using ggplot
    efa_scree_plot <-
      plot_data_long %>%
      ggplot(aes(
        y = Eigenvalues,
        x = Factor,
        group = Analysis,
        color = Analysis
      )) +
      geom_point() +
      geom_line() +
      labs(title = efa_title)
      theme_minimal()
    return(efa_scree_plot)
  }

## mice inspect missingness
.miss_df <-
  function(df) {
    .md_df <-
      df %>%
      mice::flux()
    .md_df_names <-
      names(df) %>%
      tibble::as_tibble() %>%
      dplyr::rename(var_name = 1)
    .md_summary <-
      dplyr::bind_cols(
        .md_df_names,
        .md_df
      )
    return(.md_summary)
  }

## mice inspect imputation method per var
.mice_meth_df <-
  function(df) {
    .meth_df <-
      df %>%
      as_tibble()
    .meth_df_names <-
      attr(df, "names") %>%
      tibble::as_tibble() %>%
      dplyr::rename(var_name = 1)
    .meth_summary <-
      dplyr::bind_cols(
        .meth_df_names,
        .meth_df
      ) %>%
      rename(old_value = value) %>%
      mutate(
        new_value = old_value,
        ID = row_number()
      )
    return(.meth_summary)
  }

## mice inspect imputation method per var, the oppositie to the .mice_meth_df
## and expects the object to be in that form
.mice_meth_matrix <-
  function(df) {
    values <-
      df %>%
      mutate(across(everything(), ~ replace_na(.x, ""))) %>%
      select(new_value) %>%
      deframe()
    value_names <-
      df %>%
      mutate(across(everything(), ~ replace_na(.x, ""))) %>%
      select(var_name) %>%
      deframe()
    value_names
    names(values) <- value_names
    return(values)
  }

## mice inspect predictor matrix
.mice_pred_df <-
  function(df) {
    .pred_df <-
      df %>%
      as_tibble()
    .pred_df_names <-
      attr(df, "dimnames")[[1]] %>%
      tibble::as_tibble() %>%
      dplyr::rename(var_name = 1)
    .pred_summary <-
      dplyr::bind_cols(
        .pred_df_names,
        .pred_df
      )
    return(.pred_summary)
  }

## return tibble to pred matrix to be used for mice
.mice_pred_matrix <-
  function(df) {
    .pred_matrix <-
      df %>%
      ## drops the "vars" column
      dplyr::select(-1) %>%
      as.matrix()
    attr(.pred_matrix, "dimnames")[[1]] <-
      attr(.pred_matrix, "dimnames")[[2]]
    return(.pred_matrix)
  }

## symmary by grouping var
.summary_by <-
  function(data, ..., .drop = TRUE) {
    stopifnot(inherits(data, "data.frame"))
    vars <-
      ensyms(...)
    vars <-
      purrr::map(
        vars,
        function(x) factor(rlang::eval_tidy(x, data), exclude = NULL)
      )
    new_df <-
      base::split(data, vars, drop = .drop)
    summary_by <-
      new_df %>%
      purrr::map(base::summary)
    return(summary_by)
  }

## CORRELATION MATRIX
## =============================================================================
## https://paulvanderlaken.com/2020/07/28/publication-ready-correlation-matrix-significance-r/
#' correlation_matrix
#' Creates a publication-ready / formatted correlation matrix, using `Hmisc::rcorr` in the backend.
#'
#' @param df dataframe; containing numeric and/or logical columns to calculate correlations for
#' @param type character; specifies the type of correlations to compute; gets passed to `Hmisc::rcorr`; options are `"pearson"` or `"spearman"`; defaults to `"pearson"`
#' @param digits integer/double; number of decimals to show in the correlation matrix; gets passed to `formatC`; defaults to `3`
#' @param decimal.mark character; which decimal.mark to use; gets passed to `formatC`; defaults to `.`
#' @param use character; which part of the correlation matrix to display; options are `"all"`, `"upper"`, `"lower"`; defaults to `"all"`
#' @param show_significance boolean; whether to add `*` to represent the significance levels for the correlations; defaults to `TRUE`
#' @param replace_diagonal boolean; whether to replace the correlations on the diagonal; defaults to `FALSE`
#' @param replacement character; what to replace the diagonal and/or upper/lower triangles with; defaults to `""` (empty string)
#'
#' @return a correlation matrix
#' @export
#'
#' @examples
#' `correlation_matrix(iris)`
#' `correlation_matrix(mtcars)`
correlation_matrix <- function(df,
                               type = "pearson",
                               digits = 3,
                               decimal.mark = ".",
                               use = "all",
                               show_significance = TRUE,
                               replace_diagonal = FALSE,
                               replacement = "") {

  # check arguments
  stopifnot({
    is.numeric(digits)
    digits >= 0
    use %in% c("all", "upper", "lower")
    is.logical(replace_diagonal)
    is.logical(show_significance)
    is.character(replacement)
  })
  # we need the Hmisc package for this
  require(Hmisc)

  # retain only numeric and boolean columns
  isNumericOrBoolean <- vapply(df, function(x) is.numeric(x) | is.logical(x), logical(1))
  if (sum(!isNumericOrBoolean) > 0) {
    cat("Dropping non-numeric/-boolean column(s):", paste(names(isNumericOrBoolean)[!isNumericOrBoolean], collapse = ", "), "\n\n")
  }
  df <- df[isNumericOrBoolean]

  # transform input data frame to matrix
  x <- as.matrix(df)

  # run correlation analysis using Hmisc package
  correlation_matrix <- Hmisc::rcorr(x, type = )
  R <- correlation_matrix$r # Matrix of correlation coeficients
  p <- correlation_matrix$P # Matrix of p-value

  # transform correlations to specific character format
  Rformatted <- formatC(R, format = "f", digits = digits, decimal.mark = decimal.mark)

  # if there are any negative numbers, we want to put a space before the positives to align all
  if (sum(R < 0) > 0) {
    Rformatted <- ifelse(R > 0, paste0(" ", Rformatted), Rformatted)
  }

  # add significance levels if desired
  if (show_significance) {
    # define notions for significance levels; spacing is important.
    stars <- ifelse(is.na(p), "<sup>   </sup>", ifelse(p < .001, "<sup>***</sup>", ifelse(p < .01, "<sup>** </sup>", ifelse(p < .05, "<sup>*  </sup>", "<sup>   </sup>"))))
    Rformatted <- paste0(Rformatted, stars)
  }
  # build a new matrix that includes the formatted correlations and their significance stars
  Rnew <- matrix(Rformatted, ncol = ncol(x))
  rownames(Rnew) <- colnames(x)
  colnames(Rnew) <- paste(colnames(x), "", sep = " ")

  # replace undesired values
  if (use == "upper") {
    Rnew[lower.tri(Rnew, diag = replace_diagonal)] <- replacement
  } else if (use == "lower") {
    Rnew[upper.tri(Rnew, diag = replace_diagonal)] <- replacement
  } else if (replace_diagonal) {
    diag(Rnew) <- replacement
  }

  return(Rnew)
}

## MCFA RELIABILITY ALPHA
## =============================================================================
mcfa_alpha <-
  function(df, ID) {
    # compute variables
    var_names <-
      df %>%
      ## NOTE only works with current data set i.e., ID is name of within level
      select(-eval(parse(text = ID))) %>%
      names()
    harm_mean <-
      count(df, eval(parse(text = ID))) %>%
      transmute(n_inv = 1 / n) %>%
      summarise(1 / mean(n_inv))
    var_no <- length(var_names)
    ## level 1
    level_1 <-
      str_c(
        "level: 1\n",
        pmap(
          list(
            v_n = var_names,
            v_l = seq(1:var_no),
            last_var = tail(var_names, 1),
            last_van_l = var_no,
            var_f <- str_remove_all(tail(var_names, 1), "\\d")
          ),
          .f = function(v_n, v_l, last_var, last_van_l, var_f) {
            paste0(
              v_n, " ~~ ",
              str_c(
                paste0(
                  paste0(
                    "t", v_l, rep(v_l:var_no), "w", " * ",
                    paste0(var_f, rep(v_l:var_no))
                  )
                ),
                collapse = " + "
              )
            )
          }
        ) %>%
          unlist(recursive = FALSE) %>%
          str_c(collapse = "\n")
      )

    ## level 2
    level_2 <-
      str_c(
        "\nlevel: 2\n",
        pmap(
          list(
            v_n = var_names,
            v_l = seq(1:var_no),
            last_var = tail(var_names, 1),
            last_van_l = var_no,
            var_f <- str_remove_all(tail(var_names, 1), "\\d")
          ),
          .f = function(v_n, v_l, last_var, last_van_l, var_f) {
            paste0(
              v_n, " ~~ ",
              str_c(
                paste0(
                  paste0(
                    "t", v_l, rep(v_l:var_no), "b", " * ",
                    paste0(var_f, rep(v_l:var_no)))
                ),
                collapse = " + "
              )
            )
          }
        ) %>%
          unlist(recursive = FALSE) %>%
          str_c(collapse = "\n")
      )

    .tw_err <-
      pmap(
        list(
          v_n = var_no,
          v_l = seq(1:var_no)
        ),
        .f = function(v_n, v_l) {
          paste0("t", v_l, rep(v_l:v_n), "w")
        }
      ) %>%
      map(~.x[[1]]) %>%
      unlist()

    tw_err <-
      .tw_err %>%
      str_c(collapse = " + ")

    tw_corr <-
      pmap(
        list(
          v_n = var_no,
          v_l = seq(1:var_no)
        ),
        .f = function(v_n, v_l) {
          paste0("t", v_l, rep(v_l:v_n), "w")
        }
      ) %>%
      map(~str_replace_all(.x, str_c(.tw_err, collapse = "|"), NA_character_))  %>%
      map(~.x[!is.na(.x)]) %>%
      unlist() %>%
      str_c(collapse = " + ")


    .tb_err <-
      pmap(
        list(
          v_n = var_no,
          v_l = seq(1:var_no)
        ),
        .f = function(v_n, v_l) {
          paste0("t", v_l, rep(v_l:v_n), "b")
        }
      ) %>%
      map(~.x[[1]]) %>%
      unlist()

    tb_err <-
      .tb_err %>%
      str_c(collapse = " + ")

    tb_corr <-
      pmap(
        list(
          v_n = var_no,
          v_l = seq(1:var_no)
        ),
        .f = function(v_n, v_l) {
          paste0("t", v_l, rep(v_l:v_n), "b")
        }
      ) %>%
      map(~str_replace_all(.x, str_c(.tb_err, collapse = "|"), NA_character_))  %>%
      map(~.x[!is.na(.x)]) %>%
      unlist() %>%
      str_c(collapse = " + ")

    tilalpb <-
      paste0(
        "\ntilalpb := ",
        var_no,
        " * 2 * (",
        tb_corr,
        ") / (",
        var_no,
        " - 1) / (",
        tb_err,
        " + 2 * (",
        tb_corr,
        "))\n"
      )

    tilalpw <-
      paste0(
        "\ntilalpw := ",
        var_no,
        " * 2 * (",
        tw_corr,
        ") / (",
        var_no,
        " - 1) / (",
        tw_err,
        " + 2 * (",
        tw_corr,
        "))\n"
      )

    alpha2l <-
      paste0(
        "\nalpha2l := ",
        var_no,
        " * 2 * (",
        tb_corr,
        " + ",
        tw_corr,
        ") / (",
        var_no,
        " - 1) / (",
        tb_err,
        " + 2 * (",
        tb_corr,
        ") + ",
        tw_err,
        " + 2 * (",
        tw_corr,
        "))\n"
      )

    alphab <-
      paste0(
        "\nalphab := ",
        var_no,
        " * 2 * (",
        tb_corr,
        ") / (",
        var_no,
        " - 1) / (",
        tb_err,
        " + 2 * (",
        tb_corr,
        ") + (",
        tw_err,
        " + 2 * (",
        tw_corr,
        ")) /",
        harm_mean,
        ")\n"
      )

    string_final <-
      str_c(
        level_1,
        level_2,
        tilalpb,
        tilalpw,
        alpha2l,
        alphab,
        collapse = "\n"
      )
    mcfa_fit <-
      lavaan::cfa(
        string_final,
        df,
        cluster = ID
      )
    return(mcfa_fit)
  }

### ============================================================================
### END SCRIPT: func.R
### ============================================================================
