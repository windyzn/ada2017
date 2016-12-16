mason_gee <- function(data,
                      yvars = "",
                      xvars = "",
                      covars = "") {
  mason::design(data, "gee") %>%
    mason::add_settings(
      family = stats::gaussian(),
      corstr = "ar1",
      cluster.id = "SID"
    ) %>%
    mason::add_variables("yvars", yvars) %>%
    mason::add_variables("xvars", xvars) %>%
    mason::add_variables("covariates", covars) %>%
    mason::construct() %>%
    mason::scrub() %>%
    mason::polish_adjust_pvalue(method = "BH") %>%
    dplyr::mutate_each(dplyr::funs(format_rounding),
                       estimate,
                       std.error,
                       conf.low,
                       conf.high) %>%
    dplyr::mutate_each(dplyr::funs(format_p), p.value, adj.p.value)
}

# Extra/original

# mason::design("gee") %>% 
#   mason::add_settings(family = stats::gaussian(),
#                       corstr = "ar1", cluster.id = "SID") %>% 
#   mason::add_variables("yvars", c("ACR", "eGFR")) %>% 
#   mason::add_variables("xvars", "udbpBase") %>% 
#   mason::add_variables("covariates", c("VN")) %>% 
#   mason::add_variables("covariates", c("VN", "ageBase", "Sex",
#                                        "BMI", "DM", "fMedsBP")) %>%
#   # mason::construct() %>% 
#   # mason::add_variables("yvars", c("eGFR")) %>% 
#   # mason::add_variables("xvar", "udbpBase") %>% 
#   # mason::add_variables("covariates", c("VN", "ageBase", "DM")) %>% 
#   mason::construct() %>% 
#   mason::scrub() %>% 
#   mason::polish_adjust_pvalue(method = "BH") %>% 
#   # mason::polish_filter("Xterm", "term") %>%
#   dplyr::mutate_each(dplyr::funs(format_rounding), estimate, std.error, conf.low, conf.high) %>%
#   dplyr::mutate_each(dplyr::funs(format_p), p.value, adj.p.value)