# WDI ####

devep <- readr::read_csv("datasets/session_7/dev_countries.csv") %>% 
    mutate_all(.funs = ~str_to_lower(.))


wdi <- WDI::WDI(country = "all", indicator = c("unemp"="SL.UEM.TOTL.ZS"
                                               , "secon_enrol"="SE.SEC.ENRR"
                                               , "gov_exp_ed"="SE.XPD.TOTL.GD.ZS"
                                               , "fert_rate"="SP.DYN.TFRT.IN"
                                               , "life_exp"="SP.DYN.LE00.MA.IN"
                                               , "gni_pc"="NY.GNP.PCAP.CD"
                                               , "refugee"="SM.POP.REFG"
                                               , "fdi"="BX.KLT.DINV.CD.WD"
                                               , "start_bus"="IC.REG.DURS"
                                               , "res_exp"="GB.XPD.RSDV.GD.ZS"
                                               , "cpi"="FP.CPI.TOTL"
                                               , "co2_em"="EN.ATM.CO2E.PC"
                                               , "forest_area"="AG.LND.FRST.ZS"
                                               , "driking_water"="SH.H2O.SMDW.ZS"
                                               , "emp_serv"="SL.SRV.EMPL.ZS"
                                               , "mort_rate"="SH.DYN.MORT")
                , start = 1990
                , end = 2018) %>% 
    as_tibble()


wdi_case <- wdi %>% 
        group_by(country) %>% 
        summarise_at(.vars = vars(unemp:mort_rate)
                     , .funs = ~mean(., na.rm = T)
        ) %>% 
        filter(complete.cases(.)) %>% # filter and keep complete cases only
        mutate(country=str_to_lower(country)) %>% 
        filter(!grepl(country, pattern = "asia|euro|america|demographic|countries|income|ida|oecd|small|africa|world")
        ) %>% 
  left_join(devep) %>% 
  #replace_na(list(dev_class="non-developed")) %>% 
    dplyr::select(country, code, dev_class, inc_class, region, inc_class, everything())

write_rds(wdi_case, path = "datasets/session_7/wdi_2.rds")


# Census ####


data_train <- readr::read_delim(file = "datasets/session_7/adult.data", delim = ","
                                , col_names = c("age", "workclass", "fnlwgt", "education", "education_num"
                                                , "marital_status", "occupation", "relationship", "race", "sex"
                                                , "capital_gain", "capital_loss", "hours_per_week"
                                                , "native_country","inc"))
data_test <- readr::read_delim(file = "datasets/session_7/adult.test", delim = ","
                               , col_names = c("age", "workclass", "fnlwgt", "education", "education_num"
                                               , "marital_status", "occupation", "relationship", "race", "sex"
                                               , "capital_gain", "capital_loss", "hours_per_week"
                                               , "native_country","inc"))

census <- bind_rows(data_train, data_test) %>% 
    mutate_at(.vars = vars(age, fnlwgt, education_num, capital_gain:hours_per_week)
              , .funs = ~as.numeric(.)) %>%
    mutate_if(.predicate = is.character, .funs = ~str_squish(.)) %>% 
    mutate(inc=case_when(
            inc=="<=50K"~"less_50k"
            , inc=="<=50K."~"less_50k"
            , TRUE~"more_50k")
        , native_country=case_when(
            native_country=="United-States"~"USA"
            , TRUE~"non_USA"
        )
        , workclass=case_when(
            workclass=="Private"~"private"
            , T~"non_private"
        )) %>% 
    mutate_at(.vars = vars(marital_status, relationship, race,occupation)
              , .funs = ~fct_lump(f = .x, n=4) %>% 
                  fct_infreq(.)) %>% 
    dplyr::select(-fnlwgt, -education)

census <- fastDummies::dummy_cols(.data = census, select_columns = "inc")

write_rds(census, path = "datasets/session_7/census.rds")




# Online news ####

news <- readr::read_csv("datasets/session_7/OnlineNewsPopularity.csv") %>% 
    dplyr::select(-url, -timedelta, -matches("LDA"), -rate_negative_words
                  , kw_min_min:kw_avg_max, -contains("min"), -contains("max")
                  , -contains("weekday_is_")) %>%
    mutate(shares_b=case_when(
        shares>3400~1
        , TRUE~0
    )) %>% 
    dplyr::select(shares, shares_b, everything())

write_rds(news, path = "datasets/session_7/news.rds")

# wine ####

wine_red <- readr::read_delim("datasets/session_7/winequality-red.csv", delim = ";") %>% 
    janitor::clean_names() %>% 
    mutate(quality_b=case_when(
        quality>6~1
        , TRUE~0
    ))

wine_white <- readr::read_delim("datasets/session_7/winequality-white.csv", delim = ";") %>% 
    janitor::clean_names() %>% 
    mutate(quality_b=case_when(
        quality>5~1
        , TRUE~0
    ))

write_rds(wine_white, path = "datasets/session_7/wine_white.rds")
