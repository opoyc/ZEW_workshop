data_split <- wine_w %>% 
    mutate_at(.vars = vars(-quality, -quality_b), .funs = ~scale(.)) %>% 
    initial_split(prop = .7)
(train_data <- training(data_split)) %>% dim()
(test_data <- testing(data_split)) %>% dim()


ridge <- vfold_cv(train_data, v = 10) %>% 
    mutate(train=map(splits, ~training(.x))
           , validate=map(splits, ~testing(.x))
           , ridge=map(train, ~linear_reg(mixture = 0, penalty = 0.01023531) %>%
                         set_engine("glmnet") %>%
                         fit(quality~.-quality_b, data=.x)
                         )
           , pred=map2(.x=ridge, .y=validate, ~predict(.x, .y) %>% 
                           as_tibble %>% 
                           set_names("pred"))
           , mae=map2_dbl(.x=pred, .y=validate
                              , ~measures::MAE(truth = .y$quality
                                               , response = .x$pred)
               ))



# 
#            , pred=map2(.x=ridge, .y=validate, ~multi_predict(.x, .y) %>%
#                            as_tibble %>% 
#                            flatten() %>% 
#                            bind_rows() %>% 
#                            set_names(c("lambda", "pred")) %>% 
#                            group_by(lambda) %>% 
#                            nest())
#            , mae=map2(.x=pred, .y=validate, ~map_dbl(.x$lambda, function(p) measures::MAE(.y$quality, p)
#                                                      ) %>% enframe
#                       )
#            , mae2=map2(.x=pred, .y=mae, ~tibble(lambda=.x$lambda, mae=.y$value))
#            )

ridge %>% 
    unnest(mae2) %>%
    filter(lambda<10) %>% 
    ggplot(aes(lambda, mae))+
    geom_point(aes(col=id))

X <- wine_w %>% dplyr::select(-quality, -quality_b) %>% as.matrix()
y <- wine_w %>% dplyr::select(quality) %>% as.matrix()

lambdas_to_try <- 10^seq(-3, 5, length.out = 100)
# Setting alpha = 0 implements ridge regression
ridge_cv <- cv.glmnet(X, y, alpha = 0
                      #, lambda = lambdas_to_try,
                      , standardize = TRUE, nfolds = 10)
# Plot cross-validation results
plot(ridge_cv)

model <- linear_reg(mixture = 0) %>% 
    set_engine("glmnet") %>% 
    fit(quality~.-quality_b, data=ridge$train[[1]])


x <- multi_predict(model, ridge$validate[[1]])

x2 <- multi_predict(model, ridge$validate[[1]])$.pred %>% bind_rows(.id = "obs") %>% 
    set_names(c("obs", "penalty", "pred"))

x2 %>%
    group_by(penalty) %>% 
    nest() %>%
    mutate(mae=map_dbl(data, ~measures::MAE(truth = ridge$validate[[1]]$quality, response = .x$pred))) %>% 
    ggplot(aes(log(penalty), mae))+
    geom_point()
    


ridge_best <- glmnet(x = X, y = y, alpha = 0, lambda = 0.01831564)


res <- glmnet(X, y, alpha = 0, lambda = lambdas_to_try, standardize = FALSE)
plot(res, xvar = "lambda")
legend("bottomright", lwd = 1, col = 1:11, legend = colnames(X), cex = .7)


# 
# 
# ---
#     # Dataset
#     
#     Description: Ongoing research on university faculty perceptions and practices of using Wikipedia as a teaching resource. Based on a Technology Acceptance Model, the relationships within the internal and external constructs of the model are analyzed. Both the perception of colleaguesâ€™ opinion about Wikipedia and the perceived quality of the information in Wikipedia play a central role in the obtained model. 
# Link: https://archive.ics.uci.edu/ml/datasets/wiki4HE
# 
# ```{r, echo=T, warning=FALSE, message=FALSE}
# data0 <- readr::read_csv2("datasets/session_7/wiki4HE.csv") %>%
#     na_if(y = "?") %>% 
#     mutate_if(.predicate = is.character
#               , .funs = ~as.numeric(.)) %>% 
#     janitor::clean_names()
# ```
# 
# ```{r}
# 
# ```
# 
# 
# ---
#     # Missing values
#     
#     ```{r, echo=F, fig.width=6, fig.height=3.5, fig.retina=3, out.width="80%"}
# naniar::vis_miss(data0, cluster = T)
# ```
# 
# ---
#     # Imputation
#     
#     .pl[
#         ```{r}
#         (miss <- map_dbl(data0, .f = ~is.na(.x) %>% sum/913) %>% 
#                 enframe() %>% 
#                 arrange(desc(value)))
#         ```
#         ]
# 
# .pr[
#     ```{r, eval=FALSE}
#     data <- data0 %>% 
#         dplyr::select(miss$name[-c(1:2)]) %>% 
#         dplyr::select(sort(names(.))) %>% 
#         mice::mice()
#     ```
#     
#     ```{r, eval=FALSE}
#     # Save the data
#     write_rds(data %>% 
#                   mice::complete(1) %>% 
#                   as_tibble()
#               , path = "datasets/session_7/wiki4HE_imp.rds")
#     ```
#     
#     ]
# 
# ---
#     # Imputation: final dataset
#     
#     ```{r, fig.width=6, fig.height=3.5, fig.retina=3, out.width="80%"}
# (wiki <- read_rds("datasets/session_7/wiki4HE_imp.rds")) %>% 
#     naniar::vis_miss()
# ```
# 
# ---
#     # Imputation: final dataset
#     
#     ```{r, out.width="70%"}
# wiki %>% 
#     select(ph_d, gender, university, userwiki, yearsexp, age
#            , matches(match = "[0-9]")
#     ) %>% 
#     DT::datatable(options = list(pageLength=10))
# ```





ridge_shrink <- glmnet(X, y, alpha = 0, lambda = ridge_lambda, standardize = FALSE)
lasso_shrink <- glmnet(X, y, alpha = 1, lambda = lasso_lambda, standardize = FALSE)


lasso_shrink$beta %>%
    as.matrix() %>%
    t() %>% 
    as.data.frame() %>% 
    as_tibble() %>%
    mutate(lambda=lasso_lambda) %>% 
    pivot_longer(cols = -lambda, names_to = "variable", values_to = "coefficient") %>% 
    ggplot(aes(log(lambda), coefficient, col=variable))+
    geom_line()+
    geom_hline(yintercept = 0, linetype=2)

ridge_shrink$beta %>%
    as.matrix() %>%
    t() %>% 
    as.data.frame() %>% 
    as_tibble() %>%
    mutate(lambda=ridge_lambda) %>% 
    pivot_longer(cols = -lambda, names_to = "variable", values_to = "coefficient") %>% 
    ggplot(aes(log(lambda), coefficient, col=variable))+
    geom_line()+
    geom_hline(yintercept = 0, linetype=2)





