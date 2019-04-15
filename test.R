pca_var$contrib %>% 
    as.data.frame() %>% 
    tibble::rownames_to_column("obs") %>% 
    janitor::clean_names() %>% 
    pivot_longer(cols = dim_1:dim_6, names_to = "comp") %>% 
    group_by(comp) %>% 
    top_n(n = 3, wt = value) %>% 
    mutate(rank=rank(desc(value))) %>% 
    ggplot(aes(reorder(obs, value), value))+
    geom_col()+
    facet_wrap(~comp, scales = "free")+
    theme(axis.text.x = element_text(angle = 45, hjust = 1))+
    labs(x="", y="Contribution")
