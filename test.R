pca_var$contrib %>% 
    as.data.frame() %>% 
    tibble::rownames_to_column("obs") %>% 
    janitor::clean_names() %>% 
    pivot_longer(cols = dim_1:dim_6, names_to = "comp") %>% 
    group_by(comp) %>% 
    top_n(n = 3, wt = value) %>% 
    
    ggplot(aes(reorder(obs, value), value, fill=comp))+
    geom_col(position = "dodge")+
    coord_flip()
