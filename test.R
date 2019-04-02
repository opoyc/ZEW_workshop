stdF <- function(x){
    (x-mean(x, na.rm = T))/sd(x, na.rm = T)
}

normF <- function(x){
    (x-min(x, na.rm = T))/(max(x, na.rm = T)-min(x, na.rm = T))
}

x <- tibble::tibble(x1=0+1*rep(1, 100)+rnorm(100, sd = 2)
               ,x2=0+4*rep(1, 100)+rnorm(100)
               , x3=(1:100)) %>% 
    mutate_at(vars(x1, x2), .funs = list(std=~stdF(.)
                                         , norm=~normF(.)))

x %>% 
    pivot_longer(cols = -x3) %>% 
    mutate(type=case_when(
        str_detect(name, pattern = "std")~"std"
        , str_detect(name, pattern = "norm")~"norm"
        , TRUE~"original"
        )
        , var=str_trunc(name, width = 2, side = "left")
    ) %>% 
    ggplot(aes(value, col=name, fill=type))+
    geom_density(alpha=0.7)+
    facet_grid(~type)


    mutate(type=str_extract(name, pattern = "(?<=_)\\S+"))
    
              
              
              

xt <- x %>% 
    mutate(x1=normF(x1)
           , x2=normF(x2))



library(ggplot2)
require(tidyverse)

xt %>%
    pivot_longer(cols = c(x1, x2)) %>% 
    ggplot(aes(x3, value, col=name))+
    geom_point()+
    scale_y_continuous(limits = c(-7,7))+
    scale_x_continuous(limits = c(-1,1))

