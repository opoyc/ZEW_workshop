## ----setup, include=FALSE------------------------------------------------
options(htmltools.dir.version = FALSE)
#require(plotly)
require(tidyverse)
#require(ggplot2)


## ------------------------------------------------------------------------
if(T==TRUE) print("This is tautological!")


## ------------------------------------------------------------------------
if(T) print("Same as above, but implicitly")


## ------------------------------------------------------------------------
if(FALSE==FALSE) print("Does it even worth mention it?")


## ------------------------------------------------------------------------
if(FALSE) print("This is not going to be printed")


## ------------------------------------------------------------------------
if(5<10) print("5 is less than 10")


## ------------------------------------------------------------------------
x <- 8
if(x<=7){
  print("x is less equal than 7")
} else { # else MUST appear in the same line were the curly bracket closes
  print("x is more than 7")
}


## ------------------------------------------------------------------------
if(x<=7){
  print("x is less equal than 7")
} else if(x<10) {
  print("x is more than 7 but less than 10")
} else {
  print("x is more than 10")
}


## ------------------------------------------------------------------------
x <- 1:10
ifelse(test = x<5, yes = "less than 5", no = "more equal than 5")


## ------------------------------------------------------------------------
x <- letters[1:10]

for(i in 1:length(x)){
  print(x[i]=="g")
}



## ------------------------------------------------------------------------
temperature <- 10
while(temperature < 18){
  print(paste0("If the temperature is "
               , temperature, " C°: Do not swim"))
  temperature=temperature+1
}


## ---- echo=FALSE, fig.align='center'-------------------------------------
knitr::include_graphics("https://media.giphy.com/media/EmMWgjxt6HqXC/giphy.gif")


## ---- eval=FALSE---------------------------------------------------------
## repeat{
##   message("This won't stop!!") # It is not evaluated
## }


## ------------------------------------------------------------------------
x <- 1
repeat{
  print(x)
  x <- x+1
  if(x==5){
    break
  }
}


## ------------------------------------------------------------------------
set.seed(123)
rep(x = rnorm(1), 7)
replicate(n = 7, expr = rnorm(1))


## ------------------------------------------------------------------------
(mat <- matrix(1:25, nrow = 5))


## ------------------------------------------------------------------------
apply(X = mat, MARGIN = 1, FUN = sum)
apply(X = mat, MARGIN = 2, FUN = sum)


## ------------------------------------------------------------------------
set.seed(123)
list <- list(e1=rnorm(100, 5, 1)
             , e2=rnorm(100, 10, 1)
             , e3=rnorm(100, 15, 1)
             , e4=list(rnorm(100, 5, 1)*100))
lapply(X = list, FUN = mean)


## ------------------------------------------------------------------------
lapply(X = list, FUN = function(x) mean(x[[1]]))


## ------------------------------------------------------------------------
sapply(list, function(x) mean(x[[1]]))
sapply(list, function(x) mean(x[[1]]), simplify = F)


## ------------------------------------------------------------------------
mapply(rnorm, n=1:5, mean=2, sd=1)


## ------------------------------------------------------------------------
ls("package:base", pattern = "read")
ls("package:utils", pattern = "read")


## ---- echo=F-------------------------------------------------------------
knitr::include_graphics("https://media.giphy.com/media/5AVgmIw7iAzdK/giphy.gif")


## ---- echo=F, out.width="70%", fig.align='center'------------------------
knitr::include_graphics("img/s2i1.png")


## ------------------------------------------------------------------------
readLines(con = "datasets/sample.txt", n = 11)


## ------------------------------------------------------------------------
read.table(file = "datasets/sample.txt", sep = "\t")

## ---- error=TRUE---------------------------------------------------------
read.delim(file = "datasets/sample.txt", sep = ".")


## ------------------------------------------------------------------------
data <- read.csv("datasets/big-mac-full-index.csv"
                 , stringsAsFactors = F)
str(data)


## ------------------------------------------------------------------------
survey <- foreign::read.spss("datasets/survey.sav"
                             , to.data.frame = T)
dim(survey)


## ------------------------------------------------------------------------
head(attributes(survey)$variable.labels)


## ------------------------------------------------------------------------
(money <- foreign::read.dta("datasets/money.dta"))


## ------------------------------------------------------------------------
(money <- haven::read_dta("datasets/money.dta"))


## ---- echo=FALSE, out.width="25%"----------------------------------------
knitr::include_graphics("https://media.giphy.com/media/NS7gPxeumewkWDOIxi/giphy.gif")


## ------------------------------------------------------------------------
haven::write_dta(data = head(survey), path = "datasets/survey.dta")


## ------------------------------------------------------------------------
export_obj <- survey[1:5, 1:3]


## ------------------------------------------------------------------------
write.csv(x = export_obj, file = "datasets/sample1.csv")
write.csv2(x = export_obj, file = "datasets/sample2.csv")


## ------------------------------------------------------------------------
readLines(con = "datasets/sample1.csv", n = 2)
readLines(con = "datasets/sample2.csv", n = 2)


## ---- eval=FALSE---------------------------------------------------------
## saveRDS(object = object, file = "file.rds")


## ---- eval=F-------------------------------------------------------------
## readRDS(file = "file.rds")


## ---- eval=FALSE---------------------------------------------------------
## save(list = list_objects, file = "file.RData")


## ---- eval=FALSE---------------------------------------------------------
## load(file = "file.RData")


## ---- echo=F, out.width="25%"--------------------------------------------
knitr::include_graphics("img/hex-tidyverse.png")


## ---- echo=FALSE, fig.showtext="fff", out.width="505", fig.cap="Data analysis workflow (source: Wickham & Garret, 2017)", fig.align='center'----
knitr::include_graphics("img/s2i2.png")


## ---- echo=FALSE---------------------------------------------------------
(data <- data.frame(name=c("rebecca", "thomas", "janna")
                   , treatment_a=c(1, 3, 4)
                   , treatment_b=c(2, 6, 8)))


## ---- echo=FALSE---------------------------------------------------------
data2 <- t(as.matrix(data[,-1]))
colnames(data2) <- data$name
data2


## ---- echo=FALSE---------------------------------------------------------
data %>% gather(key = "treatment", value = "result", -name)


## ---- eval=FALSE---------------------------------------------------------
## install.packages("tidyverse")


## ------------------------------------------------------------------------
x <- survey$age # New intermediary variable
age_2 <- x^2 # Apply the function
(mean_2 <- mean(age_2)) # Calculate the mean of squared age


## ------------------------------------------------------------------------
(mean_2 <- mean((survey$age)^2))


## ------------------------------------------------------------------------
survey$age %>%
  .^2 %>%
  mean()


## ------------------------------------------------------------------------
(survey_2 <- as_tibble(survey[1:100
                            , c("sex", "age", "educ", "mast1")]
                     )
 ) # Let's create a sample of survey from the SPSS file


## ---- eval=T-------------------------------------------------------------
survey_2$age_2 <- survey_2$age^2
survey_2$log_age <- log(survey_2$age)
survey_2 %>% head(3)


## ---- eval=T-------------------------------------------------------------
survey_2 <- survey_2 %>%
  mutate(age_2=age^2
         , log_age= age %>%
           log())
head(survey_2, 3)


## ---- eval=T-------------------------------------------------------------
survey_2 %>%
  transmute(age_2=age^2
         , log_age= age %>%
           log()) %>%
  head(3)


## ------------------------------------------------------------------------
survey_2[,"age"] %>% head(3)


## ------------------------------------------------------------------------
survey %>%
  select(age) %>%
  head(3)

survey %>%
  select(edad=age) %>%
  head(3)

survey %>%
  select(contains("age")) %>%
  head(3)


## ------------------------------------------------------------------------
survey_2[survey_2$sex=="FEMALES",] %>% head(3)


## ------------------------------------------------------------------------
survey_2 %>%
  filter(sex=="FEMALES") %>%
  head(3)


## ------------------------------------------------------------------------
survey_2 %>%
  filter(sex=="FEMALES" & age_2==576) %>% head(3)


## ------------------------------------------------------------------------
aggregate(survey_2$age, by=list(survey_2$educ, survey_2$sex), FUN=mean)


## ------------------------------------------------------------------------
survey_2 %>%
  group_by(educ, sex) %>%
  summarise(mean_age=mean(age))


## ------------------------------------------------------------------------
head(survey_2)[order(head(survey_2$age)),]


## ------------------------------------------------------------------------
head(survey_2) %>%
  arrange(age)

