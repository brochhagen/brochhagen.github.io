library(readr)
library(dplyr)

library(brms)
library(rstudioapi)

library(ggplot2)
library(ggridges)
library(glue)
library(stringr)
library(forcats)
library(tidyr)

library(tidybayes)

 

### brms settings ###
chns <- 4
iters <- 2000
sdn <- 13
 
# Uses the computer cores available
options(mc.cores = parallel::detectCores())


## add names to different levels of each covariate

named.contr.sum<-function(x, ...) {
  if (is.factor(x)) {
    x <- levels(x)
  } else if (is.numeric(x) & length(x)==1L) {
    stop("cannot create names with integer value. Pass factor levels")
  }
  x<-contr.sum(x, ...)
  colnames(x) <- apply(x,2,function(x) 
    names(x[x>0])
  )
  x
}


# set the source file direction as working directory
#setwd(dirname(getSourceEditorContext()$path))

#### reading and building dataframes ####

df_estimates_SUBJ <- read.csv('df_estimates_subject3.csv') %>%
  mutate (
    articleID = as.factor(articleID),
    sampleID = as.factor(sampleID),
    subjectOrNot = rep('subject', length(article)),
    article = paste("SUBJ", article, sep=" "),
    article = str_replace_all(article, "[.]", " ")
  ) 


df_estimates_OBJ <- read.csv('df_estimates_object3.csv') %>%
  mutate (
    subjectOrNot = rep('object', length(article)),
    articleID = as.factor(articleID),
    sampleID = as.factor(sampleID),
    article = paste("OBJ", article, sep=" "),
    article = str_replace_all(article, "[.]", " ")
  )

# the following effect sizes come from models that didn't converge with random effect structure
df_estimates_OBJ <- df_estimates_OBJ[!(df_estimates_OBJ$article %in% c("OBJ hwang et al (icv1_because_so_null)", "OBJ Hwang_marker (exp2_tpv)")), ]


df_estimates_whole <- rbind(df_estimates_SUBJ, df_estimates_OBJ)
colnames(df_estimates_whole)[which(colnames(df_estimates_whole) == "format")] <- "modality"


####Section 4.1: predictability and the most reduced reference form (null & germanic pronouns) vs. all other forms ####


df_estimates_mostReduced <- subset (df_estimates_whole, pronoun_type == "pronoun/null" )
df_estimates_mostReduced_nonProDrop <- subset (df_estimates_whole, pronoun_type == "pronoun/null" & language_family == 'Germanic' )

#### Model 2: predictability and the most reduced reference form while controlling for variations from three factors: ####
### convert to factors ###
df_estimates_mostReduced$subjectOrNot <- factor(df_estimates_mostReduced$subjectOrNot, levels = c("object", "subject"))
df_estimates_mostReduced$language_family <- factor(df_estimates_mostReduced$language_family, levels = c("Turkic", "Mandarin", "Korean","Germanic", "Romance"))
df_estimates_mostReduced$manipulation <- factor(df_estimates_mostReduced$manipulation, levels = c("ICV", "relativeClause", "relation", "TPV"))


### sum coding ###
contrasts(df_estimates_mostReduced$language_family)=named.contr.sum(levels(df_estimates_mostReduced$language_family))
contrasts(df_estimates_mostReduced$subjectOrNot)=named.contr.sum(levels(df_estimates_mostReduced$subjectOrNot))
contrasts(df_estimates_mostReduced$manipulation)=named.contr.sum(levels(df_estimates_mostReduced$manipulation))


######Section 4.3: pro-drop languages #####

df_estimates_proDrop <- subset(df_estimates_whole, language %in% c("Catalan", "Italian", "Korean", "Mandarin",
                                                                   "Spanish", "Turkish") )

### add names to different levels of each covariate

df_estimates_proDrop$subjectOrNot <- factor(df_estimates_proDrop$subjectOrNot, levels = c("object", "subject"))
df_estimates_proDrop$language <- factor(df_estimates_proDrop$language, levels = c("Mandarin", "Catalan", "Spanish", "Italian", "Turkish", "Korean"))
df_estimates_proDrop$language_family <- factor(df_estimates_proDrop$language_family, levels = c("Turkic", "Mandarin", "Korean", "Romance"))
df_estimates_proDrop$manipulation <- factor(df_estimates_proDrop$manipulation, levels = c("ICV", "relation", "TPV"))
df_estimates_proDrop$pronoun_type <- factor(df_estimates_proDrop$pronoun_type, levels = c("overt", "pronoun/null"))


contrasts(df_estimates_proDrop$language_family)=named.contr.sum(levels(df_estimates_proDrop$language_family))
contrasts(df_estimates_proDrop$language)=named.contr.sum(levels(df_estimates_proDrop$language))
contrasts(df_estimates_proDrop$subjectOrNot)=named.contr.sum(levels(df_estimates_proDrop$subjectOrNot))
contrasts(df_estimates_proDrop$manipulation)=named.contr.sum(levels(df_estimates_proDrop$manipulation))
contrasts(df_estimates_proDrop$pronoun_type)=named.contr.sum(levels(df_estimates_proDrop$pronoun_type))



### Loading the three models
nested <- brms::brm(data = df_estimates_mostReduced,
                    family = gaussian,
                    formula = estimate | se(error) ~ 1 + (1 | articleID/sampleID), 
                    iter = iters,
                    chains = chns,
                    file='model_1',
                    save_pars = save_pars(all = TRUE),
                    control=list(adapt_delta=0.999)
)


nested_all_int2c <- brms::brm(data = df_estimates_mostReduced,
                             family = gaussian,
                             formula = estimate | se(error) ~ 1 + (1 | articleID/sampleID) + language_family + subjectOrNot + manipulation + 
                               language_family:subjectOrNot + manipulation:subjectOrNot , 
                             iter = iters ,
                             chains = chns,
                             save_pars = save_pars(all = TRUE),
                             file='model_2',
                             control=list(adapt_delta=0.99)
)

pro_drop <- brms::brm(data = df_estimates_proDrop,
                      family = gaussian,
                      formula = estimate | se(error) ~ 1 + (1 | articleID/sampleID) + 
                        subjectOrNot + pronoun_type + manipulation + language_family +
                        pronoun_type:subjectOrNot + subjectOrNot:manipulation + 
                        language_family:subjectOrNot, 
                      iter = iters,
                      chains = chns,
                      file='model_3',
                      save_pars = save_pars(all = TRUE),
                      control=list(adapt_delta=0.99)
)