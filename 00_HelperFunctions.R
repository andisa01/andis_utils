
###
# Andis data science and visualization helper functions
# Written by A. Z. Andis Arietta
# Version 0.0.2
# Last updated 20230727
###

library(tidyverse)
library(cowplot)

# Environmental setup ====
make_new_dir <-   function(DIR_TO_MAKE){
  if(dir.exists(DIR_TO_MAKE) == FALSE){
    dir.create(DIR_TO_MAKE)
  }else{
    print("Directory exists")
  }
} # This function checks to see if a directory exists, and if not, creates the new directory.

# Data prep ====

xlsx_to_csv <- function(FILEPATH, NEWDIR = NULL){
  if(is.null(NEWDIR)){
    readxl::read_excel(FILEPATH) %>%
      write.csv(paste0(gsub(".xlsx", "", FILEPATH), ".csv"), row.names = FALSE)
  }
  else{
    readxl::read_excel(FILEPATH) %>%
      write.csv(paste0(NEWDIR, "/", paste0(gsub(".xlsx", "", basename(FILEPATH)), ".csv")), row.names = FALSE)
  }
} # This function converts an xlsx file to a csv with the same name in the same directory, unless another new directory is specified

fix_century <- function(x, year = 1968){
  m <- year(x) %% 100
  year(x) <- ifelse(m > year %% 100, 1900+m, 2000+m)
  x
} # This function deal with ambiguity of double digit years (i.e. 1/1/68 is interpreted as 2068 instead of 1968)

# Data exploration ===

findNAs <- function(dataframe){
  dataframe %>% 
    ungroup() %>%
    summarise_all(.funs = function(x){sum(is.na(x))}) %>%
    pivot_longer(everything()) %>%
    filter(value > 0) %>%
    arrange(desc(value)) %>%
    print(n = nrow(.))
} # This function finds columns within a dataframe with NAs and counts the NAs.

# Data visualization ====

theme_set(
  theme_minimal() +
    theme(
      axis.line = element_line(
        size = 1,
        colour = "black",
        linetype = 1
      ),
      panel.grid.major = element_line(size = 0.1),
      panel.grid.minor = element_blank(),
      axis.title.x = element_text(hjust = 0),
      axis.title.y = element_text(hjust = 0),
      legend.position = "none"
    )
) # This alters the default ggplot design

TFA.colors <- c("#091f2c", "#690080", "#a71ada", "#f9c428", "#03a64b", "#9cd91a","#D2D2D7", "#F2F2F2") # Offical T4A colors as of 24 June 2023

TFA.colors_2023 <- c("#091f2c", "#0097a9", "#8c8279", "#cb333b", "#ffD700", "firebrick") # Official T4A colors

vizNAs <- function(dataframe, grouping_var){
  dataframe %>% 
    group_by({{grouping_var}}) %>%
    summarise_all(.funs = function(x){sum(is.na(x))}) %>%
    pivot_longer(cols = -c({{grouping_var}})) %>%
    ggplot(aes(x = fct_reorder(name, value), y = {{grouping_var}}, fill = value)) +
    geom_tile() +
    coord_flip() +
    scale_fill_gradient(low = "grey80", high = "firebrick")
} # This function makes a heatmap of NAs by colname and can group by another variable. NOTE: Eventually I need to make the scale limits into a percentage base on the size of the dataset conditioned on the grouping variable.

# Modeling ====

balance.dataset <- function(SET, MINORCLASS){
  minor.class <- nrow(SET %>% ungroup() %>% filter(Outcome == MINORCLASS))
  major.class <- nrow(SET) - minor.class
  
  if(minor.class*.5*3*2 < (minor.class + major.class)){
    SET %>% ungroup() %>%
      filter(Outcome == MINORCLASS) %>%
      bind_rows(slice_sample(SET %>% ungroup() %>%
                               filter(Outcome == MINORCLASS), 
                             prop = 0.5, replace = TRUE)) %>%
      bind_rows(SET %>% ungroup() %>%
                  filter(Outcome != MINORCLASS) %>%
                  slice_sample(n = round(minor.class*1.5, 0)))
  } else if (minor.class*.5*3*2 >= (minor.class + major.class)){
    SET %>% ungroup() %>%
      filter(Outcome == MINORCLASS) %>%
      bind_rows(slice_sample(SET %>% ungroup() %>%
                               filter(Outcome == MINORCLASS), 
                             n = (major.class - minor.class)/2, replace = TRUE)) %>%
      bind_rows(SET %>% ungroup() %>%
                  filter(Outcome != MINORCLASS) %>%
                  slice_sample(n = round(minor.class + ((major.class - minor.class)/2), 0)))
  }
} # This function is used to balance the dataset up to 1.5x the minor class.

mtry.param.test <- function(DATA, MIN_MTRY = 1, MAX_MTRY = ncol(DATA) - 1){
  Mtry.parameter.test <- c()
  T01 <- Sys.time()
  for(i in MIN_MTRY:MAX_MTRY){
    set.seed(874)
    T0 <- Sys.time()
    E.Rate <- randomForest(Outcome ~ .,
                           data = DATA,
                           importance = TRUE,
                           ntree = 500,
                           mtry = i)$err.rate %>% mean()
    Mtry.parameter.test <- bind_rows(Mtry.parameter.test,
                                     data.frame(Error.rate = E.Rate,
                                                Mtry = i))
    print(paste0("Itteration ", i, " completed in ", Sys.time() - T0))
  }
  Sys.time() - T01
  beep()
  
  Mtry.parameter.test %>%
    ggplot(aes(x = Mtry, y = Error.rate)) +
    geom_point() +
    geom_line() +
    geom_label(aes(label = Mtry), vjust = -1)
} # This function loops through mtry hyperparameter in a random forest and outputs a graph with the minimum error

RF_Var_Imp_plot <- function(RF.mod, title = NULL){
  plot_grid(
    ggdraw() + 
      draw_label(title),
    plot_grid(
      RF.mod$importance %>% 
        as.data.frame() %>% 
        mutate(Var = rownames(.),
               impsd = RF.mod$importanceSD[,3]) %>%
        mutate(MeanDecreaseAccuracy = MeanDecreaseAccuracy/impsd) %>%
        select(Var, 
               Accuracy = MeanDecreaseAccuracy, 
               Gini = MeanDecreaseGini) %>%
        filter(!is.na(Accuracy)) %>%
        ggplot(aes(x = fct_reorder(Var, Accuracy), y = Accuracy)) +
        geom_bar(aes(fill = ifelse(grepl("^noise", Var), "firebrick", "grey50")), stat = "identity") +
        labs(x = "")+
        coord_flip() +
        scale_fill_identity(),
      
      RF.mod$importance %>% 
        as.data.frame() %>% 
        mutate(Var = rownames(.),
               impsd = RF.mod$importanceSD[,3]) %>%
        mutate(MeanDecreaseAccuracy = MeanDecreaseAccuracy/impsd) %>%
        select(Var, 
               Accuracy = MeanDecreaseAccuracy, 
               Gini = MeanDecreaseGini) %>%
        filter(!is.na(Gini)) %>%
        ggplot(aes(x = fct_reorder(Var, Gini), y = Gini)) +
        geom_bar(aes(fill = ifelse(grepl("^noise", Var), "firebrick", "grey50")), stat = "identity") +
        labs(x = "")+
        coord_flip() +
        scale_fill_identity(),
      
      nrow = 1
    ),
    nrow = 2,
    rel_heights = c(0.1, 1)
  )
} # This function creates side-by-side plots of importance metrics (accuracy and node impurity) from random forest classifier.

RF_Prob_Histogram <- function(DATAFRAME, MODEL, MODEL_TITLE = NULL){
  
  if(is.null(MODEL_TITLE)){
    MODEL_TITLE <- deparse(substitute(MODEL))
  }
  DATAFRAME %>%
    mutate(fit = predict(MODEL, type = "prob")[,"Y"]) %>%
    ggplot(aes(x = fit, col = Outcome, fill = Outcome)) +
    geom_histogram(alpha = 0.4, position = "identity", bins = 50) +
    scale_fill_manual(values = c(TFA.colors[4], TFA.colors[1])) +
    scale_color_manual(values = c(TFA.colors[4], TFA.colors[1])) +
    theme(
      legend.position = c(.9, .9),
      legend.justification = c("right", "top"),
      legend.box.just = "right"
    ) +
    labs(x = "Probability of exit",
         title = MODEL_TITLE,
         color = "Exited",
         fill = "Exited")
} # This function creates a histogram of the distribution of model probabilities with observed positive and negative cases overlaid.

GlimnetSummaryTable <- function(COEFS){
  COEFS %>% summary() %>% as.data.frame()
  data.frame("Variables" = COEFS@Dimnames[[1]]) %>%
    mutate(i = 1:nrow(.)) %>%
    left_join(COEFS %>% summary() %>% as.data.frame(), by = "i") %>%
    rename(Coefs = x) %>%
    select(Variables, Coefs)
} # Output a summary table of the elastic net/LASSO coefficients from glmnet
