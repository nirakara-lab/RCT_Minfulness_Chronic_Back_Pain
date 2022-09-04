We load the packages we will need…

# Preprocessing

All the preprocessing as well as the regression computation was done in
a jupyter notebook in python. Next, dataframes are declared with the
data already cleaned (and imputed).

    ## New names:New names:New names:New names:

# General EDA

I make a list of variables that allows me to iterate the functions. To
do this, I choose the first 57 variables of the long-format dataframe,
excluding demographic variables or variables that only correspond to the
experimental group, i.e. those that are specific to the performance of
the programme.

``` r
df <- Df_long
df$...1 <- NULL
df$index <- NULL #deleting index
colnames(df)[36] = 'MAIA_Total' #for some reason MAIA_Total was not correct, it was written MAIA_global.
colnames(Df_long)[38] = 'MAIA_Total' #for some reason MAIA_Total was not correct in Df_long either, it was written MAIA_global.
variables_temp <- names(df)
variables <- variables_temp[1:60]
df[variables] <- lapply(df[variables], as.numeric)

colnames(df)
```

    ##  [1] "Cortisol"                 "IL_1"                    
    ##  [3] "IL_6"                     "IL_17"                   
    ##  [5] "TNF"                      "PSQI_calidad"            
    ##  [7] "PSQI_latencia"            "PSQI_duracion"           
    ##  [9] "PSQI_eficiencia"          "PSQI_perturbaciones"     
    ## [11] "PSQI_medicacion"          "PSQI_disfuncion_diurna"  
    ## [13] "PSQI_Total"               "RMQ_Total"               
    ## [15] "OQ_Total"                 "FFMQ_ACT"                
    ## [17] "FFMQ_NONREACT"            "FFMQ_NONJ"               
    ## [19] "FFMQ_OBS"                 "FFMQ_DES"                
    ## [21] "FFMQ_Total"               "SCS_self_kindness"       
    ## [23] "SCS_self_judgment"        "SCS_common_humanity"     
    ## [25] "SCS_isolation"            "SCS_mindfulness"         
    ## [27] "SCS_over_identified"      "MAIA_Noticing"           
    ## [29] "MAIA_NotDistracting"      "MAIA_NotWorrying"        
    ## [31] "MAIA_AttRegulation"       "MAIA_EmRegulation"       
    ## [33] "MAIA_SelfRegulation"      "MAIA_BodyListening"      
    ## [35] "MAIA_Trusting"            "MAIA_Total"              
    ## [37] "DASS_Depression"          "DASS_Anxiety"            
    ## [39] "DASS_Stress"              "DASS_Total"              
    ## [41] "SF_physical_functioning"  "SF_limitations_physical" 
    ## [43] "SF_limitations_emotional" "SF_energy"               
    ## [45] "SF_emotional_wellbeing"   "SF_social_functioning"   
    ## [47] "SF_pain"                  "SF_general_health"       
    ## [49] "TSK_Total"                "FABQ_work"               
    ## [51] "FABQ_physical_activity"   "PCS_Rumination"          
    ## [53] "PCS_Magnification"        "PCS_Helplessness"        
    ## [55] "PCS_Total"                "CPAQ_activity_engagement"
    ## [57] "CPAQ_pain_willingness"    "CPAQ_Total"              
    ## [59] "SWLS_Total"               "WHO_Total"               
    ## [61] "SF_21"                    "SF_22"                   
    ## [63] "VAS_rasgo_1"              "VAS_estado_1"            
    ## [65] "Ciudad"                   "ID"                      
    ## [67] "Grupo"                    "Genero"                  
    ## [69] "edad"                     "estudios"                
    ## [71] "email"                    "Time"                    
    ## [73] "tiempo_total_practica"    "valoracion_profesor"

A dataframe can be made that encompasses the most elementary statistics
and a graph that helps us to understand the behaviour of the variables.

``` r
summary <- df %>%
group_by(Time, Grupo) %>% get_summary_stats(type = "mean_sd")
summary_df <- data.frame(summary, stringsAsFactors = TRUE)
cols <- c("Grupo", "Time", "variable")
summary_df[cols] <- lapply(summary_df[cols], as.factor)  ## cambio esas variables as factor
write_xlsx(summary_df,"outcomes/sumario_estadistico_basico_dropna.xlsx") # lo escribo
```

Now I develop a graph of the normalised dataframe. First I develop it
with barplots:

``` r
normalize <- function(x) {
return ((x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
}
df_norm <- df
df_norm <- df_norm %>% drop_na(PSQI_calidad)
df_norm <- df_norm %>% drop_na(Cortisol)
df_norm[,1:62] <-lapply(df_norm[,1:62],normalize)
```

``` r
summary_norm <- df_norm %>%
group_by(Time, Grupo) %>% get_summary_stats(type = "mean_sd")
summary_df_norm <- data.frame(summary_norm, stringsAsFactors = TRUE)
cols <- c("Grupo", "Time", "variable")
summary_df_norm[cols] <- lapply(summary_df_norm[cols], as.factor)
summary_df_norm <- subset(summary_df_norm, variable !="edad" & 
                            variable != "valoracion_profesor" &
                            variable != "tiempo_total_practica" &
                            variable != "estudios" &
                            variable != 'email' &
                            variable != 'Genero' &
                            variable != 'Ciudad' &
                            variable != 'VAS_rasgo_1' &
                            variable != 'VAS_estado_1' &
                            variable != 'ID')

summary_df_norm$Time <- factor(summary_df_norm$Time, levels = c("Post", "Pre"))
summary_df_norm$description <- "error"
# incluyo una columna en summary que tenga la descripción corta de 
# las escalas
for(i in 1:length(summary_df_norm$variable)){
  for(j in 1:length(des$variable)){
    if(summary_df_norm$variable[i] == des$variable[j]){
       summary_df_norm$description[i] <- des$description[j]
  } 
 }
}
```

![](notebook4publication_files/figure-markdown_github/unnamed-chunk-7-1.png)
And now with boxplots:

``` r
df_toolarge <- df_norm
df_toolarge <- select( df_toolarge,- c("valoracion_profesor", "tiempo_total_practica", "estudios", 'VAS_rasgo_1', 'VAS_estado_1' ))
df_toolarge <- melt(df_toolarge, id.vars = c("Grupo", "Genero", "email",
                                             "Time","Ciudad", "ID", "edad"))
df_toolarge$description <- "error"
# incluyo una columna en summary que tenga la descripción corta de 
# las escalas
for(i in 1:length(df_toolarge$variable)){
  for(j in 1:length(des$variable)){
    if(df_toolarge$variable[i] == des$variable[j]){
      #print(paste(df_toolarge$variable[i],des$description[j])) 
      df_toolarge$description[i] <- des$description[j]
  } 
 }
}
```

![](notebook4publication_files/figure-markdown_github/unnamed-chunk-9-1.png)

# Outliers

I make a database with all outliers.

``` r
outliers <- data.frame(matrix(ncol = 6, nrow = 0))
x <- c("Grupo", "Time", "value", 
       "is.outlier", "is.extreme", 
       "variable" )
colnames(outliers) <- x
for (i in variables)
  {
  outliersxvariable <- df %>% select(Grupo, Time, i) %>% group_by(Time, Grupo) %>% identify_outliers(i)
  outliersxvariable_df <- data.frame(outliersxvariable, stringsAsFactors = TRUE)
  variable<-i
  if (nrow(outliersxvariable_df) == 0){print(paste(des$description[des$variable == variable], ' no tiene outliers'))}else{
    outliersxvariable_df$variable = variable
    names(outliersxvariable_df)[names(outliersxvariable_df) == i] <- "value"
    outliers <- rbind(outliers, outliersxvariable_df)
} 
  }
```

    ## [1] "PSQI Subjective sleep quality  no tiene outliers"
    ## [1] "PSQI  Sleep latency  no tiene outliers"
    ## [1] "PSQI Sleep duration  no tiene outliers"
    ## [1] "PSQI Sleep efficiency  no tiene outliers"
    ## [1] "PSQI Sleep disturbance  no tiene outliers"
    ## [1] "PSQI Use of sleep medication  no tiene outliers"
    ## [1] "FFMQ Observation  no tiene outliers"
    ## [1] "SCS Self-Judgment  no tiene outliers"
    ## [1] "SCS Isolation  no tiene outliers"
    ## [1] "SCS Mindfulness  no tiene outliers"
    ## [1] "SCS Over-Identification  no tiene outliers"
    ## [1] "MAIA Body Listening  no tiene outliers"
    ## [1] "SF-36 Role limitations due to physical health  no tiene outliers"
    ## [1] "TSK  no tiene outliers"
    ## [1] "FABQ Work  no tiene outliers"
    ## [1] "PCS Rumination  no tiene outliers"
    ## [1] "SWLS  no tiene outliers"

# Distribution

I make a dataframe in which the shapiro test is iterated for each
variable. Variables that pass the gaussian test, in the normal column
are labelled as “Gaussian”, however those that do not pass are labelled
as “Non-gaussian”.

``` r
normalidad <- data.frame(matrix(ncol = 6, nrow = 0))
x <- c("Grupo", "Time", "variable", 
       "statistic", "p", 
       "normal" )
colnames(normalidad) <- x
for (i in variables){
  shapiro <- df %>%
  group_by(Time, Grupo) %>% shapiro_test(i)
  shapiro_df <- data_frame(shapiro)
  vector <- c()
  for (pvalue in shapiro_df$p){
  if (pvalue > 0.05){vector <- c(vector, "normal")}
    else{vector <- c(vector, "No normal")}
  }
  shapiro_df$normal <- vector
  normalidad <- rbind(normalidad, shapiro_df)
  
}
```

``` r
plot <- ggplot(normalidad, aes(x = normal)) + 
  geom_bar( aes(color = Time, fill = Time), stat = "count", 
            position = position_dodge(0.8), width = 0.7) + 
  facet_wrap(~Grupo,  ncol=1) +  
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=.5,colour='gray50')) +
  font("x.text", size = 10, vjust = 0.5)+ theme_light()
plot
```

![](notebook4publication_files/figure-markdown_github/unnamed-chunk-12-1.png)

# Homogeneity of variance

The homogeneity of variance of the factor (Group) between subjects can
be tested using Levene’s test. The test is performed at each level of
the variable Temporal:

We make a print of the **non-homogeneous**.

``` r
for (i in 1:length(homogeneidad$homogeneidad)){
  if(homogeneidad$homogeneidad[i] == "No homogeneo"){
    print(paste(des$description[des$variable==homogeneidad$variable[i]],"en",homogeneidad$Time[i], 
                "es",homogeneidad$homogeneidad[i]))}
  
}
```

    ## [1] "FFMQ Non-reactivity en Post es No homogeneo"
    ## [1] "DASS Depression en Post es No homogeneo"
    ## [1] "DASS Anxiety en Post es No homogeneo"
    ## [1] "SF-36 Pain en Pre es No homogeneo"
    ## [1] "SF-36 General health en Pre es No homogeneo"
    ## [1] "SWLS en Post es No homogeneo"

## Homogeneity of covariances

The homogeneity of factor (group) covariances across subjects can be
assessed using Box’s M-test implemented in the rstatix package. If this
test is statistically significant (i.e. p \< 0.001), it does not have
equal covariances, but if the test is not statistically significant
(i.e. p \> 0.001), it has equal covariances and has not violated the
assumption of homogeneity of covariances. It is highly sensitive, so
unless p \< 0.001 and the sample sizes are unequal, ignore it. However,
if it is significant and has unequal sample sizes, the test is not
robust (<https://en.wikiversity.org/wiki/Box%27s_M>, Tabachnick &
Fidell, 2001).

Translated with www.DeepL.com/Translator (free version)

I make a plot for those variables that do not meet the box test.

``` r
for (i in 1:length(homogeneidad_cova$box)){
  if(homogeneidad_cova$box[i] == "No homogeneo covarianzas") 
    print(paste(homogeneidad_cova$variable[i], 
                "es",homogeneidad_cova$box[i]))
  
}
```

# Sphericity

The assumption of sphericity will be automatically checked during the
calculation of the ANOVA test using the R anova_test() function (rstatix
package). The Mauchly test is used internally to evaluate the sphericity
assumption. Using the get_anova_table() function \[rstatix\] to extract
the ANOVA table, the Greenhouse-Geisser sphericity correction is
automatically applied to factors that violate the sphericity assumption.

I compute a dataframe with all mixed ANOVA tests.

## función cutoff

Función que me permite hacer un cutoff por encima de cierto valor, sólo
para valores pre

``` r
cutoff <- function(df, columna, cutoff){
  # Recorta aquellos ID que presentan un valor mayor que cutoff en el pre
  ID_drop <- c() # genero una lista vacía donde se guardarán los ID malditos
  `%notin%` <- Negate(`%in%`)
  for (i in seq_along(df)){ 
    if (is.na(df[i,columna])){}
    else if (df$Time[i] == 'Pre' && df[i, columna] > cutoff){ID_drop <- c(ID_drop, df$ID[i])}
    else{}
  }
  return(subset(df, ID %notin% ID_drop))
}
```

# Robust ANOVA tests:

``` r
anova_df_robust <- data.frame(matrix(ncol = 6, nrow = 0))
x <- c('effect', "value", "p.value", 
       "variable", "significance", "effect_size" )
colnames(anova_df_robust) <- x
for (i in variables){
  a <- select(df, i, Time, Grupo, ID)
  formula = as.formula(paste(i,"~","Grupo * Time"))
  x <- bwtrim(formula = formula, id = ID, data = Df_long)
  value <- c(x$Qa,x$Qb,x$Qab)
  p.value <- c(x$A.p.value,x$B.p.value,x$AB.p.value)
  anova_df_i <- data.frame("effect" = c("Grupo", "Time", "Grupo:Time"),
                         "value" = value,
                         "p.value" = p.value)
  anova_df_i$variable <- des$description[des$variable==i]
  anova_df_i$significance <- "no"
  anova_df_i$effect_size<-akp.effect(formula = formula, data = Df_long)
  for(j in 1:length(anova_df_i$p.value)){
    if(anova_df_i$p.value[j]< 0.05){anova_df_i$significance[j] = "*"}
  }
  anova_df_robust <- rbind(anova_df_robust, anova_df_i)
}
write_xlsx(anova_df_robust, "analisis_globales/robust_anova_dropna.xlsx")
```

## Pooling ANOVA results

Algorithm to perform robust tests or mixed anova according to the
normality of the variable.

``` r
ANOVA_tratado_df <- data.frame(matrix(ncol = 6, nrow = 0))
x <- c("Effect", "Estadístico", "p", "p<.05", "variable", "Method" )
colnames(ANOVA_tratado_df) <- x



for (i in variables){
  #shapiro <- df %>%
  #group_by(Time, Grupo) %>% shapiro_test(i) #hago el test de normalidad
  des_i <- subset(des, variable == i)
  if (des_i$Statistical_method == "Mixed ANOVA"){
    #ANOVA MIXTA
  a <- select(df, i, Time, Grupo, ID)
  res.aov <- anova_test(data = a, dv = i, 
                        wid = ID,  between = Grupo, within = Time)
  x <- get_anova_table(res.aov)
  anova_df_i <- as.data.frame(x)
  keeps <- c("Effect", "F", "p")
  anova_df_i <- anova_df_i[keeps]
  col_names <- c("Effect","Estadístico", "p")
  colnames(anova_df_i) <- col_names
  anova_df_i$significance <- cut(anova_df_i$p,
                               breaks = c(-Inf, 0.0001, 0.001, 0.01, 0.05, Inf),
                               labels = c("****", "***", "**", "*", "ns"))
  anova_df_i$variable <- des$description[des$variable==i]
  anova_df_i$method <- "Mixed ANOVA"
  ANOVA_tratado_df <- rbind(ANOVA_tratado_df, anova_df_i)
  }
    else{
  a <- select(df, i, Time, Grupo, ID)
  formula = as.formula(paste(i,"~","Time * Grupo"))
  x <- bwtrim(formula = formula, id = ID, data = a, tr = 0)
  value <- c(x$Qa,x$Qb,x$Qab)
  p.value <- c(x$A.p.value,x$B.p.value,x$AB.p.value)
  anova_df_i <- data.frame("Effect" = c("Time", "Grupo", "Time:Grupo"),
                         "Estadístico" = value,
                         "p" = p.value)
  anova_df_i$significance <- cut(anova_df_i$p,
                               breaks = c(-Inf, 0.0001, 0.001, 0.01, 0.05, Inf),
                               labels = c("****", "***", "**", "*", "ns"))
  anova_df_i$variable <- des$description[des$variable==i]
  anova_df_i$method <- "Robust Mixed ANOVA"
  ANOVA_tratado_df <- rbind(ANOVA_tratado_df, anova_df_i)
      
    }
  
}


write_xlsx(ANOVA_tratado_df, "analisis_globales/ANOVA_tratado_df.xlsx")
```

# Post-Hoc Tests

As there are 2 Groups and 2 Times, we can do a double test: the first,
for each time, do an ANOVA between Groups and the second, do for each
group, an ANOVA for each time. It will be corrected in each case using
Bonferroni .If the distributions do not meet the requirements we will
use Dunne’s test and Wilcoxon alternatively.

``` r
posthoc <- data.frame(matrix(ncol = 11, nrow = 0))
x <- c("Level", "Effect", "DFn", 
       "DFd", "F", "p", "p<.05", "ges", "p.adj", "variable", "method" )

colnames(posthoc) <- x

for (i in variables){
  des_i <- subset(des, variable==i)
  if (des_i$Statistical_method == "Mixed ANOVA"){
  
  a <- select(df, ID, Time, Grupo, i)
  one.way <- a %>%
  group_by(Time) %>%
  anova_test(dv = i, wid = ID, between = Grupo) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
  x <- one.way
  d_grupo <- as.data.frame(x)
  d_grupo <- rename(d_grupo, c("Level"="Time"))
  d_grupo$variable <- des$description[des$variable==i]
  d_grupo$method <- "Anova"
  d_grupo['p<.05'] <- cut(d_grupo$p.adj, breaks = c(-Inf, 0.0001, 0.001, 0.01, 0.05, Inf),labels = c("****", "***", "**", "*", "ns"))
  posthoc <- rbind(posthoc, d_grupo)
  one.way2 <- a %>%
  group_by(Grupo) %>%
  anova_test(dv = i, wid = ID, within = Time) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
  x <- one.way2
  d_time <- as.data.frame(x)
  d_time <- rename(d_time, c("Level"="Grupo"))
  d_time$variable <- des$description[des$variable==i]
  d_time$method <- "Anova"
  d_time['p<.05'] <- cut(d_time$p.adj, breaks = c(-Inf, 0.0001, 0.001, 0.01, 0.05, Inf),labels = c("****", "***", "**", "*", "ns"))
  posthoc <- rbind(posthoc, d_time)
  }
  else{
    
  a <- select(df, ID, Time, Grupo, i)
  ## dunn test for comparison between groups
  formula = as.formula(paste(i,"~","Grupo"))
  dn <- a %>%
  group_by(Time) %>%
  dunn_test(formula, p.adjust.method = "bonferroni")
  colnames(dn) <- c('Level', 'variable', 'b1', 'b2', 'b3', 'b4', "F", 'p', 'p.adj', 'p<.05' ) # rename de columnas
  dn <- select(dn, 'Level', 'variable',"F", 'p', 'p.adj', 'p<.05' )
  dn$Effect = "Grupo"
  dn$DFn = 'x'
  dn$DFd = 'x'
  dn$method = 'Dunn’s test'
  dn$ges = 'x'
  posthoc <- rbind(posthoc, dn)
  
  #and wilxon for time
  formula = as.formula(paste(i,"~","Time"))
  wx <- a %>% 
  group_by(Grupo) %>% 
  wilcox_test(formula, paired = TRUE, p.adjust.method = "bonferroni")
  colnames(wx) <- c('Level', 'variable', 'b1', 'b2', 'b3', 'b4', "F",  'p.adj' ) # rename de columnas
  wx <- select(wx,'Level', 'variable', "F",  'p.adj')
  wx$Effect = "Time"
  wx$DFn = 'x'
  wx$DFd = 'x'
  wx$method = 'Wilcoxon'
  wx$ges = 'x'
  wx$p = wx$p.adj
  wx['p<.05'] <- cut(wx$p.adj,breaks = c(-Inf, 0.0001, 0.001, 0.01, 0.05, Inf),labels = c("****", "***", "**", "*", "ns"))
  posthoc <- rbind(posthoc, wx) 
  
  }
  }
```
