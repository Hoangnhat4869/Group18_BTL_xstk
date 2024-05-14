df <- data.frame(
   Lithography=cpu_data$Lithography,
   Recommend_Customer_Price=cpu_data$Recommended_Customer_Price,
)

hist(log(df$Recommend_Customer_Price), main = "Recommended_Customer_Price", xlab = "Recommended_Customer_Price", col="blue2", labels=TRUE)
df %>% select(Lithography) %>% is.na() %>% sum
#[1] 10

#Execute_Disable_Bit
#    Missing          No         Yes 
#  0.043043812 0.005380477 0.951575711

# Remove all NA
df <- df[!is.na(df$Lithography), ]

df %>% select(Lithography) %>% is.na() %>% sum
#[1] 0

df %>% select(Lithography) %>% table()
#Lithography
# 14  22  32  45  65  90 130 
#407 481 211 140  27  15  10

df %>% select(Lithography) %>% table() %>% length()
#[1] 7


#-----
# Check the normal distribution of RCP

df$Lithography <- as.factor(df$Lithography)
library(ggpubr)
png("images/normal_distribution_for_each_group.png")
ggqqplot(df, "Recommend_Customer_Price", facet.by = "Lithography")
dev.off()


df %>% group_by(Lithography) %>% shapiro_test(Recommend_Customer_Price)
# # A tibble: 7 × 4
#   Lithography variable                 statistic        p
#   <fct>       <chr>                        <dbl>    <dbl>
# 1 14          Recommend_Customer_Price     0.581 3.91e-30
# 2 22          Recommend_Customer_Price     0.534 1.02e-33
# 3 32          Recommend_Customer_Price     0.711 7.23e-19
# 4 45          Recommend_Customer_Price     0.648 7.66e-17
# 5 65          Recommend_Customer_Price     0.750 2.12e- 5
# 6 90          Recommend_Customer_Price     0.908 1.25e- 1
# 7 130         Recommend_Customer_Price     0.886 1.52e- 1

df_log <- df
df_log$Recommend_Customer_Price <- log(df_log$Recommend_Customer_Price)
df_log %>% group_by(Lithography) %>% shapiro_test(Recommend_Customer_Price)

png("images/normal_distribution_for_each_group_after_log.png")
ggqqplot(df_log, "Recommend_Customer_Price", facet.by = "Lithography")
dev.off()

library(rstatix)
df %>% group_by(Lithography) %>% select(Recommend_Customer_Price) %>% head()

# Build the linear model
model  <- lm(Recommend_Customer_Price ~ Lithography, data = df_log)
# Create a QQ plot of residuals
ggqqplot(residuals(model))
shapiro_test(residuals(model))


Recommend_Customer_Price   <- df$Recommend_Customer_Price
Lithography                <- as.factor(df$Lithography) #group

Recommend_Customer_Price_Log   <- log(df$Recommend_Customer_Price)
png("Lithography_Price_Boxplot.png")
boxplot(Recommend_Customer_Price_Log ~ Lithography, col="blue")
dev.off()

png("qqnorm.png")
qqnorm(Recommend_Customer_Price_Log)
qqline(Recommend_Customer_Price_Log)
dev.off()

ad.test(Recommend_Customer_Price)
#         Anderson-Darling normality test

# data:  Recommend_Customer_Price
# A = 193.65, p-value < 2.2e-16

shapiro.test(Recommend_Customer_Price)
#         Shapiro-Wilk normality test

# data:  Recommend_Customer_Price
# W = 0.56555, p-value < 2.2e-16

# ----------- Anova 

av <- aov(Recommend_Customer_Price_Log ~ Lithography)
summary(av)
#                          Df Sum Sq Mean Sq F value   Pr(>F)    
# as.factor(Lithography)    6  114.4  19.066   11.74 7.64e-13 ***
# Residuals              1284 2085.4   1.624                     
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

tk <- TukeyHSD(av)

#   Tukey multiple comparisons of means
#     95% family-wise confidence level

# Fit: aov(formula = Recommend_Customer_Price_Log ~ as.factor(Lithography))

# $`as.factor(Lithography)`
#                diff         lwr          upr     p adj
# 22-14  -0.387636364 -0.64110129 -0.134171442 0.0001395
# 32-14  -0.318239062 -0.63749349  0.001015365 0.0513599
# 45-14  -0.788568380 -1.15730270 -0.419834058 0.0000000
# 65-14  -0.315928455 -1.06383426  0.431977346 0.8753154
# 90-14   0.782948098 -0.20650341  1.772399607 0.2272769
# 130-14  1.254544878  0.04991967  2.459170085 0.0348924
# 32-22   0.069397302 -0.24135933  0.380153934 0.9946533
# 45-22  -0.400932016 -0.76233386 -0.039530169 0.0185919
# 65-22   0.071707909 -0.67261016  0.816025974 0.9999569
# 90-22   1.170584462  0.18384205  2.157326872 0.0086049
# 130-22  1.642181242  0.43978023  2.844582250 0.0011399
# 45-32  -0.470329318 -0.88056123 -0.060097410 0.0129168
# 65-32   0.002310607 -0.76690218  0.771523390 1.0000000
# 90-32   1.101187160  0.09553339  2.106840931 0.0213286
# 130-32  1.572783940  0.35481551  2.790752375 0.0027359
# 65-45   0.472639925 -0.31839099  1.263670845 0.5724979
# 90-45   1.571516478  0.54907767  2.593955287 0.0001257
# 130-45  2.043113258  0.81124934  3.274977178 0.0000225
# 90-65   1.098876553 -0.11305546  2.310808566 0.1047813
# 130-65  1.570473333  0.17731620  2.963630469 0.0156251
# 130-90  0.471596780 -1.06480747  2.008001026 0.9716127
png("Tukey.png")
plot(tk, ordered=T)
dev.off()


# bartlett.test(Recommend_Customer_Price_Log ~ Lithography)
# > bartlett.test(Recommend_Customer_Price_Log ~ Lithography)

#         Bartlett test of homogeneity of variances

# data:  Recommend_Customer_Price_Log by Lithography
# Bartlett's K-squared = 19.947, df = 6, p-value = 0.002831


pairwise.t.test(Recommend_Customer_Price_Log, Lithography, p.adjust="bonferroni", pool.sd = T)
#         Pairwise comparisons using t tests with pooled SD 

# data:  Recommend_Customer_Price_Log and Lithography 

#     14      22      32      45      65      90     
# 22  0.00014 -       -       -       -       -      
# 32  0.06935 1.00000 -       -       -       -      
# 45  7.8e-09 0.02270 0.01536 -       -       -      
# 65  1.00000 1.00000 1.00000 1.00000 -       -      
# 90  0.41176 0.00999 0.02633 0.00013 0.15773 -      
# 130 0.04508 0.00122 0.00302 2.3e-05 0.01883 1.00000

# P value adjustment method: bonferroni

pairwise.t.test(Recommend_Customer_Price_Log, Lithography, p.adjust="BH", pool.sd = T)

#         Pairwise comparisons using t tests with pooled SD 

# data:  Recommend_Customer_Price_Log and Lithography 

#     14      22      32      45      65      90     
# 22  3.6e-05 -       -       -       -       -      
# 32  0.00533 0.56337 -       -       -       -      
# 45  7.8e-09 0.00227 0.00192 -       -       -      
# 65  0.26247 0.81488 0.99292 0.10224 -       -      
# 90  0.02745 0.00143 0.00239 3.6e-05 0.01127 -      
# 130 0.00376 0.00024 0.00050 1.1e-05 0.00209 0.42570

# P value adjustment method: BH 

library(agricolae)


print(LSD.test(av, "Lithography"))
# $statistics
#    MSerror   Df     Mean       CV
#   1.624169 1284 5.862757 21.73771

# $parameters
#         test p.ajusted      name.t ntr alpha
#   Fisher-LSD      none Lithography   7  0.05

# $means
#     Recommend_Customer_Price_Log       std   r         se      LCL      UCL
# 130                     7.387047 0.7826265  10 0.40300979 6.596417 8.177677
# 14                      6.132502 1.2895756 407 0.06317109 6.008572 6.256432
# 22                      5.744866 1.2467624 481 0.05810895 5.630867 5.858865
# 32                      5.814263 1.4437459 211 0.08773530 5.642143 5.986383
# 45                      5.343934 1.0492029 140 0.10770890 5.132629 5.555239
# 65                      5.816574 1.3240166  27 0.24526395 5.335412 6.297736
# 90                      6.915450 1.2621507  15 0.32905611 6.269904 7.560997
#          Min      Max      Q25      Q50      Q75
# 130 5.726848 8.213924 7.019980 7.556897 8.036517
# 14  3.044522 9.473550 5.385641 5.917549 7.074270
# 22  2.833213 8.878219 4.927254 5.638355 6.345636
# 32  2.263844 8.444622 4.787492 5.686975 6.997757
# 45  2.944439 7.950502 4.417940 5.513421 5.897154
# 65  3.610918 8.252707 4.762283 5.755742 6.906755
# 90  4.304065 8.213924 6.296361 7.347300 7.824115

# $comparison
# NULL

# $groups
#     Recommend_Customer_Price_Log groups
# 130                     7.387047      a
# 90                      6.915450      a
# 14                      6.132502      b
# 65                      5.816574     bc
# 32                      5.814263      c
# 22                      5.744866      c
# 45                      5.343934      c

# attr(,"class")
# [1] "group"
# > print(LSD.test(av, "Lithography"))
# $statistics
#    MSerror   Df     Mean       CV
#   1.624169 1284 5.862757 21.73771

# $parameters
#         test p.ajusted      name.t ntr alpha
#   Fisher-LSD      none Lithography   7  0.05

# $means
#     Recommend_Customer_Price_Log       std   r         se      LCL      UCL
# 130                     7.387047 0.7826265  10 0.40300979 6.596417 8.177677
# 14                      6.132502 1.2895756 407 0.06317109 6.008572 6.256432
# 22                      5.744866 1.2467624 481 0.05810895 5.630867 5.858865
# 32                      5.814263 1.4437459 211 0.08773530 5.642143 5.986383
# 45                      5.343934 1.0492029 140 0.10770890 5.132629 5.555239
# 65                      5.816574 1.3240166  27 0.24526395 5.335412 6.297736
# 90                      6.915450 1.2621507  15 0.32905611 6.269904 7.560997
#          Min      Max      Q25      Q50      Q75
# 130 5.726848 8.213924 7.019980 7.556897 8.036517
# 14  3.044522 9.473550 5.385641 5.917549 7.074270
# 22  2.833213 8.878219 4.927254 5.638355 6.345636
# 32  2.263844 8.444622 4.787492 5.686975 6.997757
# 45  2.944439 7.950502 4.417940 5.513421 5.897154
# 65  3.610918 8.252707 4.762283 5.755742 6.906755
# 90  4.304065 8.213924 6.296361 7.347300 7.824115

# $comparison
# NULL

# $groups
#     Recommend_Customer_Price_Log groups
# 130                     7.387047      a
# 90                      6.915450      a
# 14                      6.132502      b
# 65                      5.816574     bc
# 32                      5.814263      c
# 22                      5.744866      c
# 45                      5.343934      c


# The portion of the output that we’re most interested in is the section titled $groups. 
# The types of lithinium that have different characters in the groups column are significantly different.




#-----------------------------------


df2 <- data.frame(
   Recommend_Customer_Price=cpu_data$Recommended_Customer_Price,
   Vertical_Segment=cpu_data$Vertical_Segment
)

df2$Vertical_Segment <- as.factor(df2$Vertical_Segment)
levels(df2$Vertical_Segment)

df2 %>% select(Vertical_Segment) %>% is.na() %>% sum
ggqqplot(df2, "Recommend_Customer_Price", facet.by = "Vertical_Segment")

df2 %>% group_by(Vertical_Segment) %>% shapiro_test(Recommend_Customer_Price)


df2_log <- df2
df2_log$Recommend_Customer_Price