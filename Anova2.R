## Checking for packages installation
my_packages <- c("tidyverse", "mice", "VIM", "readr",
                 "dplyr", "stringr", "ggplot2", "stats",
                 "psych", "knitr", "nortest", "car", "zoo")
missing_packages <- my_packages[!(my_packages %in%
                                    installed.packages()[, "Package"])]
if (length(missing_packages)) install.packages(missing_packages)
remove("my_packages")
remove("missing_packages")

## Library initialization
library("tidyverse")
library("mice")
library("VIM")
library("readr")
library("dplyr")
library("stringr")
library("ggplot2")
library("stats")
library("psych")
library("knitr")
library("nortest")
library("car")
library("zoo")

## Reading the dataset
df2 <- read.csv("Dataset\\Intel_CPUs.csv", header = TRUE,
                     stringsAsFactors = FALSE, na.strings = c("", "N/A"))
# View(cpu_data)

df2 <- df2 %>% select("Vertical_Segment",
                     "Recommended_Customer_Price", "Lithography"
)
temp <- df2


## Convert any string data that need to be in integer type.

# Recommended_Customer_Price converting.
recommend_price_cleaning <- function(price_range) {
  if (grepl("-", price_range)) {
    range <- strsplit(price_range, "-")[[1]]
    return((as.double(range[1]) + as.double(range[2])) / 2)
  }
  return(price_range)
}

df2$Recommended_Customer_Price <- gsub("\\$", "", df2$Recommended_Customer_Price) 
df2$Recommended_Customer_Price <- gsub(",", "", df2$Recommended_Customer_Price)

df2$Recommended_Customer_Price <- sapply(df2$Recommended_Customer_Price, recommend_price_cleaning) 
df2$Recommended_Customer_Price <- as.double(df2$Recommended_Customer_Price)

# Lithography converting.
df2 <- df2 %>%
  mutate(Lithography = gsub("nm|\\s", "", Lithography),
         Lithography = as.double(Lithography))

## xóa dữ liệu khuyết, Vertical_Segment ko khuyết
df2 <- df2[!is.na(df2$Recommended_Customer_Price), ]
df2 <- df2[!is.na(df2$Lithography), ]

#đếm số dữ liệu khuyết r in ra
#print(apply(is.na(df2), 2, sum))

#đếm số giá trị của Lithography và số lần xuất hiện
#print(df2 %>% select(Lithography) %>% table())
#print(df2 %>% select(Lithography) %>% table() %>% length())

#đếm số giá trị của Vertical_Segment và số lần xuất hiện
#print(df2 %>% select(Vertical_Segment) %>% table())
#print(df2 %>% select(Vertical_Segment) %>% table() %>% length())


#vẽ biểu đồ Q-Q plot cho Vertical_Segment để xét có tuân theo phân phối chuẩn
#png("qqVer.png")
#qqVer <- ggplot(df2, aes(sample = Recommended_Customer_Price)) +
  #geom_qq() +
  #facet_wrap(~ Vertical_Segment, scales = "free") +
  #theme_minimal()
#qqVer <- qqVer + stat_qq_line(color = "red")
#print(qqVer)
#dev.off()

#vẽ biểu đồ Q-Q plot cho Lithography để xét có tuân theo phân phối chuẩn
#png("qqLi.png")
#qqLi <- ggplot(df2, aes(sample = Recommended_Customer_Price)) +
  #geom_qq() +
  #facet_wrap(~ Lithography, scales = "free") +
  #theme_minimal()
#qqLi <- qqLi + stat_qq_line(color = "red")
#print(qqLi)
#dev.off()

df2$Vertical_Segment <- as.factor(df2$Vertical_Segment)
df2$Lithography <- as.factor(df2$Lithography)

#thực hiện shapiro test (trc khi log)
#res.aov1 <- aov(Recommend_Customer_Price ~ Vertical_Segment * Lithography, data = df2)
#aov_residuals1 <- residuals(object = res.aov1)
#print(shapiro.test(x = aov_residuals1)) 

#Thử sử dụng Log Transformation để đưa dữ liệu về phân phối chuẩn
df2_log <- df2
df2_log$Recommend_Customer_Price_Log <- log(df2_log$Recommended_Customer_Price)

#thực hiện shapiro test (sau khi log)
#res.aov2 <- aov(Recommend_Customer_Price_Log ~ Vertical_Segment * Lithography, data = df2_log)
#aov_residuals2 <- residuals(object = res.aov2)
#print(shapiro.test(x = aov_residuals2))

#Levene test cho biến độc lập định tính để đánh giá tính đồng nhất phương sai
#leve_result <- leveneTest(Recommend_Customer_Price_Log ~ Vertical_Segment, data = df2_log)
#print(leve_result)

#Bartlett test cho biến độc lập định lượng để đánh giá tính đồng nhất phương sai
#bartlett_result <- bartlett.test(Recommend_Customer_Price_Log ~ Lithography, data = df2_log)
#print(bartlett_result)

#kiểm tra outlier bằng phương pháp khoảng cách Cooks
#png("outlier.png")
#mod <- lm(Recommend_Customer_Price_Log ~ Vertical_Segment * Lithography, data = df2_log)
#cooksd <- cooks.distance(mod)
#plot(cooksd, pch = "*", cex = 2, main = "Influential Obs by Cooks distance")
#abline(h = 4*mean(cooksd, na.rm = TRUE), col = "red")
#text(x = 1:length(cooksd) + 1, y = cooksd, 
     #labels = ifelse(cooksd > 4*mean(cooksd, na.rm = TRUE),
                     #names(cooksd), ""), col = "red")
#influential <- cooksd > 4 * mean(cooksd, na.rm = TRUE)
#outliers <- df2[influential, ]
#dev.off()
