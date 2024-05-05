## Created on 2023-03-22

## Checking for packages installation
my_packages <- c("tidyverse", "mice", "VIM", "readr",
                 "dplyr", "stringr", "ggplot2",
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
library("psych")
library("knitr")
library("nortest")
library("car")
library("zoo")

## Reading the dataset
cpu_data <- read.csv("Dataset/Intel_CPUs.csv", header = TRUE,
                     stringsAsFactors = FALSE, na.strings = c("", "N/A"))
# View(cpu_data)

cpu_data <- cpu_data %>% select("Vertical_Segment", "Lithography",
                                "Recommended_Customer_Price", "nb_of_Cores", "nb_of_Threads",
                                "Processor_Base_Frequency", "Cache", "Max_Memory_Size",
                                "Max_Memory_Bandwidth", "Execute_Disable_Bit"
)
temp <- cpu_data

## Plot data pattern
aggr_plot <- aggr(cpu_data, col = c("navyblue", "red"),
                  numbers = TRUE,
                  sortVars = TRUE,
                  labels = names(cpu_data),
                  cex.axis = .7,
                  gap = 3,
                  ylab = c("Histogram of Missing data", "Pattern"),
                  cex.numbers = 0.1
)

## Convert any string data that need to be in integer type.

# Recommended_Customer_Price converting.
recommend_price_cleaning <- function(price_range) {
  if (grepl("-", price_range)) {
    range <- strsplit(price_range, "-")[[1]]
    return((as.double(range[1]) + as.double(range[2])) / 2)
  }
  return(price_range)
}

cpu_data$Recommended_Customer_Price <- gsub("\\$", "", cpu_data$Recommended_Customer_Price) 
cpu_data$Recommended_Customer_Price <- gsub(",", "", cpu_data$Recommended_Customer_Price)

cpu_data$Recommended_Customer_Price <- sapply(cpu_data$Recommended_Customer_Price, recommend_price_cleaning) 
cpu_data$Recommended_Customer_Price <- as.double(cpu_data$Recommended_Customer_Price) 

# Lithography converting.
cpu_data <- cpu_data %>%
  mutate(Lithography = gsub("nm|\\s", "", Lithography),
         Lithography = as.double(Lithography))

# Processor_Base_Frequency converting (GHZ & MHZ) to MHZ.
Processor_Base_Frequency_cleaning <- function(string) { # Cleaning function
  if (!is.na(string)) {
    if (grepl("GHz", string)) {
      return(as.double(gsub("GHz|\\s", "", string)) * 1000)
    }
    return(as.double(gsub("MHz|\\s", "", string)))
  }
  return(NA)
}
cpu_data$Processor_Base_Frequency <- sapply(cpu_data$Processor_Base_Frequency,
                                            Processor_Base_Frequency_cleaning)

# Max_Memory_Size converting (TB & GB) to GB.
Max_Memory_Size_cleaning <- function(string) { # Cleaning function
  if (!is.na(string)) {
    if (grepl("TB", string)) {
      return(as.double(gsub("TB|\\s", "", string)) * 1024)
    }
    return(as.double(gsub("GB|\\s", "", string)))
  }
  return(NA)
}
cpu_data$Max_Memory_Size <- sapply(cpu_data$Max_Memory_Size,
                                   Max_Memory_Size_cleaning)

# Max_Memory_Bandwidth converting.
cpu_data <- cpu_data %>%
  mutate(Max_Memory_Bandwidth = gsub("GB/s|\\s", "", Max_Memory_Bandwidth),
         Max_Memory_Bandwidth = as.double(Max_Memory_Bandwidth))

# Cache converting, split into 2 columns represent cache_size, cache_type.
cpu_data <- cpu_data %>%
  mutate(Cache = sub(" ", "", Cache)) %>% # Remove the first space character.
  # Find the first " " character, merge everything from there to the end, then separate into 2 columns
  separate(Cache, sep = " ", into = c("Cache_size", "Cache_type"),
           remove = TRUE, extra = "merge", fill = "right")

Cache_size_cleaning <- function(string) { # Cleaning function
  if (!is.na(string)) {
    if (grepl("KB", string)) {
      return(as.double(gsub("KB", "", string)) / 1024)
    }
    return(as.double(gsub("MB", "", string)))
  }
  return(NA)
}
cpu_data$Cache_size <- sapply(cpu_data$Cache_size, Cache_size_cleaning)

## Remove any missing data in Recommended_Customer_Price
cpu_data <- cpu_data[!is.na(cpu_data$Recommended_Customer_Price), ]
print(apply(is.na(cpu_data), 2, sum))

# Numerical Variables filling
cpu_data$Lithography <- na.locf(cpu_data$Lithography)
# cpu_data$Recommended_Customer_Price[is.na(cpu_data$Recommended_Customer_Price)] <- median(cpu_data$Recommended_Customer_Price, na.rm = TRUE)
cpu_data$nb_of_Threads <- ifelse(is.na(cpu_data$nb_of_Threads), cpu_data$nb_of_Cores*2, cpu_data$nb_of_Threads)
cpu_data$Processor_Base_Frequency[is.na(cpu_data$Processor_Base_Frequency)] <- median(cpu_data$Processor_Base_Frequency, na.rm = TRUE)
cpu_data$Cache_size[is.na(cpu_data$Cache_size)] <- median(cpu_data$Cache_size, na.rm = TRUE)
cpu_data$Max_Memory_Size[is.na(cpu_data$Max_Memory_Size)] <- median(cpu_data$Max_Memory_Size, na.rm = TRUE)
cpu_data$Max_Memory_Bandwidth[is.na(cpu_data$Max_Memory_Bandwidth)] <- median(cpu_data$Max_Memory_Bandwidth, na.rm = TRUE)

par(mfrow = c(1, 1))
hist(cpu_data$Recommended_Customer_Price, main = "Recommended_Customer_Price", xlab = "Recommended_Customer_Price", col="blue2", labels=TRUE)

# Convert to logarit.
cpu_data$Lithography <- log(cpu_data$Lithography)
cpu_data$Recommended_Customer_Price <- log(cpu_data$Recommended_Customer_Price)
cpu_data$nb_of_Cores <- log(cpu_data$nb_of_Cores)
cpu_data$nb_of_Threads <- log(cpu_data$nb_of_Threads)
cpu_data$Processor_Base_Frequency <- log(cpu_data$Processor_Base_Frequency)
cpu_data$Cache_size <- log(cpu_data$Cache_size)
cpu_data$Max_Memory_Size <- log(cpu_data$Max_Memory_Size)
cpu_data$Max_Memory_Bandwidth <- log(cpu_data$Max_Memory_Bandwidth)


# Categorical Variables, Filling with "Missing".
table(cpu_data$Vertical_Segment)
cpu_data$Cache_type[is.na(cpu_data$Cache_type)] = "Missing"
table(cpu_data$Cache_type)
cpu_data$Execute_Disable_Bit[is.na(cpu_data$Execute_Disable_Bit)] = "Missing"
table(cpu_data$Execute_Disable_Bit)

# Removing any missed data observation of Recommended_Customer_Price
# cpu_data <- na.omit(cpu_data)

# Print out the number of NA
print(apply(is.na(temp), 2, sum))
print(apply(is.na(cpu_data), 2, sum))


# cpu_data %>% summary()
# Remove any unnecessary data,...
remove("aggr_plot")
remove("temp")
remove("recommend_price_cleaning")
remove("Cache_size_cleaning")
remove("Max_Memory_Size_cleaning")
remove("Processor_Base_Frequency_cleaning")

### Data visualization
## Statistics the numerical data
numerows <- c("Lithography", "Recommended_Customer_Price", "nb_of_Cores",
              "nb_of_Threads", "Processor_Base_Frequency", "Cache_size",
              "Max_Memory_Size", "Max_Memory_Bandwidth")

mean_ = apply(cpu_data[numerows], 2, mean, na.rm = TRUE)
sd_ = apply(cpu_data[numerows], 2, sd, na.rm = TRUE)
min_ = apply(cpu_data[numerows], 2, min, na.rm = TRUE)
first_quantile_ = apply(cpu_data[numerows], 2, quantile, na.rm = TRUE, probs = 0.25)
median_ = apply(cpu_data[numerows], 2, median, na.rm = TRUE)
third_quantile_ = apply(cpu_data[numerows], 2, quantile, na.rm = TRUE, probs = 0.75)
max_ = apply(cpu_data[numerows], 2, max, na.rm = TRUE)

numerical_summary <- cbind(mean_, sd_, min_, first_quantile_, median_, third_quantile_, max_)
rownames(numerical_summary) <- numerows
numerical_summary <- as.data.frame(numerical_summary)
View(numerical_summary)

# View(as.data.frame(cor(cpu_data[numerows])))

## Categorical data
View(as.data.frame(table(cpu_data$Vertical_Segment, dnn="Vertical_Segment")))
View(as.data.frame(table(cpu_data$Cache_type, dnn="Cache_type")))
View(as.data.frame(table(cpu_data$Execute_Disable_Bit, dnn="Execute_Disable_Bit")))

caterows <- c("Vertical_Segment", "Cache_type", "Execute_Disable_Bit")
categorical_summary <- as.data.frame(matrix(ncol=4))
for (i in caterows) {
  count <- length(cpu_data[[i]])
  uniq <- length(unique(cpu_data[[i]]))
  mod <- names(sort(-table(cpu_data[[i]])))[1]
  freq <- length(cpu_data[i][cpu_data[i] == mod])
  newrow <- c(count, uniq, mod, freq)
  categorical_summary <- rbind(categorical_summary, newrow)
}
categorical_summary <- categorical_summary[-1,]
colnames(categorical_summary) <- c("Count", "Unique", "Mode", "Frequency")
rownames(categorical_summary) <- caterows
View(categorical_summary)



## Statistic for linear regression
## Draw graphs
# Histogram for Recommended_Customer_Price
ggplot(cpu_data, aes(x = Recommended_Customer_Price)) +
  geom_histogram(bins = 30, fill = "blue", color = "black") +
  labs(title = "Histogram of Recommended_Customer_Price",
       x = "Recommended_Customer_Price", y = "Frequency")

# Draw boxplot for Recommended_Customer_Price vs Vertical_Segment
ggplot(cpu_data, aes(x = Vertical_Segment, y = Recommended_Customer_Price)) +
  geom_boxplot(fill=c("green2", "lemonchiffon1", 
                      "yellow1", "indianred1"), color = "black") +
  labs(title = "Boxplot of Recommended_Customer_Price vs Vertical_Segment",
       x = "Vertical_Segment", y = "Recommended_Customer_Price")

# Draw boxplot for Recommended_Customer_Price vs Cache_type
ggplot(cpu_data, aes(x = Cache_type, y = Recommended_Customer_Price)) +
  geom_boxplot(fill = c("darkslategray1", "hotpink1", 
                        "#192a40", "olivedrab2",
                        "salmon1"), color = "black") +
  labs(title = "Boxplot of Recommended_Customer_Price vs Cache_type",
       x = "Cache_type", y = "Recommended_Customer_Price")

# Draw boxplot for Recommended_Customer_Price vs Execute_Disable_Bit
ggplot(cpu_data, aes(x = Execute_Disable_Bit, y = Recommended_Customer_Price)) +
  geom_boxplot(fill = c("darkslategray1", "hotpink1", 
                        "olivedrab2"), color = "black") +
  labs(title = "Boxplot of Recommended_Customer_Price vs Execute_Disable_Bit",
       x = "Execute_Disable_Bit", y = "Recommended_Customer_Price")


## Draw pairplot for numerical data
pairs(cpu_data[c("Recommended_Customer_Price", "Lithography")], pch=16,
      col="red2", main="Pairplot of Recommended_Customer_Price vs Lithography")
pairs(cpu_data[c("Recommended_Customer_Price", "nb_of_Cores")], pch=16,
      col="blue2", main="Pairplot of Recommended_Customer_Price vs nb_of_Cores")
pairs(cpu_data[c("Recommended_Customer_Price", "nb_of_Threads")], pch=16,
      col="green2", main="Pairplot of Recommended_Customer_Price vs nb_of_Threads")
pairs(cpu_data[c("Recommended_Customer_Price", "Processor_Base_Frequency")], pch=16,
      col="yellow2", main="Pairplot of Recommended_Customer_Price vs Processor_Base_Frequency")
pairs(cpu_data[c("Recommended_Customer_Price", "Cache_size")], pch=16,
      col="purple2", main="Pairplot of Recommended_Customer_Price vs Cache_size")
pairs(cpu_data[c("Recommended_Customer_Price", "Max_Memory_Size")], pch=16,
      col="orange2", main="Pairplot of Recommended_Customer_Price vs Max_Memory_Size")
pairs(cpu_data[c("Recommended_Customer_Price", "Max_Memory_Bandwidth")], pch=16,
      col="brown2", main="Pairplot of Recommended_Customer_Price vs Max_Memory_Bandwidth")


## Linear regression
# Split dataset into train_set and test_set
set.seed(12345) # Ensure that generated number are all the same
train_size <- floor(0.8 * nrow(cpu_data)) # Take floor of 80% of data size
train_index <- sample(seq_len(nrow(cpu_data)), size = train_size) # Generate a vector of observation
train_index
train_set <- cpu_data[train_index, ]
test_set <- cpu_data[-train_index, ]

# Linear regression for Recommended_Customer_Price
model1 <- lm(formula = Recommended_Customer_Price ~ Lithography + nb_of_Cores + nb_of_Threads +
               Processor_Base_Frequency + Cache_size + Max_Memory_Size +
               Max_Memory_Bandwidth + as.factor(Vertical_Segment) +
               as.factor(Cache_type) + 
               as.factor(Execute_Disable_Bit), data = train_set)
summary(model1)

# Statistical tests
# H0: beta_i = 0, H1: beta_i != 0.
# Removing Execute_Disable_Bit with p_value > 0.5, can't reject H0.
model2 <- lm(formula = Recommended_Customer_Price ~ Lithography + nb_of_Cores + nb_of_Threads +
               Processor_Base_Frequency + Cache_size + Max_Memory_Size +
               Max_Memory_Bandwidth + as.factor(Vertical_Segment) +
               as.factor(Cache_type), data = train_set)
summary(model2)

# Using ANOVA for hypothesis test, NESTED MODEL
# H0: beta_i of removed variables in model1 = 0, H1:  atleast 1 of beta_i of removed variables in model1 != 0
anova(model2, model1)

# # Because we can't reject H0: beta_i of removed variables in model1 = 0, mean that both model perform the same, also Adjusted R-squared of model2 are slightly better.
# # So we can choose second model to go on with no different that removed any insignificant variable.
main_model <- model2

# Check if we fulfill all the assumptions.
# Because this is a multiple predictor variables model -> Analyzing the residual.
# If this was a simple linear regression model, we can check the assumption by analyzing the graph with a predictor on x-axis and an outcome on y-axis easily.
par(mfrow = c(2, 2))
plot(main_model)

# Prediction
# Split the real price to another dataframe
predict_data <- cbind(test_set$Recommended_Customer_Price)
colnames(predict_data)[1] <- "Recommended_Customer_Price"
test_set <- select(test_set, -Recommended_Customer_Price) # Remove the price on the test_set.

predict_data <- cbind(predict_data, predict(main_model, newdata = test_set, interval = "prediction", level = 0.9))
predict_data <- data.frame(predict_data)
View(predict_data)
# Calculate the accuracy
correct_prediction <- predict_data %>% filter(Recommended_Customer_Price >= lwr & Recommended_Customer_Price <= upr)

accuracy = nrow(correct_prediction) / nrow(predict_data)
print(accuracy)