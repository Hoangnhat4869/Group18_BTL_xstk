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
cpu_data <- read.csv(".\\Intel_CPUs.csv", header = TRUE,
                     stringsAsFactors = FALSE, na.strings = c("", "N/A"))
# View(cpu_data)

cpu_data <- cpu_data %>% select("Vertical_Segment", "Lithography",
  "Recommended_Customer_Price", "nb_of_Cores", "nb_of_Threads",
  "Processor_Base_Frequency", "Cache", "Embedded_Options_Available",
  "Max_Memory_Size", "Max_nb_of_Memory_Channels", "Max_Memory_Bandwidth",
  "Instruction_Set", "Idle_States", "Execute_Disable_Bit"
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

# meantemp <- cpu_data
# mediantemp <- cpu_data
# meantemp$Lithography[is.na(meantemp$Lithography)] <- mean(meantemp$Lithography, na.rm = TRUE)
# meantemp$Recommended_Customer_Price[is.na(meantemp$Recommended_Customer_Price)] <- mean(meantemp$Recommended_Customer_Price, na.rm = TRUE)
# meantemp$nb_of_Threads[is.na(meantemp$nb_of_Threads)] <- mean(meantemp$nb_of_Threads, na.rm = TRUE)
# meantemp$Processor_Base_Frequency[is.na(meantemp$Processor_Base_Frequency)] <- mean(meantemp$Processor_Base_Frequency, na.rm = TRUE)
# meantemp$Cache_size[is.na(meantemp$Cache_size)] <- mean(meantemp$Cache_size, na.rm = TRUE)
# meantemp$Max_Memory_Size[is.na(meantemp$Max_Memory_Size)] <- mean(meantemp$Max_Memory_Size, na.rm = TRUE)
# meantemp$Max_nb_of_Memory_Channels[is.na(meantemp$Max_nb_of_Memory_Channels)] <- mean(meantemp$Max_nb_of_Memory_Channels, na.rm = TRUE)
# meantemp$Max_Memory_Bandwidth[is.na(meantemp$Max_Memory_Bandwidth)] <- mean(meantemp$Max_Memory_Bandwidth, na.rm = TRUE)
# mediantemp$Lithography[is.na(mediantemp$Lithography)] <- median(mediantemp$Lithography, na.rm = TRUE)
# mediantemp$Recommended_Customer_Price[is.na(mediantemp$Recommended_Customer_Price)] <- median(mediantemp$Recommended_Customer_Price, na.rm = TRUE)
# mediantemp$nb_of_Threads[is.na(mediantemp$nb_of_Threads)] <- median(mediantemp$nb_of_Threads, na.rm = TRUE)
# mediantemp$Processor_Base_Frequency[is.na(mediantemp$Processor_Base_Frequency)] <- median(mediantemp$Processor_Base_Frequency, na.rm = TRUE)
# mediantemp$Cache_size[is.na(mediantemp$Cache_size)] <- median(mediantemp$Cache_size, na.rm = TRUE)
# mediantemp$Max_Memory_Size[is.na(mediantemp$Max_Memory_Size)] <- median(mediantemp$Max_Memory_Size, na.rm = TRUE)
# mediantemp$Max_nb_of_Memory_Channels[is.na(mediantemp$Max_nb_of_Memory_Channels)] <- median(mediantemp$Max_nb_of_Memory_Channels, na.rm = TRUE)
# mediantemp$Max_Memory_Bandwidth[is.na(mediantemp$Max_Memory_Bandwidth)] <- median(mediantemp$Max_Memory_Bandwidth, na.rm = TRUE)
# par(mfrow = c(3, 3))


# Process missing data.
hist(cpu_data$Lithography, main = "Lithography", xlab = "Lithography", col="red", labels=TRUE)
hist(cpu_data$Recommended_Customer_Price, main = "Recommended_Customer_Price", xlab = "Recommended_Customer_Price", col="red", labels=TRUE)
hist(cpu_data$nb_of_Cores, main = "nb_of_Cores", xlab = "nb_of_Cores", col="red", labels=TRUE)  ## Not missing
hist(cpu_data$nb_of_Threads, main = "nb_of_Threads", xlab = "nb_of_Threads", col="red", labels=TRUE)
hist(cpu_data$Processor_Base_Frequency, main = "Processor_Base_Frequency", xlab = "Processor_Base_Frequency", col="red", labels=TRUE)
hist(cpu_data$Cache_size, main = "Cache_size", xlab = "Cache_size", col="red", labels=TRUE)
hist(cpu_data$Max_Memory_Size, main = "Max_Memory_Size", xlab = "Max_Memory_Size", col="red", labels=TRUE)
hist(cpu_data$Max_nb_of_Memory_Channels, main = "Max_nb_of_Memory_Channels", xlab = "Max_nb_of_Memory_Channels", col="red", labels=TRUE)
hist(cpu_data$Max_Memory_Bandwidth, main = "Max_Memory_Bandwidth", xlab = "Max_Memory_Bandwidth", col="red", labels=TRUE)

# Numerical Variables, Filling with Mean of its columns, then convert to logarit.
cpu_data$Lithography <- na.locf(cpu_data$Lithography)
cpu_data$Recommended_Customer_Price[is.na(cpu_data$Recommended_Customer_Price)] <- mean(cpu_data$Recommended_Customer_Price, na.rm = TRUE)
cpu_data$nb_of_Threads <- ifelse(is.na(cpu_data$nb_of_Threads), cpu_data$nb_of_Cores*2, cpu_data$nb_of_Threads)
cpu_data$Processor_Base_Frequency[is.na(cpu_data$Processor_Base_Frequency)] <- median(cpu_data$Processor_Base_Frequency, na.rm = TRUE)
cpu_data$Cache_size[is.na(cpu_data$Cache_size)] <- median(cpu_data$Cache_size, na.rm = TRUE)
cpu_data$Max_Memory_Size[is.na(cpu_data$Max_Memory_Size)] <- median(cpu_data$Max_Memory_Size, na.rm = TRUE)
cpu_data$Max_nb_of_Memory_Channels[is.na(cpu_data$Max_nb_of_Memory_Channels)] <- median(cpu_data$Max_nb_of_Memory_Channels, na.rm = TRUE)
cpu_data$Max_Memory_Bandwidth[is.na(cpu_data$Max_Memory_Bandwidth)] <- median(cpu_data$Max_Memory_Bandwidth, na.rm = TRUE)

# Convert to logarit.
cpu_data$Lithography <- log(cpu_data$Lithography)
cpu_data$Recommended_Customer_Price <- log(cpu_data$Recommended_Customer_Price)
cpu_data$nb_of_Cores <- log(cpu_data$nb_of_Cores)
cpu_data$nb_of_Threads <- log(cpu_data$nb_of_Threads)
cpu_data$Processor_Base_Frequency <- log(cpu_data$Processor_Base_Frequency)
cpu_data$Cache_size <- log(cpu_data$Cache_size)
cpu_data$Max_Memory_Size <- log(cpu_data$Max_Memory_Size)
cpu_data$Max_nb_of_Memory_Channels <- log(cpu_data$Max_nb_of_Memory_Channels)
cpu_data$Max_Memory_Bandwidth <- log(cpu_data$Max_Memory_Bandwidth)

# Categorical Variables, Filling with "Missing".
table(cpu_data$Vertical_Segment)
cpu_data$Cache_type[is.na(cpu_data$Cache_type)] = "Missing"
table(cpu_data$Cache_type)
cpu_data$Embedded_Options_Available[is.na(cpu_data$Embedded_Options_Available)] = "Missing"
table(cpu_data$Embedded_Options_Available)
cpu_data$Instruction_Set[is.na(cpu_data$Instruction_Set)] = "Missing"
table(cpu_data$Instruction_Set)
cpu_data$Idle_States[is.na(cpu_data$Idle_States)] = "Missing"
table(cpu_data$Idle_States)
cpu_data$Execute_Disable_Bit[is.na(cpu_data$Execute_Disable_Bit)] = "Missing"
table(cpu_data$Execute_Disable_Bit)


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
              "Max_Memory_Size", "Max_nb_of_Memory_Channels",
              "Max_Memory_Bandwidth")

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
caterows <- c("Vertical_Segment", "Cache_type", "Embedded_Options_Available",
              "Instruction_Set", "Idle_States", "Execute_Disable_Bit")
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

