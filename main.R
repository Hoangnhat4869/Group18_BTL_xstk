# Checking for packages installation
my_packages <- c("tidyverse", "mice", "VIM")
missing_packages <- my_packages[!(my_packages %in% installed.packages()[,"Package"])]
if(length(missing_packages)) install.packages(missing_packages)
remove("my_packages")
remove("missing_packages")

# Library initialization
library(tidyverse)
library(mice)
library(VIM)

# Reading the dataset
cpu_raw <- read.csv("Dataset/Intel_CPUs.csv")
cpu_raw <- cpu_raw %>% select("Vertical_Segment", "Lithography", "nb_of_Cores", "nb_of_Threads",
                              "Processor_Base_Frequency", "Cache", "TDP",
                              "Embedded_Options_Available", "Max_Memory_Size", "Max_nb_of_Memory_Channels", "ECC_Memory_Supported",
                              "PCI_Express_Revision", "Max_nb_of_PCI_Express_Lanes", "Max_Memory_Bandwidth", "Intel_Hyper_Threading_Technology_",
                               "Intel_Virtualization_Technology_VTx_", "Intel_64_", "Instruction_Set", "Idle_States",
                              "Thermal_Monitoring_Technologies", "Secure_Key", "Execute_Disable_Bit"
                              )

# Replace all blank and other to NA
cpu_complete <- cpu_raw
cpu_complete[cpu_complete == ""] <- NA
cpu_complete[cpu_complete == "N/A"] <- NA
temp = cpu_complete

# Plot data pattern
aggr_plot <- aggr(cpu_complete, col=c('navyblue','red'),
                  numbers=TRUE,
                  sortVars=TRUE,
                  labels=names(cpu_complete),
                  cex.axis=.7,
                  gap=3,
                  ylab=c("Histogram of Missing data","Pattern"),
                  cex.numbers = 0.1
                  )

# Convert any string data that need to be in integer type.
  # Lithography converting.
  cpu_complete = cpu_complete %>%
    mutate(Lithography = gsub("nm|\\s", "", Lithography),
           Lithography = as.double(Lithography))
  
  # Processor_Base_Frequency converting (GHZ & MHZ) to MHZ.
  Processor_Base_Frequency_cleaning = function(string) { # Cleaning function
    if (!is.na(string)) {
      if(grepl("GHz",string)){
        return ( as.double(gsub("GHz|\\s","",string)) * 1000 )
      }
      return ( as.double(gsub("MHz|\\s","",string)) )
    }
    return(NA)
  }
  cpu_complete$Processor_Base_Frequency = sapply(cpu_complete$Processor_Base_Frequency, Processor_Base_Frequency_cleaning) # Cleaning section
  
  # Max_Memory_Size converting (TB & GB) to GB.
  Max_Memory_Size_cleaning = function(string) { # Cleaning function
    if (!is.na(string)) {
      if(grepl("TB",string)){
        return ( as.double(gsub("TB|\\s","",string)) * 1024 )
      }
      return ( as.double(gsub("GB|\\s","",string)) )
    }
    return(NA)
  }
  cpu_complete$Max_Memory_Size = sapply(cpu_complete$Max_Memory_Size, Max_Memory_Size_cleaning) # Cleaning section
  
  # Max_Memory_Bandwidth converting.
  cpu_complete = cpu_complete %>%
    mutate(Max_Memory_Bandwidth = gsub("GB/s|\\s", "", Max_Memory_Bandwidth),
           Max_Memory_Bandwidth = as.double(Max_Memory_Bandwidth))
  
  # Cache converting, split into 2 columns represent cache_size, cache_type.
  cpu_complete = cpu_complete %>%
    mutate(Cache = sub(" ", "", Cache)) %>% # Remove the first space \\s character.
    # Find the first " " character, merge everything from there to the end, then separate into 2 columns
    separate(Cache, sep = " ", into = c("Cache_size", "Cache_type"), remove = TRUE, extra = "merge", fill = "right")
  
  Cache_size_cleaning = function(string) { # Cleaning function
    if (!is.na(string)) {
      if(grepl("KB",string)){
        return ( as.double(gsub("KB","",string)) / 1024 )
      }
      return ( as.double(gsub("MB","",string)) )
    }
    return(NA)
  }
  cpu_complete$Cache_size = sapply(cpu_complete$Cache_size, Cache_size_cleaning) # Cleaning section
  
  # TDP Converting
  cpu_complete = cpu_complete %>%
    mutate(TDP = as.double(gsub("W|\\s", "", TDP)))
  
# Process missing data.  
  # Numerical Variables, Filling with Mean of its columns.
cpu_complete$nb_of_Threads[is.na(cpu_complete$nb_of_Threads)] = mean(cpu_complete$nb_of_Threads, na.rm = TRUE)
cpu_complete$Lithography[is.na(cpu_complete$Lithography)] = mean(cpu_complete$Lithography, na.rm = TRUE)
cpu_complete$Processor_Base_Frequency[is.na(cpu_complete$Processor_Base_Frequency)] = mean(cpu_complete$Processor_Base_Frequency, na.rm = TRUE)
cpu_complete$Cache_size[is.na(cpu_complete$Cache_size)] = mean(cpu_complete$Cache_size, na.rm = TRUE)
cpu_complete$TDP[is.na(cpu_complete$TDP)] = mean(cpu_complete$TDP, na.rm = TRUE)
cpu_complete$Max_Memory_Size[is.na(cpu_complete$Max_Memory_Size)] = mean(cpu_complete$Max_Memory_Size, na.rm = TRUE)
cpu_complete$Max_nb_of_Memory_Channels[is.na(cpu_complete$Max_nb_of_Memory_Channels)] = mean(cpu_complete$Max_nb_of_Memory_Channels, na.rm = TRUE)
cpu_complete$Max_nb_of_PCI_Express_Lanes[is.na(cpu_complete$Max_nb_of_PCI_Express_Lanes)] = mean(cpu_complete$Max_nb_of_PCI_Express_Lanes, na.rm = TRUE)
cpu_complete$Max_Memory_Bandwidth[is.na(cpu_complete$Max_Memory_Bandwidth)] = mean(cpu_complete$Max_Memory_Bandwidth, na.rm = TRUE)

  # Categorical Variables, Filling with "Missing".
cpu_complete$Instruction_Set[is.na(cpu_complete$Instruction_Set)] = "Missing"
cpu_complete$Cache_type[is.na(cpu_complete$Cache_type)] = "Missing"
cpu_complete$Embedded_Options_Available[is.na(cpu_complete$Embedded_Options_Available)] = "Missing"
cpu_complete$ECC_Memory_Supported[is.na(cpu_complete$ECC_Memory_Supported)] = "Missing"
cpu_complete$PCI_Express_Revision[is.na(cpu_complete$PCI_Express_Revision)] = "Missing"
cpu_complete$Intel_Hyper_Threading_Technology_[is.na(cpu_complete$Intel_Hyper_Threading_Technology_)] = "Missing"
cpu_complete$Intel_Virtualization_Technology_VTx_[is.na(cpu_complete$Intel_Virtualization_Technology_VTx_)] = "Missing"
cpu_complete$Intel_64_[is.na(cpu_complete$Intel_64_)] = "Missing"
cpu_complete$Idle_States[is.na(cpu_complete$Idle_States)] = "Missing"
cpu_complete$Thermal_Monitoring_Technologies[is.na(cpu_complete$Thermal_Monitoring_Technologies)] = "Missing"
cpu_complete$Secure_Key[is.na(cpu_complete$Secure_Key)] = "Missing"
cpu_complete$Execute_Disable_Bit[is.na(cpu_complete$Execute_Disable_Bit)] = "Missing"


# Print out the number of NA
print(apply(is.na(temp),2,sum))
print(apply(is.na(cpu_complete),2,sum))
cpu_complete %>% summary()

# Remove any unnecessary data,...
remove("aggr_plot")
remove("temp")
remove("Cache_size_cleaning")
remove("Max_Memory_Size_cleaning")
remove("Processor_Base_Frequency_cleaning")