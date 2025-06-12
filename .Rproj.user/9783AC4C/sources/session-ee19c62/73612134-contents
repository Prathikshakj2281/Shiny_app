
# Load necessary libraries
library(sf)
library(smwrBase)
library(smwrData)
library(smwrGraphs)


# Sample water quality data for Bengaluru (mg/L)
bengaluru_data <- data.frame(
  Site = paste("Sample", 1:5),
  Ca = c(40, 35, 50, 60, 45),       
  Mg = c(12, 15, 10, 20, 18),
  Na = c(25, 30, 22, 28, 27),
  K  = c(2, 2.5, 2, 3, 2.8),
  Cl = c(30, 35, 28, 40, 33),
  SO4 = c(20, 25, 18, 22, 21),
  HCO3 = c(90, 100, 85, 110, 95)
)

# Function to convert mg/L to meq/L
to_meq <- function(mgL, mol_weight, valence) {
  (mgL / mol_weight) * valence
}

# Convert to meq/L
bengaluru_meq <- transform(bengaluru_data,
                           Ca   = to_meq(Ca,   40.08, 2),
                           Mg   = to_meq(Mg,   24.31, 2),
                           NaK  = to_meq(Na + K, 22.99, 1),  # Combine Na + K
                           Cl   = to_meq(Cl,   35.45, 1),
                           SO4  = to_meq(SO4,  96.06, 2),
                           HCO3 = to_meq(HCO3, 61.02, 1)
)

# Call piperPlot using ONLY positional arguments (very important!)
piperPlot(
  bengaluru_meq$Ca,       # 1st: Calcium
  bengaluru_meq$Mg,       # 2nd: Magnesium
  bengaluru_meq$NaK,      # 3rd: Sodium + Potassium
  bengaluru_meq$Cl,       # 4th: Chloride
  bengaluru_meq$SO4,      # 5th: Sulfate
  bengaluru_meq$HCO3,     # 6th: Bicarbonate
  Plot = list(name = bengaluru_data$Site),  # Sample labels
  caption = "Piper Diagram – Bengaluru City Water Samples"
)



