
# Install the rptR package for estimation of repeatability
install.packages("rptR")
# Load necessary libraries
library(readxl)
library(rptR)

# List available files
list.files()

# Read phenotype data 
pheno <- read_excel("Pheno_1.xlsx", 
                    sheet = "Sheet1",
                    .name_repair = 'minimal') |> as.data.frame()
head(pheno)

# Get colnames and extract columns with data
colnames(pheno)
data <- pheno[, c('Year','Trial','Name','SEV','INC','INDEX')]
head(data)

# Check data structure
str(data)

# Omit missing data
data <- na.omit(data)

# Covert paramters from character to numeric
data$SEV <- as.numeric(data$SEV)
data$INC <- as.numeric(data$INC)
data$INDEX <- as.numeric(data$INDEX)
str(data)

# Get the number of unique genotypes
length(unique(data$Name)) ; unique(data$Name)
# Get the number of unique years
length(unique(data$Year)); unique(data$Year)
# Get the number of unique trials
length(unique(data$Trial)) ; unique(data$Trial)

# Estimate repeatability for SEV across years and Genotypes and Trials
SEV_rpt <- rpt(SEV ~ (1 | Name) + (1 | Year) + (1 | Trial),
  grname = c("Name", "Year" , 'Trial'),
  data = data,
  datatype = "Gaussian",
  nboot = 1000,
  npermut = 0
)
lme4::VarCorr(SEV_rpt$mod) # Get variance components
summary(SEV_rpt) # Get repeatability estimates
# Plot the results for repeatability SEV
plot(SEV_rpt, main = 'Bootstrap Repeatability for SEV')

# Estimate repeatability for INC across years and Genotypes and Trials
INC_rpt <- rpt(INC ~ (1 | Name) + (1 | Year) + (1 | Trial),
               grname = c("Name", "Year" , 'Trial'),
               data = data,
               datatype = "Gaussian",
               nboot = 1000,
               npermut = 0
)
lme4::VarCorr(INC_rpt$mod) # Get variance components
summary(INC_rpt) # Get repeatability estimates
# Plot the results for repeatability INC
plot(INC_rpt, main = 'Bootstrap Repeatability for INC')

# Estimate repeatability for INDEX across years and Genotypes and Trials
INDEX_rpt <- rpt(INDEX ~ (1 | Name) + (1 | Year) + (1 | Trial),
               grname = c("Name", "Year" , 'Trial'),
               data = data,
               datatype = "Gaussian",
               nboot = 1000,
               npermut = 0
)
lme4::VarCorr(INDEX_rpt$mod) # Get variance components
summary(INDEX_rpt) # Get repeatability estimates
# Plot the results for repeatability INDEX
plot(INDEX_rpt, main = 'Bootstrap Repeatability for INDEX')

# Partition and view the plots for all 3 parameters
par(mfrow=c(3,1))
plot(SEV_rpt,main = 'Bootstrap Repeatability for SEV')
plot(INC_rpt,main = 'Bootstrap Repeatability for INC')
plot(INDEX_rpt,main = 'Bootstrap Repeatability for INDEX')



