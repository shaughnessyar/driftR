# Clean Water-quality Monitoring Data
#
# A data set containing corrected measurements from a YSI Sonde 6600
#
# @docType data
#
# @usage data(sondeClean)
#
# @format a dataframe with 1528 rows and 17 variables
# \describe{
#    \item{Date}{Date of measurement}
#    \item{Time}{Time of measurement}
#    \item{DateTime}{combined date and time variables}
#    \item{Temp}{Temperature}
#    \item{SpCond}{Specific conductivity raw}
#    \item{SpCond_Corr}{Specific conductivity corrected}
#    \item{pH}{pH raw}
#    \item{pH_Corr}{pH corrected}
#    \item{pHmV}{Potential reading from pH sensor}
#    \item{Chloride}{Chloride raw}
#    \item{Chloride_Corr}{Chloride corrected}
#    \item{AmmoniumN}{Ammonium-Nitrogen}
#    \item{NitrateN}{Nitrate-Nitrogen}
#    \item{`Turbidity+`}{Turbidity raw}
#    \item{Turbidity_Corr}{Turbidity corrected}
#    \item{DO}{Dissolved Oxygen raw}
#    \item{DO_Corr}{Dissolved Oxygen corrected}
# }
#
# @source Saint Louis University Geochemistry Lab
#
# @examples
# str(sondeClean)
# head(sondeClean)
#
"sondeClean"
