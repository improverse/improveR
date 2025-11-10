#IMPORT DATA
#install package if not already available
if (!requireNamespace("nlme", quietly = TRUE)) {
  install.packages("nlme")
}
#extract dataset from nlme package
remData <- nlme::Remifentanil
#export dataset as csv (column sep = ";")
readr::write_csv2(remData, "remData.csv")
#Comment to render file outdated
#Comment to render file outdated
#Comment to render file outdated
