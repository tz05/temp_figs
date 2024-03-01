library(readxl)

cnties <- excel_sheets('GDP Data (2).xlsx')[-(1:2)]
gdps <- NULL
for(cnt in cnties) {
  gdp <- readxl::read_xlsx('GDP Data (2).xlsx',cnt,range='E7:J7',col_names=F)
  gdps <- rbind(gdps,gdp)
}
gdps <- data.frame(as.numeric(gdps))
rownames(gdps) <- cnties
colnames(gdps) <- 2017:2022
