### Converts tables in database to csv files

library(dplyr)
library(readr)

# ProPublica analysis
f_propub = "Raw_data/compas-analysis/"

# Connect to the database
db <- src_sqlite(paste0(f_propub,"compas.db"), create = TRUE)

table_names <- src_tbls(db)

# Save each table csv in the format "db_[table name].csv"
for (i in 1:length(table_names)){
  table = tbl(db,table_names[i])
  
  write_csv(as.tbl(as.data.frame(table)),
            paste0(f_propub,"db_",table_names[i],".csv"))
}
