library(dplyr)

# assess parentage status

sequoia.out <- read.table("results/0.parentage/sequoia/Parents.txt", header = TRUE)

child.ids.sequoia <- sequoia.out %>%
  filter(!is.na(dam)) %>%
  select(id)

#write.table(child.ids.sequoia, file = "data/child.ids.sequoia.txt", col.names = FALSE, row.names = FALSE, quote = FALSE)