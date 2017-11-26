k <- c(50:60)
kresult <- searchK(gsr_stm$documents,
                   gsr_stm$vocab,
                   k,
                   prevalence = ~ rating,
                   data = gsr_stm$meta,
                   cores = 4)
plot(kresult)