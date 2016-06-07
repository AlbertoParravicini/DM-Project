# vendite annuali per zona e prodotto
x <- data.frame()

for(k in unique(dataset_polimi$Key)){
  for(y in unique(dataset_polimi$Anno)){
    p1 <- sum(filter(dataset_polimi, Anno==y, Key==k, Categoria_prodotto=="Prodotto_1")$Vendite)
    p2 <- sum(filter(dataset_polimi, Anno==y, Key==k, Categoria_prodotto=="Prodotto_2")$Vendite)
    print(c(k, y, p1, p2, (p1+p2)))
    x <- rbind(x, data.frame(k, y, p1, p2, (p1+p2)))
  }
}

colnames(x)<-c("Key", "Anno", "P1", "P2", "Total")

write.csv(x, file="Modified data/aggregazione_chiave_anno.csv")
