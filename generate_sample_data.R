mvdata = matrix(runif(100*20), ncol=20, byrow=T)
colnames(mvdata) = paste("sample", 1:20, sep="")
rownames(mvdata) = paste("var", 1:100, sep="")
write.table(mvdata, file="mvdata.txt", sep="\t")
write.csv2(mvdata, file="mvdata.csv")

metadata = data.frame(f=factor(runif(20)<0.5), 
                      g=factor(runif(20)<0.5), 
                      h = runif(20))
rownames(metadata) = paste("sample", 1:20, sep="")

write.table(metadata, file="metadata.txt", sep="\t")
write.csv2(metadata, file="metadata.csv")
