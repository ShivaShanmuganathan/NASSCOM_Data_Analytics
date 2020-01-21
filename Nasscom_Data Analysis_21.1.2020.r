df <- read.csv("C:/Users/RAGHUL/Desktop/GRE-Quants/assignment.csv")

head(df)

ggplot(data=df, aes(x=Words_per_minute, y=Proofreading, color=Instructor)) + geom_point(size=2)

df_num <- (df[,-1])
df_num

matrix_corr <- cor(df_num)
matrix_corr

for (i in 1:nrow(matrix_corr)){
  correlations <-  which((matrix_corr[i,] > 0.50) & (matrix_corr[i,] != 1))
  
  if(length(correlations)> 0){
    print(colnames(df_num)[i])
    print(correlations)
  }
}

library(corrplot)
corrplot(matrix_corr, method = "ellipse")

result <- cor.test(df_num$Spreadsheet, df_num$Proofreading, method ="pearson")
result2 <- cor.test(df_num$Spreadsheet, df_num$Proofreading, method ="kendall")
result3 <- cor.test(df_num$Spreadsheet, df_num$Proofreading, method ="spearman")

result$estimate
result2$estimate
result3$estimate

model <- lm(df_num$Words_per_minute ~ df_num$Proofreading)
summary(model)

plot(model)
