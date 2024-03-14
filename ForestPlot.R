library(forestplot)

dataset <- 
  structure(list(
    OR= c(NA, NA, 1.34, NA, NA, 1.70, NA, NA, 1.49, NA, NA, 4.42, 1.92, 1.56, NA, NA, 
          1.23, 0.92, NA, NA, 1.35, NA, NA, 2.79, NA, NA, 2.67, NA, NA, 1.64, NA, NA, 
          1.51, NA, NA, 1.97, NA, NA, 0.97, NA, NA, 1.68, NA, NA, 1.55, NA, NA, 1.36, NA, NA, 
          1.68, NA, NA, 1.56, NA, NA, 1.68, NA, NA, 1.58, NA, NA, 1.13, NA, 1.73), 
    
    lower = c(NA, NA, 1.12, NA, NA, 1.42, NA, NA, 1.10, NA, NA, 0.93, 0.45, 0.78, NA, NA, 
              0.66, 0.46, NA, NA, 0.79, NA, NA, 1.84, NA, NA, 1.23, NA, NA, 0.47, NA, NA, 
              0.71, NA, NA, 1.15, NA, NA, 0.84, NA, NA, 1.03, NA, NA, 1.01, NA, NA, 1.14, NA, NA, 
              1.13, NA, NA, 0.67, NA, NA, 0.99, NA, NA, 0.59, NA, NA, 0.65, NA, 1.72),
    
    upper = c(NA, NA, 4.72, NA, NA, 2.17, NA, NA, 3.14, NA, NA, 8.22, 3.11, 2.98, NA, NA, 
              2.32, 1.87, NA, NA, 2.26, NA, NA, 3.15, NA, NA, 5.63, NA, NA, 3.98, NA, NA, 
              3.18, NA, NA, 2.85, NA, NA, 1.31, NA, NA, 2.19, NA, NA, 2.22, NA, NA, 2.65, NA, NA, 
              1.91, NA, NA, 1.98, NA, NA, 2.99, NA, NA, 1.94, NA, NA, 1.75, NA, 1.74)),
    
    .Names = c("OR", "lower", "upper"), 
    
    row.names = c(NA, -35L), 
    
    class = "data.frame" )

mean(dataset$OR, na.rm = T)

tabletext<-cbind(
  
  c( "Covariates", 
     "Age",
     "> 29 years",  "≤ 29 years (Ref.)", 
     "Gender", 
     "Female","Male (Ref.)",
     "Marital Status",
     "Married", "Unmarried (Ref.)",
     "Highest Education", 
     "Higher Secondary or above", "Secondary","Primary","No education (Ref.)",
     "Monthly Income",
     "Low","Medium", "High (Ref.)",
     "Obesity",
     "Yes","No (Ref.)",
     "Diabetes",
     "Yes","No (Ref.)",
     "Hypertension",
     "Yes","No (Ref.)",
     "Chronic Pulmonary Disease",
     "Yes","No (Ref.)",
     "Ischemic Heart Disease",
     "Yes","No (Ref.)",
     "Abdominal Pain",
     "Yes","No (Ref.)",
     "Diarrhea",
     "Yes","No (Ref.)",
     "Vomiting",
     "Yes","No (Ref.)",
     "lethargy",
     "Yes","No (Ref.)",
     "Headache",
     "Yes","No (Ref.)",
     "Rash",
     "Yes","No (Ref.)",
     "Chills and Rigors",
     "Yes","No (Ref.)",
     "Nausea",
     "Yes","No (Ref.)",
     "Hemorrhagic",
     "Yes","No (Ref.)",
     "Musculoskeletal Pain",
     "Yes","No (Ref.)",
     "                    Pooled AOR")
)
library(dplyr)

x <- grid.grabExpr(print(forestplot(tabletext, 
           graph.pos = 2,
           dataset,new_page = T,
           is.summary=c(TRUE,TRUE,rep(FALSE,5),
                        TRUE,rep(FALSE,2),
                        TRUE,rep(FALSE,3),
                        TRUE,rep(FALSE,6),
                        TRUE,rep(FALSE,5),
                        TRUE,rep(FALSE,5),
                        TRUE,rep(FALSE,2),
                        TRUE,rep(FALSE,2),
                        TRUE,rep(FALSE,2),
                        TRUE,rep(FALSE,2),
                        TRUE,rep(FALSE,2),
                        FALSE),
           clip=c(0.5,6), 
           xlog=FALSE, 
           boxsize = 0.20,
           col=fpColors(box="black",line="black"),
           xticks=c(seq(0.5,6,0.5)),
           ci.vertices=TRUE,
           title = "MICS 2006",
           xlab = "Adjusted Odds Ratio (AOR)" 
)))
x

library(gridExtra)
tiff("AOR.tiff", units="in", width=12, height=12, res=300)
grid.arrange(x, ncol=1)
dev.off()
