main_dir <- "Z:/Lee Leavitt/ML experiments"
experimentors <- grep("_experiments",setdiff(list.dirs(),"."), value = T)
#LOAD IN ALL DATA
for (i in 1:length(experimentors)){
    print(i)
    setwd(experimentors[i])
    if(i == 1){
        features <- list.files(pattern = "feature")
        features <- read.csv(features)
        features <- features[,order(names(features))]
        experimentor <- sub("./","",experimentors[i])    
        feature_df_1<-cbind(experimentor, features)
    }else{
        features <- list.files(pattern = "feature")
        features <- read.csv(features)
        features <- features[,order(names(features))]
        experimentor <- sub("./","",experimentors[i])    
        feature_df_2<-cbind(experimentor, features)
        names(feature_df_2)<-names(feature_df_1)
        feature_df_1 <- rbind(feature_df_1, feature_df_2)
    }

    if(i == 1){
        labels <- list.files(pattern = "label")
        labels <- read.csv(labels)
        labels <- labels[,order(names(labels))]
        experimentor <- sub("./","",experimentors[i])    
        label_df_1<-cbind(experimentor, labels)
    }else{
        labels <- list.files(pattern = "label")
        labels <- read.csv(labels)
        labels <- labels[,order(names(labels))]
        experimentor <- sub("./","",experimentors[i])    
        label_df_2<-cbind(experimentor, labels)
        names(label_df_2) <- names(label_df_1)
        label_df_1 <- rbind(label_df_1, label_df_2)
    }
    setwd(main_dir)
}

main_features <- feature_df_1
main_labels <- label_df_1

write.csv(main_features, file="main_features.csv")
write.csv(main_labels, file="main_labels.csv")

