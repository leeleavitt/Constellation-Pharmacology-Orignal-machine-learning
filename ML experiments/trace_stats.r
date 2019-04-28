#Trying to build more informative statistics.
main_dir <- "Z:/Jortan/CCI experiments/"
setwd(main_dir)
exps_dirs <- select.liest(list.dirs(), multiple=T)
print("These are all of the experiments to load")
print(exps_dirs)
print("")
exps_info <- strsplit(exps_dirs,  "/" )

#This is how to create the Feature space
features <- c('aitc', 'menth', 'caps', 'k.40')
feature_df <- data.frame()
rd_name<-c()
setwd(main_dir)
for( i in 1:length(exps_dirs) ){
    setwd( exps_dirs[i] )
    rd_name[i] <- list.files(pattern = "RD[.][0-9]")
    print( rd_name[i] )
    load( rd_name[i] )
    setwd(main_dir)
}


#    rd_obj <- get( ls(pattern = "RD[.][0-9]") )


