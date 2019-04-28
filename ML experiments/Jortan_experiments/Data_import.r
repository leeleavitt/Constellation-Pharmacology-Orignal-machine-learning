# Project 1
# Autoamtically Detect a response using Jortan Tun's scoring
# 1. AITC.100uM
# 2. Menthol.400uM
# 3. Capsaicin.300nM
# 4. K.40mM
source("Z:/procPharm 170210.r")
main_dir <- "Z:/Jortan/CCI experiments/"
setwd(main_dir)
exps_dirs <- select.list(list.dirs(), multiple=T)
print("These are all of the experiments to load")
print(exps_dirs)
print("")
exps_info <- strsplit(exps_dirs,  "/" )

#This is how to create the Feature space
features <- c('aitc', 'menth', 'caps', 'k.40')
feature_df <- data.frame()
rd_name<-c()
for( i in 1:length(exps_dirs) ){
    setwd( exps_dirs[i] )
    rd_name[i] <- list.files(pattern = "RD[.][0-9]")
    print( rd_name[i] )
    load( rd_name[i] )
    rd_obj <- get( ls(pattern = "RD[.][0-9]") )

    ## FEATURE SPACE GENERATION
    features_logic <- paste0(features, collapse = "|")
    scp_all_colnames <- colnames( rd_obj[[ 'scp' ]] )
    scp_colnames <- grep(features_logic, scp_all_colnames, ignore.case = T, value = T)
    
    ## FIX THE FEATURE SPACE
    # Legacy software collected statistics from the t.dat not the blc
    if( length( scp_colnames ) / length( features ) >= 6){
        print("I NEED TO CORRECT YOUR FEATURES")
        rd_obj <- TraceBrewer( rd_obj )
        features_logic <- paste0(features, collapse = "|")
        scp_all_colnames <- colnames( rd_obj[[ 'scp' ]] )
        scp_colnames <- grep(features_logic, scp_all_colnames, ignore.case = T, value = T)
    }

    # Now create the first data.frame for us to allow us to add things to.
    if( i == 1){
        feature_df_1 <- rd_obj[[ 'scp' ]] [ , scp_colnames ]
        feature_df_1 <- cbind( cell_name = row.names(rd_obj$scp), feature_df_1 )
        feature_df_1 <- cbind( exp_name = rd_name[i], feature_df_1)
    }else{ #If the first data_frame has already been created the append the new df to it
        feature_df_2 <- rd_obj[[ 'scp' ]] [ , scp_colnames ]
        feature_df_2 <- cbind( cell_name = row.names( rd_obj$scp ), feature_df_2 )
        feature_df_2 <- cbind( exp_name = rd_name[i], feature_df_2 )
        feature_df_1 <- rbind( feature_df_1, feature_df_2 ) 
    }

    ## LABEL SPACE GENERATION
    labels_logic <- paste0( features, collapse = "|")
    bin_all_colnames <- colnames( rd_obj[[ 'bin' ]] )
    bin_colnames <- grep( labels_logic, bin_all_colnames, ignore.case = T, value = T)
    # Now create the first data.frame for us to allow us to add things to.
    if( i == 1){
        label_df_1 <- rd_obj[[ 'bin' ]] [ , bin_colnames ]
        label_df_1 <- cbind( cell_name = row.names(rd_obj$bin), label_df_1 )
        label_df_1 <- cbind( exp_name = rd_name[i], label_df_1)
    }else{ 
        #If the first data_frame has already been created the append the new df to it
        label_df_2 <- rd_obj[[ 'bin' ]] [ , bin_colnames ]
        label_df_2 <- cbind( cell_name = row.names(rd_obj$bin), label_df_2 )
        label_df_2 <- cbind( exp_name =  rd_name[i], label_df_2)
        label_df_1 <- rbind( label_df_1, label_df_2 ) 
    }
    setwd(main_dir)
    rm(list=ls(pattern = "RD[.][0-9]"))
    gc()
}

setwd("Z:/Lee Leavitt/ML experiments")
write.csv(feature_df_1, file = paste0( Sys.Date(), ".feature.csv" ) )
write.csv(label_df_1, file = paste0( Sys.Date(), ".label.csv" ) )



