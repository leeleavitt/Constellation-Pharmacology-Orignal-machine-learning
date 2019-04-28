# Project 1
# Autoamtically Detect a response using Jortan Tun's scoring
# 1. AITC.100uM
# 2. Menthol.400uM
# 3. Capsaicin.300nM
# 4. K.40mM

dat_harvest <-function(main_dir="Z:/Kate Tsourmas"){
    library(Pharming)
    main_dir <- "Z:/Kate Tsourmas"
    print(paste("I have entered", main_dir))

    #LOAD
    ML_experiment_dir <- getwd()
    source("Z:/procPharm 170210.r")

    #AUTOLOAD
    if( length(list.files(pattern = "exps_dirs.Rdata")) == 0 ){
        manual_find <- T
    }else{
        manual_find <- F
    }
    if(manual_find){
        setwd(main_dir)
        exps_dirs <- select.list(list.dirs(), multiple=T)
        setwd(ML_experiment_dir)
        save(exps_dirs, file="exps_dirs.Rdata")
        setwd(main_dir)
    }else{
        load( "exps_dirs.Rdata" ) 
        setwd(main_dir)
    }
    #CHECKING WR1
    exp_dirs_2 <- c()
    for( i in 1:length(exps_dirs) ){
        setwd( exps_dirs[i] )
        wrdef <- "wr1.docx"
        require(docxtractr)
        if (!is.null(wrdef)) {
            wr <- docx.wr1.importer(wrdef)
        }

        features <- c(
            '^aitc*', 
            '^menth.*', 
            '^caps.*', 
            '^k[.][4-5]{1}0mM')
        feat <- list()
        for( j in 1:length(features) ){
            feat[[j]] <- grep(features[j], as.character(wr['treatment',]), ignore.case = T, value = T)     
        }
        feat<-Reduce(c,feat[!is.na(feat)])
        # print(as.character(wr['treatment',]))
        # cat('\n\nWould you like to Add this Experiment? [y,n]')
        # choice<-scan(n=1, what="character")
        
        if(length(feat) >3 & length(feat)<5){
            #print(feat)
            exp_dirs_2[i] <- exps_dirs[i]
            #print(exp_dirs_2)
        }    
        setwd(main_dir)
        #print(exp_dirs_2)
    }

    exps_dirs <- exp_dirs_2[ !is.na(exp_dirs_2) ]
    print(exps_dirs)
    save(exps_dirs, file="exps_dirs.Rdata")
    ##########################################################################

    #LOADING IN AND CREATING FEATURE AND LABEL SPACE
    rd_name<-c()
    for( i in 1:length(exps_dirs) ){
        setwd( exps_dirs[i] )
        rd_name[i] <- list.files(pattern = "RD[.][0-9]")
        if(!is.null(rd_name)){
            print( rd_name[i] )
            load( rd_name[i] )
            alarm()
            rd_obj <- get( ls(pattern = "RD[.][0-9]") )
            
            #CHECKING HOW many potassiums to use
            lev_test <- grep('^k[.][4-9]{1}0mM$', unique(rd_obj$w.dat$wr1), ignore.case = T, value=T)
            if( length(lev_test) > 1){
                features <- c(
                    '^aitc.*[A-z0-9.]{2,8}$', 
                    '^menth.*[A-z0-9.]{2,8}$', 
                    '^caps.*[A-z0-9.]{2,8}$', 
                    '^k[.][5]{1}0mM[a-z.]{0,8}$')
            }else{
                features <- c(
                        '^aitc.*[A-z0-9.]{2,8}$', 
                        '^menth.*[A-z0-9.]{2,8}$',
                        '^caps.*[A-z0-9.]{2,8}$', 
                        '^k[.][4-9]{1}0mM[a-z.]{0,8}$')
            }
            
            ## FEATURE SPACE GENERATION
            scp_all_colnames <- colnames( rd_obj[[ 'scp' ]] )
            scp_colnames <- c()
            for(j in 1:length(features)){
                scp_colnames<-c(scp_colnames, grep(features[j], scp_all_colnames, ignore.case = T, value = T) )
            }
            #print(paste(length(scp_colnames),"::", scp_colnames))
            
            ## FIX THE FEATURE SPACE WITH NEW 
            # Legacy software collected statistics from the t.dat not the blc
            if( length( scp_colnames ) / length( features ) >= 6){
                print("I NEED TO CORRECT YOUR FEATURES")
                rd_obj <- TraceBrewer( rd_obj )
                features_logic <- paste0(features, collapse = "|")
                scp_all_colnames <- colnames( rd_obj[[ 'scp' ]] )
                scp_colnames <- grep(features_logic, scp_all_colnames, ignore.case = T, value = T)
            }

            # FEATURES: CREATE
            if( i == 1){
                feature_df_1 <- rd_obj[[ 'scp' ]] [ , scp_colnames ]
                feature_df_1 <- cbind( cell_name = row.names(rd_obj$scp), feature_df_1 )
                feature_df_1 <- cbind( exp_name = rd_name[i], feature_df_1)
            }else{ #If the first data_frame has already been created the append the new df to it
                feature_df_2 <- rd_obj[[ 'scp' ]] [ , scp_colnames ]
                feature_df_2 <- cbind( cell_name = row.names( rd_obj$scp ), feature_df_2 )
                feature_df_2 <- cbind( exp_name = rd_name[i], feature_df_2 )
                names(feature_df_2)<-names(feature_df_1)
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
                names(label_df_2)<-names(label_df_1)
                label_df_1 <- rbind( label_df_1, label_df_2 ) 

            }
        }        
        setwd(main_dir)
        rm(list=ls(pattern = "RD[.][0-9]"))
        gc()
    }
    print( paste( "Feature Dataframe dimensions ", dim(feature_df_1) ))
    setwd(ML_experiment_dir)
    write.csv(feature_df_1, file = paste0( Sys.Date(), ".feature.csv" ) )
    write.csv(label_df_1, file = paste0( Sys.Date(), ".label.csv" ) )
}

dat_harvest()