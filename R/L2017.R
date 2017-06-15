#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
# L2017 - Graphs for Language Article
#
# Most uptodate versions available from http://bld.markellison.net/


#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
# L2017Graphme - Graphs for Language 2017 Paper on Rapid Lexical Divergence
# - each item associated with a relative frequency
# - no associative language interaction (as monolingual)
# - no monitoring
# - some more content
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
#
require(R6)
L2017Graphme <- R6Class("L2017Graphme",
                        public = list(
                          resolution = 300,
                          widthMm = 150,
                          heightMm = 150,
                          width = 640, height = 640,
                          isColour = FALSE,
                          scaler = 1.0,
                          initialize = function() {
                            require(ggplot2); # require(cowplot); require(grid);
                            self$scaler <- 1.0;
                            self$width <- round( self$widthMm * self$resolution * 0.0393701 );
                            self$height <- round( self$heightMm * self$resolution * 0.0393701 );
                          }
                        )
);

#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
# L2017Graphme$sgData
#
L2017Graphme$set("public","setSize", function(widthMm,heightMm) {
  self$widthMm <- widthMm;
  self$heightMm <- heightMm;
  return( self );
});

#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
# L2017Graphme$sgData
#
L2017Graphme$set("public","setResolution", function(resolution) {
  self$resolution <- resolution;
  self$scaler <- 1.0;
  self$width <- round( self$widthMm * self$resolution * 0.0393701 );
  self$height <- round( self$heightMm * self$resolution * 0.0393701 );
  return( self );
});

#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
# L2017Graphme$sgData
#
L2017Graphme$set("public","setIsColour", function(isColour) {
  self$isColour <- isColour;
  return( self );
});

#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
# L2017Graphme$sgData
#
L2017Graphme$set("public","writeGgplot2Png", function(graphNumber,filename,g) {
  filename <- paste(graphNumber,filename,sep="-");
  filepath <- paste(self$graphDir,filename,sep="");
  png(filepath,width=self$widthMm,height=self$heightMm,units='mm',res=self$resolution);
  plot( g );
  dev.off();
  plot( g ); # draw to screen as well
  return( self );
});

#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
# L2017Datame - Graphs for Language 2017 Paper on Rapid Lexical Divergence
# - each item associated with a relative frequency
# - no associative language interaction (as monolingual)
# - no monitoring
#
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
#
L2017Datame <- R6Class("L2017Datame",
                        public = list(
                          TA = NULL,
                          MonolingualAv = NULL,
                          BilingualResponse = NULL,
                          initialize = function() {
                            require(tensorA);
                            self$TA <- TensorAgent$new();
                          }
                        )
);

#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
# L2017Datame$set_english_gaidhlig
#
L2017Datame$set("public","lex__english_gaidhlig", function() {
  self$TA$clearLexicon();
  self$TA$addExample("WINTER","E", "winter",   ct=100);
  self$TA$addExample("WINTER","SG","geamradh", ct= 73);
  self$TA$addExample("WINTER","SG","gamh",     ct= 27);
  r <- self$TA$constructDataTensor();
});

#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
# L2017Datame$lex__n_vs_n
#
L2017Datame$set("public","lex__n_n", function() {
  self$TA$clearLexicon();
  self$TA$addExample("DOG","French","chien", ct=100);
  self$TA$addExample("DOG","English","dog",  ct=100);
  r <- self$TA$constructDataTensor();
});

#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
# L2017Datame$lex__dn_d
#
L2017Datame$set("public","lex__dn_d", function(A_d=50,A_n=50,B_d=100) {
  self$TA$clearLexicon();
  self$TA$addExample("PHOTO","English","foto",    ct=A_d);
  self$TA$addExample("PHOTO","English","picture", ct=A_n);
  # Doppels only recognised in simulation if forms are identical, so modify photo
  self$TA$addExample("PHOTO","Dutch",  "foto",    ct=B_d);
  r <- self$TA$constructDataTensor();
});

#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
# L2017Datame$lex__dn_dn
#
L2017Datame$set("public","lex__dn_dn", function(A_d=50,A_n=50,B_d=50,B_n=50) {
  self$TA$clearLexicon();
  self$TA$addExample("PHOTO","English","other",   ct=A_d);
  self$TA$addExample("PHOTO","English","second",  ct=A_n);
  # Doppels only recognised in simulation if forms are identical, so modify photo
  self$TA$addExample("PHOTO","Dutch",  "other",   ct=B_d); # "ander" but we need identical forms
  self$TA$addExample("PHOTO","Dutch",  "tweede",  ct=B_n);
  r <- self$TA$constructDataTensor();
});

#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
# L2017Datame$lex_experiment
#
L2017Datame$set("public","lex__experiment", function() {
  data(EM2017_Stimuli);
  data(EM2017_Responses);
  #
  r <- EM2017_Responses;
  s <- EM2017_Stimuli;
  #
  r$Doppel <- s$EnglishDoppel[r$StimulusId];
  r$Meaning <- paste("S",r$StimulusId,"_",toupper(s$EnglishDoppel)[r$StimulusId],sep="");
  self$MonolingualAv <- aggregate(IsDoppel ~ StimulusId+Meaning+Doppel,r[r$Condition == "Monolingual",], mean);
  self$BilingualResponse <- r[r$Condition == "Bilingual",];
  StimulusIds <- s$StimulusId[order(s$StimulusId)];
  #
  self$TA$clearLexicon();
  for (stimulusId in StimulusIds) {
    av <- self$MonolingualAv[self$MonolingualAv$StimulusId == stimulusId,];
    self$TA$addExample(av$Meaning,"English", av$Doppel, ct=av$IsDoppel);
    self$TA$addExample(av$Meaning,"English", "Enondoppel", ct=1.0 - av$IsDoppel);
    # To be recognised as a doppel in simulation, the forms must be identical
    # As we have no information about form frequency in these contexts in Dutch we assume there is
    # a non-doppel synonym with equal frequency to the doppel
    self$TA$addExample(av$Meaning,"Dutch", av$Doppel, ct=0.5);
    self$TA$addExample(av$Meaning,"Dutch", "Dnondoppel", ct=0.5);
  }
  r <- self$TA$constructDataTensor();
});

#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
# L2017Datame$runVariousLmMl
#
L2017Datame$set("public","runVariousLmMl", function(fromLm=0.0,toLm=1.0,nLm=100,
                                                    fromMl=0.0,toMl=1.0,nMl=100) {
  results <- data.frame(lm=c(),ml=c(),f=c(),s=c(),t=c(),p=c());
  for (i in 0:nLm) {
    if (nLm > 0) { languageMode <- fromLm + i * (toLm - fromLm) / nLm; }
    else { languageMode <- fromLm; }
    for (j in 0:nMl) {
      print(paste("i",i,"j",j));
      if (nMl > 0) { monitoringLevel <- fromMl + j * (toMl - fromMl) / nMl; }
      else { monitoringLevel <- fromMl; }
      self$TA$setLanguageMode(languageMode);
      self$TA$setMonitoringLevel(monitoringLevel);
      self$TA$make_p_f_st__bm();
      df <- self$TA$as.data.frame();
      df$lm <- languageMode;
      df$ml <- monitoringLevel;
      results <- rbind(results,df);
    }
  }
  return( results );
});

#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
# L2017Datame$get_1ab
#
L2017Datame$set("public","get_1ab", function() {
  self$lex__english_gaidhlig();
  df <- self$runVariousLmMl(fromMl=0.0,toMl=0.0,nMl=0);
  return( df );
});

#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
# L2017Datame$get_2
#
L2017Datame$set("public","get_2", function() {
  self$lex__dn_d();
  df <- self$runVariousLmMl(fromMl=0.0,toMl=0.0,nMl=0);
  a <- df[(df$f == 1)&(df$t == 1),]; a <- a[order(a$lm),]; # doppel
  b <- df[(df$f == 2)&(df$t == 1),]; b <- b[order(b$lm),]; # non-doppel
  a$pR <- a$p / b$p;
  a$ndp <- b$p;
  return( a );
});

#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
# L2017Datame$get_3b
#
L2017Datame$set("public","get_3a", function() {
  df <- data.frame(Language=c("French","French","French",
                              "English","English","English",
                              "Hesitation","Hesitation","Hesitation"),
                   Mode=c("Monolingual","Bilingual A","Bilingual B",
                          "Monolingual","Bilingual A","Bilingual B",
                          "Monolingual","Bilingual A","Bilingual B"),
                   Syllables=c(245,211,173,
                               5,   12, 25,
                               36,  27, 23))
  #
  df$XOrd <- 11
  df$XOrd[df$Mode=="Monolingual"] <- 1
  df$XOrd[df$Mode=="Bilingual A"] <- 10
  df$YOrd <- 11
  df$YOrd[df$Language=="French"] <- 1
  df$YOrd[df$Language=="English"] <- 10
  return( df );
});

#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
# L2017Datame$get_3b
#
L2017Datame$set("public","get_3b", function() {
  self$lex__n_n();
  df <- self$runVariousLmMl(fromMl=0.0,toMl=0.0,nMl=0);
  df <- df[df$Language == "French",];
  df$LanguageSource <- "French";
  df$LanguageSource[df$Form == "dog"] <- "English";
  df <- df[order(df$LanguageSource,df$lm,df$f),]
  fr <- df[df$LanguageSource == "French",];
  en <- df[df$LanguageSource == "English",];
  df$q <- df$p;
  df$q[df$LanguageSource == "English"] <- en$p + fr$p;
  #
  gradient <- (198 - 250) / 0.2;
  intercept <- 250 - 0.05 * gradient;
  df$pct <- df$p * (df$lm * gradient + intercept);
  df$qct <- df$q * (df$lm * gradient + intercept);
  #
  return( df );
});

#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
# L2017Datame$get_4a
#
L2017Datame$set("public","get_4a", function() {
  self$lex__n_n();
  df <- self$runVariousLmMl(fromLm=1.0,toLm=1.0,nLm=0);
  df <- df[df$Language == "French",];
  df$LanguageSource <- "French";
  df$LanguageSource[df$Form == "dog"] <- "English";
  df <- df[order(df$LanguageSource,df$ml,df$f),]
  fr <- df[df$LanguageSource == "French",];
  en <- df[df$LanguageSource == "English",];
  df$q <- df$p;
  df$q[df$LanguageSource == "English"] <- en$p + fr$p;
  #
  gradient <- (250 - 198) / 0.2;
  intercept <- 250 - 0.95 * gradient;
  df$pct <- df$p * (df$ml * gradient + intercept);
  df$qct <- df$q * (df$ml * gradient + intercept);
  return( df );
});

#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
# L2017Datame$get_4b
#
L2017Datame$set("public","get_4b", function(lm=1.0) {
  self$lex__dn_dn();
  df <- self$runVariousLmMl(fromLm=lm,toLm=lm,nLm=0);
  return( df );
});

#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
# L2017Datame$get_4c
#
L2017Datame$set("public","get_4c", function() {
  df <- self$get_4b( 2/3 );
  return( df );
});

#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
# L2017Datame$get_6
#
L2017Datame$set("public","get_6", function() {
  self$lex__dn_dn();
  df <- self$runVariousLmMl();
  return( df );
});

#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
# L2017Datame$get_7a
#
L2017Datame$set("public","get_7a", function() {
  data(EM2017_Responses);
  df <- aggregate(IsDoppel ~ ParticipantId + Condition,EM2017_Responses, mean);
  g <- ggplot();
  g <- g + geom_density(data=df, aes(IsDoppel, colour=Condition));
  plot( g );
  return( df );
});

#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
# L2017Datame$get_7b
#
L2017Datame$set("public","get_7b", function() {
  data(EM2017_Responses);
  #
  df <- aggregate(IsDoppel ~ StimulusId + Condition,EM2017_Responses, mean);
  #
  StimulusIds <- unique(df$StimulusId);
  StimulusIds <- StimulusIds[order(StimulusIds)];
  #
  dfs <- df[(df$Condition == "Bilingual")&(df$StimulusId == StimulusIds),];
  dfs$mIsDoppel <- df$IsDoppel[(df$Condition == "Monolingual")&(df$StimulusId == StimulusIds)];
  dfs$BiSubMonolingualDoppelRate <- dfs$IsDoppel - dfs$mIsDoppel;
  names(dfs)[names(dfs) == "IsDoppel"] <- "BilingualDoppelRate";
  names(dfs)[names(dfs) == "mIsDoppel"] <- "MonolingualDoppelRate";
  g <- ggplot();
  g <- g + geom_density(data=dfs, aes(BiSubMonolingualDoppelRate),colour="red");
  g <- g + geom_vline(xintercept=0.0,colour="black");
  plot( g );
  return( dfs );
});

#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
# L2017Datame$get_8
#
L2017Datame$set("public","get_8", function() {
  self$lex__experiment();
  FromScratch <- FALSE; # Change to TRUE to recalculate by model from scratch
  if (FromScratch) {
    df <- self$runVariousLmMl(); # Very slow - took me a day TME 08/09/2016
  } else {
    data(EM2017_ParameterFit);
    df <- EM2017_ParameterFit;
  }
  #  # To calculate from scratch, uncomment this line
  data(EM2017_Stimuli);
  data(EM2017_Responses);
  #
  StimulusIds <- unique(EM2017_Stimuli$StimulusId);
  StimulusIds <- StimulusIds[order(StimulusIds)];
  ParticipantCt <- length(unique(EM2017_Responses$ParticipantId[EM2017_Responses$Condition == "Bilingual"]));
  df <- df[(df$Language == "English"),];
  #
  df <- df[order(df$s,df$f,df$ml,df$lm),];
  #
  # TODO: Identify doppel vs non-doppel forms by stimulusId, work out propbability of actual occurrences
  df$IsDoppel <- 0;
  for (sid in StimulusIds) {
    s <- self$MonolingualAv[self$MonolingualAv$StimulusId == sid,];
    df$IsDoppel[(df$Meaning == s$Meaning)&(df$Form == s$Doppel)] <- 1;
  }
  information <- df[(df$f == 1),c("s","Meaning","ml","lm")];
  information$i <- 0.0;
  information$d_p <- 0;
  information$nd_p <- 0;
  information$d_i <- 0;
  information$nd_i <- 0;
  information$d_ct <- 0;
  information$nd_ct <- 0;
  Offset <- 0.0000000000001;
  #
  for (sid in StimulusIds) {
    if (sid == 36) next;
    s <- self$MonolingualAv[self$MonolingualAv$StimulusId == sid,];
    responses <- self$BilingualResponse[self$BilingualResponse$StimulusId == sid,];
    doppel_ct <- sum(responses$IsDoppel);
    non_doppel_ct <- ParticipantCt - doppel_ct;
    #
    dfs <- df[df$Meaning == s$Meaning,];
    doppel_p <- dfs$p[dfs$IsDoppel == 1];
    non_doppel_p <- dfs$p[dfs$Form == "Enondoppel"];
    doppel_i <- - log( doppel_p + Offset, 2);
    non_doppel_i <- - log( non_doppel_p + Offset, 2);
    print( paste("sid",sid,"Doppel length", length(doppel_i), "Doppel i", doppel_i, "Doppel ct", doppel_ct) );
    print( paste("sid",sid,"Non-doppel length", length(non_doppel_i), "Non-doppel i", non_doppel_i, "Non-doppel ct", non_doppel_ct) );
    i <- doppel_i * doppel_ct;
    i <- i + non_doppel_i * non_doppel_ct;
    #
    information$d_p[(information$Meaning == s$Meaning)] <- doppel_p;
    information$nd_p[(information$Meaning == s$Meaning)] <- non_doppel_p;
    information$d_i[(information$Meaning == s$Meaning)] <- doppel_i;
    information$nd_i[(information$Meaning == s$Meaning)] <- non_doppel_i;
    information$d_ct[(information$Meaning == s$Meaning)] <- doppel_ct;
    information$nd_ct[(information$Meaning == s$Meaning)] <- non_doppel_ct;
    information$i[(information$Meaning == s$Meaning)] <- i;
  }
  combined <- aggregate(i ~ lm + ml, information, sum);
  #
  StimulusId <- 22;
  gdf <- information[(information$s == StimulusId)&(information$lm == 0.0),];
  print(unique(information$Meaning[information$s == StimulusId]));
  gdf$mp <- self$MonolingualAv$IsDoppel[self$MonolingualAv$StimulusId == StimulusId];
  print(unique(self$MonolingualAv$Meaning[self$MonolingualAv$StimulusId == StimulusId]));
  g <- ggplot();
  g <- g + coord_cartesian(ylim=c(0.0,1.0));
  g <- g + geom_line(data=gdf,
                     aes(x=ml,y=d_p), colour="red");
  g <- g + geom_line(data=gdf,
                     aes(x=ml,y=d_ct/(d_ct+nd_ct)), colour="brown");
  g <- g + geom_line(data=gdf,
                     aes(x=ml,y=mp), colour="darkblue");
  plot( g );
  #
  #
  return( combined );
});

#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
# L2017Datame$get_9
#
L2017Datame$set("public","get_9", function(nT=25,lm=0.54,ml=1.0,A_d=100/101,A_n=1/101) {
  # Default lm, ml are best-fits from the model and experiment
  # nT=25; lm=0.53; ml=0.99; A_d=100/101; A_n=1/101;
  nT <- nT + 1;
  results <- data.frame(t=(1:nT)-1,p_d=rep(A_d,nT),p_n=rep(A_n,nT));
  #
  self$lex__dn_d(A_d=A_d,A_n=A_n);
  self$TA$setLanguageMode( lm ); # Set this to best fit from model
  self$TA$setMonitoringLevel( ml ); # Set this to best fit from model
  for (i in 2:nT) {
    self$TA$make_p_f_st__bm(); # No monitoring
    df <- self$TA$as.data.frame();
    results$p_d[i] = df$p[(df$Language=="English")&(df$Form=="foto")];
    results$p_n[i] = df$p[(df$Language=="English")&(df$Form=="picture")];
    self$lex__dn_d(A_d=results$p_d[i],A_n=results$p_n[i]);
  }
  return( results );
});

#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
# L2017Datame - Graphs for Language 2017 Paper on Rapid Lexical Divergence
# - each item associated with a relative frequency
# - no associative language interaction (as monolingual)
# - no monitoring
#
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
#
#' L2017
#'
#' This class contains methods for creating the graphs in Ellison & Miceli (2017 - Language 93(2):255-287).
#' These graphs are based on simulations from the classes \code{TensorAgent} and \code{PopulationSimulation}.
#' There is a method corresponding to each graph, which creates the graph in PNG format and writes it to the
#' current working directory.
#'
#' A further method uses the Student t test to evaluate the significance of the difference in distribution
#' between the monolingual and bilingual results from the experiment.
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @return An object of class L2017.
#' @return Object of \code{\link{L2017}} with methods for creating graphs for Ellison & Miceli (2017) published in Language 93(2):255-287.
#' @format \code{\link{R6Class}} object.
#' @section Methods:
#' \describe{
#'   \item{\code{$new()}}{Creates a new, empty lexicon object.}
#'   \item{\code{$draw_1ab(graph_number,language,side)}}{Draws graphs 1a and 1b, depending on arguments (see example).}
#'   \item{\code{$t_test()}}{Performs the Student t test comparing doppel rates between monolinguals and bilinguals in the experimental results.}
#'   \item{\code{$draw_2()}}{Draws graph 2.}
#'   \item{\code{$draw_3a()}}{Draws graph 3a.}
#'   \item{\code{$draw_3b()}}{Draws graph 3b.}
#'   \item{\code{$draw_4a()}}{Draws graph 4a.}
#'   \item{\code{$draw_4b(graphnum="4b")}}{Draws graph 4b.}
#'   \item{\code{$draw_4c()}}{Draws graph 4c.}
#'   \item{\code{$draw_6()}}{Draws graph 6.}
#'   \item{\code{$draw_7a()}}{Draws graph 7a.}
#'   \item{\code{$draw_7b()}}{Draws graph 7b.}
#'   \item{\code{$draw_8()}}{Draws graph 8.}
#'   \item{\code{$draw_9()}}{Draws graph 9.}
#'   \item{\code{$draw_10a(rx=0.2,ry=0.2,xc=0.5,yc=0.5,A=100,B=100,AB=100,}
#'         \code{linetypes=c("solid","dashed"),} \code{linecolours=c("red","green"),}
#'         \code{fillcolours=c("red","green"))}}{Draws graph 10a - graphs 10b, 10c build on this function, changing the default parameter values.}
#'   \item{\code{$draw_10b()}}{Draws graph 10b.}
#'   \item{\code{$draw_10c()}}{Draws graph 10c.}
#'   \item{\code{$draw_all()}}{Draws graphs all graphs from the paper.}
#' }
#' @examples
#' library(bldR)
#' self$t_test();
#' self$draw_1ab("1a","SG","left");
#' self$draw_1ab("1b","E","right");
#' self$draw_2();
#' self$draw_3a();
#' self$draw_3b();
#' self$draw_4a();
#' self$draw_4b();
#' self$draw_4c();
#' self$draw_6();
#' self$draw_7a();
#' self$draw_7b();
# self$draw_8(); # This can take a long time to run, uncomment when needed
#' self$draw_9();
#' self$draw_10a();
#' self$draw_10b();
#' self$draw_10c();
L2017 <- R6Class("L2017",
                       public = list(
                         G = NULL,
                         D = NULL,
                         initialize = function() {
                           set.seed( 2017 );
                           self$G <- L2017Graphme$new();
                           self$D <- L2017Datame$new();
                         }
                       )
);

#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
# L2017$get_1ab
#
L2017$set("public","t_test", function() {
  data(EM2017_Responses);
  r <- aggregate(IsDoppel ~ StimulusId + Condition, EM2017_Responses, sum);
  r <- r[order(r$StimulusId,r$Condition),];
  rm <- r[r$Condition == "Monolingual",];
  rb <- r[r$Condition == "Bilingual",];
  t.test(rm$IsDoppel,rb$IsDoppel,paired=TRUE)
});

#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
# L2017$get_1ab
#
L2017$set("public","setGraphDir", function(dir) {
  self$G$graphDir <- dir;
});

#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
# L2017$get_1ab
#
L2017$set("public","wrapg", function(g,
                                     x_lab,y_lab,
                                     xlim,ylim,
                                     lpos=c(0.7,0.8),
                                     linesize=2,
                                     addScale=TRUE) {
  g <- g + theme_bw();
  g <- g + xlab(x_lab);
  g <- g + ylab(y_lab);
  g <- g + theme(axis.ticks.length=unit(1,"mm"),
                 axis.ticks=element_line(size=1),
                 axis.text=element_text(size=18),
                 axis.title=element_text(size=20,face="bold")
  );
  g <- g + theme(legend.text=element_text(size=18),
                 legend.title=element_text(size=20,face="bold"),
                 legend.text.align = 0);
  g <- g + theme(legend.key.width=unit(2,"cm"),legend.key.height=unit(1.236,"cm"));
  if (linesize > 0.0) {
    g <- g + geom_line(size=linesize*self$G$scaler);
  }
  if (!is.null(lpos)) {
    g <- g + theme(legend.position = lpos);
  } else {
    g <- g + theme(legend.position = "none");
  }
  if( addScale ) {
    g <- g + scale_x_continuous(lim=xlim, expand = c(0,0));
    g <- g + scale_y_continuous(lim=ylim, expand = c(0,0));
  }
  g <- g + coord_cartesian(xlim = xlim, ylim = ylim);
  return( g );
});

#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
# L2017$get_1ab
#
L2017$set("public","draw_1ab", function(graphNumber,language,side) {
  df <- self$D$get_1ab();
  #
  if (self$G$isColour) {
    g <- ggplot(df[df$Language == language,], aes(x = lm, y = p, colour=Form));
  } else {
    g <- ggplot(df[df$Language == language,], aes(x = lm, y = p, linetype=Form));
  }
  xmargin <- 0.04;
  ymargin <- 0.02;
  g <- self$wrapg(g, "Language Mode", "Likelihood of Form",c(0.0-xmargin,1.0+xmargin),c(0.0-ymargin,1.0+ymargin));
  if (side == "right") {
    g <- g + theme(legend.position = "none");
    g <- g + scale_x_reverse(lim=c(1.0+xmargin,0.0-xmargin), expand = c(0,0));
    g <- ggdraw(switch_axis_position(g, axis = 'y'));
  } else {
  }
  # Plot in file
  self$G$writeGgplot2Png(graphNumber,paste("Gaidhlig-English-",side,".png",sep=""),g);
});

#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
# L2017$draw_2
#
L2017$set("public","draw_2", function() {
  df <- self$D$get_2();
  #
  g <- ggplot(data=df, aes(x = lm, y = pR));
  g <- self$wrapg(g, "Language Mode","P(doppel)/P(non-doppel)", c(-0.01,1.05),c(0.95,3.02), c(0.7,0.7));
  self$G$writeGgplot2Png("2","doppel-facilitation.png",g);
});

#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
# L2017$draw_3b4a
#
L2017$set("public","draw_3b4a", function(bardf,df,xlim,xlab,graphnum,filename) {
  cols <- c(French="darkgrey",English="black");
  linetypes <- c(French="dashed",English="solid");
  g <- ggplot();
  g <- g + theme_bw();
  g <- g + coord_cartesian(ylim = c(100,300));
  g <- g + scale_y_continuous(expand = c(0,0));
  if (xlim[1] < xlim[2]) {
    g <- g + scale_x_continuous( lim=xlim, expand = c(0,0) );
  } else {
    g <- g + scale_x_reverse( lim=xlim, expand = c(0,0) );
  }
  print(bardf);
  g <- g + geom_bar(data=bardf, aes(x=x, y=pct, fill=LanguageSource),colour="black",size=0.75,stat = "identity", width = 0.07);
  g <- g + scale_fill_manual(values = cols, guide = guide_legend(title = "Form Language"));
  g <- g + geom_line(data=df,aes(x=x, y=qct,linetype=LanguageSource), size=2*self$G$scaler);
  g <- g + scale_linetype_manual(values = linetypes, guide = guide_legend(title = "Form Language"));
  #
  g <- g + labs(title="Simulated Intrusion Rates",y="Expected Frequency",x=xlab);
  g <- g + theme(legend.position=c(0.6,0.9))
  g <- g + theme(axis.ticks.length=unit(1,"mm"),
                 axis.ticks=element_line(size=1),
                 axis.text=element_text(size=18),
                 axis.title=element_text(size=20,face="bold")
  );
  g <- g + theme(legend.text=element_text(size=18),
                 legend.title=element_text(size=20,face="bold"));
  tb_margin <- 18;
  g <- g + theme(plot.title=element_text(size=24,face="bold",margin=margin(b=tb_margin,t=tb_margin,unit="pt")));
  g <- g + theme(legend.key.width=unit(2,"cm"));
  self$G$writeGgplot2Png(graphnum,filename,g);
});


#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
# L2017$draw_3a
#
L2017$set("public","draw_3a", function() {
  df <- self$D$get_3a();
  df <- b$D$get_3a();
  df$x <- df$lm;
  #
  cols <- c(French="darkgrey",English="black",Hesitation="lightgrey");
  g <- ggplot();
  g <- g + theme_bw();
  g <- g + coord_cartesian(ylim = c(100,300));
  g <- g + scale_y_continuous(expand = c(0,0));
  g <- g + geom_bar(data=df, aes(x=reorder(Mode,XOrd), y=Syllables, fill=reorder(Language,-YOrd)),colour="black",size=0.75,stat = "identity", width = 0.75);
  g <- g + scale_fill_manual(values = cols, guide = guide_legend(title = "Form Language"));
  #
  g <- g + labs(title="Grosjean's 1997 Experiment",y="Syllable Count",x="Addressee");
  g <- g + theme(legend.position=c(0.6,0.88))
  g <- g + theme(axis.ticks.length=unit(1,"mm"),
                 axis.ticks=element_line(size=1),
                 axis.text=element_text(size=18),
                 axis.title=element_text(size=20,face="bold")
  );
  g <- g + theme(legend.text=element_text(size=18),
                 # legend.title=element_text(size=20,face="bold"));
                 legend.title=element_blank());
  tb_margin <- 18;
  g <- g + theme(plot.title=element_text(size=24,face="bold",margin=margin(b=tb_margin,t=tb_margin,unit="pt")));
  g <- g + theme(legend.key.width=unit(2,"cm"));
  b$G$writeGgplot2Png("3a","Grosjean-experiment-LanguageMode.png",g);
  #
  self$G$writeGgplot2Png("3a","Grosjean-experiment-LanguageMode.png",g);
  #
});

#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
# L2017$draw_3b
#
L2017$set("public","draw_3b", function() {
  df <- self$D$get_3b();
  df$x <- df$lm;
  #
  bardf <- df[(df$lm %in% c(0.05,0.15,0.25)),];
  bardf <- bardf[order(bardf$f),];
  print(bardf);
  #
  # self$
  self$draw_3b4a(bardf,df,xlim=c(-0.01,0.31),xlab="Language Mode",
                 graphnum="3b",filename="Grosjean-experiment-sim-LanguageMode.png");
});

#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
# L2017$draw_4a
#
L2017$set("public","draw_4a", function() {
  df <- self$D$get_4a();
  df$x <- df$ml;
  #
  bardf <- df[(df$ml %in% c(0.95,0.85,0.75)),];
  bardf <- bardf[order(bardf$f),];
  print(bardf);
  #
  # self$
  self$draw_3b4a(bardf,df,xlim=c(1.01,0.69),xlab="Monitoring Level",
                 graphnum="4a",filename="Grosjean-experiment-sim-MonitoringLevel.png");
});

#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
# L2017$draw_4b
#
L2017$set("public","draw_4b", function(graphnum="4b") {
  if (graphnum == "4b") df <- self$D$get_4b();
  if (graphnum == "4c") df <- self$D$get_4c();
  df <- df[df$Language == "English",];
  df$Form[df$Form == "other"]   <- "A";
  df$Form[df$Form == "second"]  <- "B";
  df$Form[df$Form == "tweede"]  <- "C";
  #
  if (self$G$isColour) {
    g <- ggplot(df, aes(x = ml, y = p, colour=Form));
    g <- g + scale_color_discrete(labels=c(B=expression(L[A]~Non-Doppel),
                                           A=expression(L[AB]~Doppel),
                                           C=expression(L[B]~Non-Doppel)));
  } else {
    g <- ggplot(df, aes(x = ml, y = p, linetype=Form));
    g <- g + scale_linetype_discrete(labels=c(B=expression(L[A]~Non-Doppel),
                                              A=expression(L[AB]~Doppel),
                                              C=expression(L[B]~Non-Doppel)));
    linetypes <- c(B="solid",A="dashed",C="dotted");
  }
  xmargin <- 0.04;
  ymargin <- 0.02;
  g <- self$wrapg(g, "Monitoring Level", "Likelihood of Form",c(0.0-xmargin,1.0+xmargin),c(0.0-ymargin,1.0+ymargin));
  #
  # Plot in file
  self$G$writeGgplot2Png(graphnum,"MonitoringOnly.png",g);
});

#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
# L2017$draw_4c
#
L2017$set("public","draw_4c", function() {
  self$draw_4b("4c");
});

#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
# L2017$draw_6
#
L2017$set("public","draw_6", function() {
  df <- self$D$get_6();
  df <- df[df$Language == "English",];
  df <- df[order(df$lm,df$ml,df$Form),];
  df_d <- df[df$Form == "other",];
  df_n <- df[df$Form == "second",];
  df_d$dP <- df_d$p / df_n$p;
  #
  #
  g <- ggplot(data=df_d,aes(x=lm,y=ml,fill=dP,z=dP));
  g <- g + geom_tile();
  if (self$G$isColour) {
    g <- g + scale_fill_gradient(limits = c(min(df_d$dP),max(df_d$dP)), low = 'red', high = 'blue');
  } else {
    g <- g + scale_fill_gradient(limits = c(min(df_d$dP),max(df_d$dP)), low = 'black', high = 'grey90');
  }
  g <- g + geom_contour(color = "white", alpha = 0.5);
  #
  g <- g + labs(title="",y="Monitoring Level",x="Language Mode");
  # g <- g + theme(legend.position=c(0.73,0.6))
  g <- g + theme(axis.ticks.length=unit(1,"mm"),
                 axis.ticks=element_line(size=1),
                 axis.text=element_text(size=18),
                 axis.title=element_text(size=20,face="bold")
  );
  g <- g + theme(legend.text=element_text(size=18),
                 legend.title=element_text(size=20,face="bold"));
  g <- g + theme(legend.key.width=unit(2,"cm"),legend.key.height=unit(1.236,"cm"));
  #
  self$G$widthMm <- 225;
  self$G$writeGgplot2Png("6","doppel-likelihood-ratio.png",g);
  self$G$widthMm <- 150;
});

#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
# L2017$draw_7a
#
L2017$set("public","draw_7a", function() {
  df <- self$D$get_7a();
  #
  g <- ggplot(data=df,aes(x=IsDoppel));
  if (self$G$isColour) {
    linecolours <- c(Monolingual="grey40",Bilingual="red");
    fillcolours <- alpha(linecolours,0.7); names(fillcolours) <- names(linecolours);
    linetypes <- c(Monolingual="solid",Bilingual="solid");
  } else {
    linecolours <- c(Monolingual="black",Bilingual="black");
    fillcolours <- c(Monolingual="grey80",Bilingual="grey50");
    linetypes   <- c(Monolingual="dotted",Bilingual="solid");
  }
  g <- g + scale_colour_manual(   name="Condition", values = linecolours );
  g <- g + scale_fill_manual(     name="Condition", values = fillcolours );
  g <- g + scale_linetype_manual( name="Condition", values = linetypes   );
  g <- g + geom_histogram(aes(y=..density..,fill=Condition),colour="black",position="dodge",binwidth=0.04);
  g <- g + geom_density(aes(colour=Condition,linetype=Condition,fill=Condition),size=1.0,adjust=0.5,alpha=0.3);
  #
  xmargin <- 0.1;
  ymargin <- 0.0;
  g <- self$wrapg(g, "Doppel Proportion", "Number of Participants",
                  c(min(df$IsDoppel)-xmargin,max(df$IsDoppel)+xmargin),c(0.0-ymargin,14.0+ymargin),c(0.3,0.75),
                  linesize=0);
  # Plot in file
  self$G$writeGgplot2Png("7a","DataByParticipant.png",g);
});

#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
# L2017$draw_7b
#
L2017$set("public","draw_7b", function() {
  df <- self$D$get_7b();
  #
  g <- ggplot(data=df,aes(x=BiSubMonolingualDoppelRate));
  if (self$G$isColour) {
    linecolour <- "red";
    fillcolour <- "red";
    fillcolourD <- "red";
  } else {
    linecolour <- "black";
    fillcolour <- "grey80";
    fillcolourD <- "black";
  }
  g <- g + geom_histogram(aes(y=..density..),colour=linecolour,binwidth=0.05,fill=fillcolour);
  g <- g + geom_density(colour=linecolour,size=1.0,adjust=0.5,fill=fillcolourD,alpha=0.3);
  g <- g + geom_vline(xintercept=0.0,linetype="dashed",size=1.0,colour="black");
  #
  xmargin <- 0.1;
  ymargin <- 0.0;
  g <- self$wrapg(g, "Difference in Doppel Proportion", "Number of Stimuli",
                  c(min(df$BiSubMonolingualDoppelRate)-xmargin,max(df$BiSubMonolingualDoppelRate)+xmargin),
                  c(0.0-ymargin,4.0+ymargin),c(0.3,0.75),linesize=0);
  # plot(g);
  # Plot in file
  self$G$writeGgplot2Png("7b","DataByStimulus.png",g);
});

#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
# L2017$draw_8
#
L2017$set("public","draw_8", function() {
  combined <- self$D$get_8();
  #
  bottom <- combined$i[(combined$lm == 0.0)&(combined$ml == 0.0)];
  combined$di <- bottom - combined$i;
  subcombined <- combined;
  subcombined$di <- subcombined$di * log(2,10);
  subcombined <- subcombined[subcombined$di > 0.0,];
  g <- ggplot(data=subcombined,aes(x=lm,y=ml,fill=di,z=di));
  g <- g + geom_tile();
  if (self$G$isColour) {
    g <- g + scale_fill_gradient(name=expression(log[10](P[C]/P["2M"])),
                                 limits = c(min(subcombined$di),max(subcombined$di)),
                                 low = 'red', high = 'blue');
  } else {
    g <- g + scale_fill_gradient(name=expression(log[10](P[C]/P["2M"])),
                                 limits = c(min(subcombined$di),max(subcombined$di)),
                                 low = 'black', high = 'grey90');
  }
  g <- g + geom_contour(color = "white", alpha = 0.5);
  #
  g <- g + labs(title="",y="Monitoring Level",x="Language Mode");
  # plot( g );
  g <- g + theme(legend.position=c(0.7,0.4))
  g <- g + theme(axis.ticks.length=unit(1,"mm"),
                 axis.ticks=element_line(size=1),
                 axis.text=element_text(size=18),
                 axis.title=element_text(size=20,face="bold")
  );
  g <- g + theme(legend.text=element_text(size=18),
                 legend.title=element_text(size=20,face="bold"));
  g <- g + theme(legend.key.width=unit(2,"cm"),legend.key.height=unit(1.236,"cm"));
  #
  self$G$writeGgplot2Png("8","parameter-fit.png",g);
});

#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
# L2017$draw_9
#
L2017$set("public","draw_9", function() { # S-shaped curve
  nT <- 26;
  df <- self$D$get_9( nT=nT );
  dfx <- data.frame(t=df$t,y=df$p_d,Form="Doppel");
  dfx <- rbind(dfx,data.frame(t=df$t,y=df$p_n,Form="Non-Doppel"));
  #
  if (self$G$isColour) {
    g <- ggplot(dfx, aes(x = t, y = y, colour=Form));
  } else {
    g <- ggplot(dfx, aes(x = t, y = y, linetype=Form));
  }
  xmargin <- 0.04;
  ymargin <- 0.02;
  g <- self$wrapg(g, "Generation", "Likelihood of Form",c(0.0-xmargin,nT+xmargin),c(0.0-ymargin,1.0+ymargin),
                  lpos=NULL);
  # Plot in file
  self$G$widthMm <- 225;
  self$G$writeGgplot2Png("9","s-shaped-doppel-loss.png",g);
  self$G$widthMm <- 150;

});

#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
# L2017$draw_overlapping_circles
#
L2017$set("public","draw_overlapping_circles", function(rx=0.2,ry=0.2,xc=0.5,yc=0.5,
                                                        A=100,B=100,AB=100,
                                                        linetypes=c("solid","dashed"),
                                                        linecolours=c("red","green"),
                                                        fillcolours=c("red","green")) {
  r1multiplier <- sqrt((A+AB)/150); r2multiplier <- sqrt((B+AB)/150);
  r1x <- rx * r1multiplier; r1y <- ry * r1multiplier;
  r2x <- rx * r2multiplier; r2y <- ry * r2multiplier;
  overlap_pc <- 100 * AB / (A + B + AB);
  gg_circle <- function(rx,ry, xc, yc, color="black", fill=NA, ...) {
    x <- xc + rx*cos(seq(0, pi, length.out=100))
    ymax <- yc + ry*sin(seq(0, pi, length.out=100))
    ymin <- yc + ry*sin(seq(0, -pi, length.out=100))
    return( annotate("ribbon", x=x, ymin=ymin, ymax=ymax, color=color, fill=fill, ...) );
  }
  r1 <- r1x; r2 <- r2x;
  theta1 <- pi*(1:1000)/1000;
  w <- r1*sin(theta1/2)/r2;
  x <- which(w <= 1.0);
  theta1 <- theta1[x]; w <- w[x];
  theta2 <- 2 * asin(w);
  a1 <- r1^2*(theta1 - sin(theta1))/2;
  a2 <- r2^2*(theta2 - sin(theta2))/2;
  dx1 <- r1*cos(theta1 / 2); dx2 <- r2*cos(theta2 / 2);
  #
  theta1 <- c(theta1, theta1);
  theta2 <- c(theta2,2*pi - theta2);
  a1 <- c(a1, a1);
  a2 <- c(a2,pi*r2*r2 - a2);
  dx1 <- c(dx1, dx1);
  dx2 <- c(dx2,- dx2);
  intersection_area <- a1+a2;
  union_area <- pi*r1^2 + pi*r2^2 - intersection_area;
  df <- data.frame(theta1=theta1,theta2=theta2, dx1=dx1,dx2=dx2,
                   overlap_ratio = intersection_area / union_area);
  df$disparity <- abs(df$overlap_ratio - overlap_pc/100);
  dfs <- df[df$disparity == min(df$disparity),];
  sep <- (r2 + dfs$dx2[1]) + (r1 + dfs$dx1[1]);
  xc1 <- xc - sep/2 + r1; xc2 <- xc + sep/2 - r2;
  annotations <- c( gg_circle(r1x,r1y,xc1,yc,color=linecolours[1],linetype=linetypes[1],fill=fillcolours[1],
                              size=2,alpha=0.5),
                    gg_circle(r2x,r2y,xc2,yc,color=linecolours[2],linetype=linetypes[2],fill=fillcolours[2],
                              size=2,alpha=0.5) );
  return( annotations );
});

#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
# L2017$draw_10a
#
L2017$set("public","draw_10a", function(graphNumber="10a",A=150,B=150,AB=0,nG=35) {
  # graphNumber="10a";A=150;B=150;AB=0; nG <- 35;
  ps <- PopulationSimulation$new();
  ps$setPopulationStructure(A,B,AB);
  ps$setNumberOfGenerations(nG);
  ps$setMonitoringLevel(1.0);
  ps$setLanguageMode(0.54);
  ps$setSamplesPerAgent(100);
  ps$setLexicon();
  ps$simulate(exact=FALSE);
  #
  data <- aggregate(p ~ Form + Language + g, data=ps$Trace, FUN=mean);
  #
  g <- ggplot();
  if (self$G$isColour) {
    linecolours <- c(A="green",B="red");
    fillcolours <- c(A="green",B="red");
    linetypes <- c(A="solid",B="solid");
  } else {
    linecolours <- c(A="black",B="black");
    fillcolours <- c(A="grey40",B="grey60");
    linetypes   <- c(A="dotted",B="solid");
  }
  categories <- c(expression(L[A]),expression(L[B]));
  g <- g + scale_colour_manual(   name="Language", values = linecolours, labels = categories );
  g <- g + scale_fill_manual(     name="Language", values = fillcolours, labels = categories );
  g <- g + scale_linetype_manual( name="Language", values = linetypes,   labels = categories );
  g <- g + geom_line(data=data[(data$Form == "d"),], aes(x=g,y=p,colour=Language,linetype=Language), size=1.5);
  r <- 0.125;
  circles <- self$draw_overlapping_circles( 35*r,r, 17.5,0.75,
                                            A,B,AB,
                                            linetypes=linetypes,linecolours=linecolours,
                                            fillcolours=fillcolours);
  g <- g + circles[1]; g <- g + circles[2];
  g <- g + ylim(c(0,1)) + xlim(c(0,max(data$g)));
  # plot(g);
  #
  xmargin <- 0.1;
  ymargin <- 0.0;
  legend_pos <- c(0.5,0.25);
  if (graphNumber != "10a") {
    legend_pos <- NULL;
    # g <- g + theme(legend.position = "none");
  }
  g <- self$wrapg(g, "Generation", "Doppel Proportion",
                  c(min(data$g)-xmargin,max(data$g)+xmargin),c(0.0-ymargin,1.0+ymargin),
                  legend_pos,linesize=2, addScale=FALSE);
  # Plot in file
  self$G$writeGgplot2Png(graphNumber,"PopulationSimulation.png",g);
});

#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
# L2017$draw_10b
#
L2017$set("public","draw_10b", function() {
  self$draw_10a("10b",150,0,150);
});

#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
# L2017$draw_10c
#
L2017$set("public","draw_10c", function() {
  self$draw_10a("10c",100,100,100);
});

#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
# L2017$draw_all
#
L2017$set("public","draw_all", function() {
  self$t_test();
  self$draw_1ab("1a","SG","left");
  self$draw_1ab("1b","E","right");
  self$draw_2();
  self$draw_3a();
  self$draw_3b();
  self$draw_4a();
  self$draw_4b();
  self$draw_4c();
  self$draw_6();
  self$draw_7a();
  self$draw_7b();
  self$draw_8();
  self$draw_9();
  self$draw_10a();
  self$draw_10b();
  self$draw_10c();
});
