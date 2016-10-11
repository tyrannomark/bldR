#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
# TODOS:
#
require(R6);

#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
# PopulationSimulation - Multi-Agent Population Simulations
# - each item associated with a relative frequency
# - no associative language interaction (as monolingual)
# - no monitoring
#
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
#
#' Class defining a meaning-form frequency distribution.
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @return An object of class PopulationSimulation.
#' @return Object of \code{\link{PopulationSimulation}} with methods for drawing graphs according suitable for final version of the paper.
#' @format \code{\link{R6Class}} object.
#' @field meanings Stores a list of distinct meanings.
#' @field forms Stores a list of distinct forms.
#' @field meanings2 Stores a list of meanings with repeats if more than one matching form.
#' @field forms2 Stores a list of forms with repeats if more than one matching meaning.
#' @field frequencies Stores the total frequency supplied for this meaning-form pair.
#' @section Methods:
#' \describe{
#'   \item{Documentation}{Presents the meanings and uses of \code{PopulationSimulation}'s methods.}
#'   \item{\code{new()}}{Creates a new, empty lexicon object.}
#'   \item{\code{addItemInstance(meaning,form,ct=1)}}{Adds a new \code{meaning}-\code{form} pair to the lexicon (if none already exists). It increments the count for that pair by \code{ct}.}
#'   \item{\code{makeFormDistribution(meaning)}}{Returns the probability distribution over forms matching the argument \code{meaning}.}
#'   \item{\code{selectForm(meaning)}}{Selects a single form according to the probability distribution matching the argument \code{meaning}.}
#' }
#' @examples
#' L1 <- Lexicon$new();
#' L1$addItemInstance("BAG","sac",5);
#' L1$addItemInstance("FISH","peche",3);
#' L1$addItemInstance("BAG","valise",11);
#' print( L1aA$frequencies );
#' print( L1$makeFormDistribution("BAG") );
#' print( L1$selectByDistribution("BAG") );
PopulationSimulation <- R6Class("PopulationSimulation",
                        public = list(
                          LanguageMode = 0.0,
                          MonitoringLevel = 0.0,
                          Population_A = 100,
                          Population_B = 100,
                          Population_AB = 100,
                          Population = 300,
                          SamplesPerAgent = 100,
                          NumberOfGenerations = 10,
                          Agents = NULL,
                          Agents_A = NULL,
                          Agents_B = NULL,
                          Trace = NULL,
                          initialize = function() {
                            require(tensorA);
                            self$Agents = c();
                            self$Agents_A = c();
                            self$Agents_B = c();
                          }
                        )
);

#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
# PopulationSimulation$setPopulation
#
PopulationSimulation$set("public","setPopulationStructure", function(A=100,B=100,AB=0) {
  self$Population_A <- A;
  self$Population_B <- B;
  self$Population_AB <- AB;
  self$Population <- A + B + AB;
  #
  self$Agents_A <- c();
  self$Agents_B <- c();
  self$Agents <- c();
  a <- c();
  if (A >= 1) a <- 1:A;
  for (i in a) {
    ta <- TensorAgent$new();
    self$Agents_A <- c(self$Agents_A, ta);
    self$Agents <- c(self$Agents, ta);
  }
  b <- c();
  if (B >= 1) b <- 1:B;
  for (i in b) {
    ta <- TensorAgent$new();
    self$Agents_B <- c(self$Agents_B, ta);
    self$Agents <- c(self$Agents, ta);
  }
  ab <- c();
  if (AB >= 1) ab <- 1:AB;
  for (i in ab) {
    ta <- TensorAgent$new();
    self$Agents_A <- c(self$Agents_A, ta);
    self$Agents_B <- c(self$Agents_B, ta);
    self$Agents <- c(self$Agents, ta);
  }
  for (a in self$Agents) {
    # a$ensureLanguage("A");
    # a$ensureLanguage("B");
    a$setLanguageMode( self$LanguageMode );
    a$setMonitoringLevel( self$MonitoringLevel );
  }
});

#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
# PopulationSimulation$setLanguageMode
#
PopulationSimulation$set("public","setLanguageMode", function(lm) {
  self$LanguageMode <- lm;
  for (agent in self$Agents) {
    agent$setLanguageMode( lm );
  }
});

#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
# PopulationSimulation$setMonitoringLevel
#
PopulationSimulation$set("public","setMonitoringLevel", function(ml) {
  self$MonitoringLevel <- ml;
  for (agent in self$Agents) {
    agent$setMonitoringLevel( ml );
  }
});

#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
# PopulationSimulation$setSamplesPerAgentPerMeaning
#
PopulationSimulation$set("public","setSamplesPerAgent", function(ns) {
  self$SamplesPerAgent <- ns;
});

#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
# PopulationSimulation$setNumberOfGenerations
#
PopulationSimulation$set("public","setNumberOfGenerations", function(nG=10) {
  self$NumberOfGenerations <- nG;
});

#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
# PopulationSimulation$clearLexicon
#
PopulationSimulation$set("public","clearLexicon", function() {
  for (a in self$Agents) a$clearLexicon();
  # for (a in self$Agents) a$ensureLanguage("A");
  # for (a in self$Agents) a$ensureLanguage("B");
});

#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
# PopulationSimulation$constructDataTensor
#
PopulationSimulation$set("public","constructDataTensor", function() {
  for (a in self$Agents) a$constructDataTensor();
});

#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
# PopulationSimulation$make_p_f_st__bm
#
PopulationSimulation$set("public","make_p_f_st__bm", function() {
  for (a in self$Agents) a$make_p_f_st__bm();
});

#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
# PopulationSimulation$setLexicon
#
PopulationSimulation$set("public","setLexicon", function(A_d=0.5,A_nA=0.5,A_nB=0.0,
                                                         B_d=0.5,B_nA=0.0,B_nB=0.5) {
  self$clearLexicon();
  for (a in self$Agents_A) a$addExample("MEANING","A","d", A_d);
  for (a in self$Agents_A) a$addExample("MEANING","A","An",A_nA);
  for (a in self$Agents_A) a$addExample("MEANING","A","Bn",A_nB);
  for (b in self$Agents_B) b$addExample("MEANING","B","d", B_d);
  for (b in self$Agents_B) b$addExample("MEANING","B","An",B_nA);
  for (b in self$Agents_B) b$addExample("MEANING","B","Bn",B_nB);
  self$constructDataTensor();
  self$make_p_f_st__bm();
});

#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
# PopulationSimulation$sample
#
PopulationSimulation$set("public","sampleX", function(exact=FALSE,agentSet="A") {
  t <- 1;
  if (agentSet == "B") t <- 2;
  agents <- self[[paste("Agents",agentSet,sep="_")]];
  l <- length(agents);
  #
  tr <- to.tensor(rep(0,3),c(f=3,t=1,s=1));
  for (parent in agents) {
    this_t <- parent$Languages[agentSet];
    p_f_s__bm <- parent$p_f_st__bm[[t=this_t]];
    tr <- add.tensor(tr,p_f_s__bm);
  }
  tr <- tr / l;
  #
  if (exact) {
    z <- data.frame(d=rep(0,l),nA=rep(0,l),nB=rep(0,l),t=rep(0,l),i=rep(0,l),a=rep("",l),stringsAsFactors = FALSE);
    # print(z[1,1]);
    for (i in 1:length(agents)) {
      z[i,"d"] <- tr[f=1,t=1,s=1];
      z[i,"nA"] <- tr[f=2,t=1,s=1];
      z[i,"nB"] <- tr[f=3,t=1,s=1];
      z[i,"t"] <- t;
      z[i,"i"] <- i;
      z[i,"a"] <- agentSet;
    }
    # print(z[1,1]);
    return( z );
  }
  prob_d  = tr[f=1,t=1,s=1][[1]]; # doppel
  prob_nA = tr[f=2,t=1,s=1][[1]]; # language internal non-doppel
  prob_nB = tr[f=3,t=1,s=1][[1]]; # other language non-doppel
  d_samples = rbinom(length(agents),size=self$SamplesPerAgent,prob=prob_d);
  z <- data.frame(d=rep(0,l),nA=rep(0,l),nB=rep(0,l),t=rep(0,l),i=rep(0,l),a=rep("",l),stringsAsFactors = FALSE);
  for (i in 1:length(agents)) {
    d_ct <- d_samples[i];
    p <- prob_nA/(prob_nA+prob_nB);
    if (p == 0.0) {
      nA_ct <- 0; nB_ct <- self$SamplesPerAgent - d_ct;
    } else if (p == 1.0) {
      nB_ct <- 0; nA_ct <- self$SamplesPerAgent - d_ct;
    } else {
      # print(paste("N",self$SamplesPerAgent-d_ct,p));
      nA_ct <- rbinom(1,size=self$SamplesPerAgent-d_ct,prob=p);
      nB_ct <- self$SamplesPerAgent - d_ct - nA_ct;
    }
    # z[i,] <- c(d_ct,nA_ct,nB_ct,t=t,i=i,a=agentSet);
    z[i,"d"] <- d_ct;
    z[i,"nA"] <- nA_ct;
    z[i,"nB"] <- nB_ct;
    z[i,"t"] <- t;
    z[i,"i"] <- i;
    z[i,"a"] <- agentSet;
  }
  return( z );
});

#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
# PopulationSimulation$sample
#
PopulationSimulation$set("public","sampleOld", function(exact=FALSE) {
  df <- self$sampleX(exact=exact,agentSet="A");
  df <- rbind(df, self$sampleX(exact=exact,agentSet="B"));
  return( df );
});

#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
# PopulationSimulation$productionDistribution
#
PopulationSimulation$set("public","productionDistribution", function() {
  m <- matrix(rep(0,8),ncol=4,nrow=2);
  colnames(m) <- c("d","nA","nB","ct");
  rownames(m) <- c("A","B");
  for (agent in self$Agents) {
    tr <- agent$p_f_st__bm;
    # print(dim(tr)); print(names(agent$Languages));
    for (language in names(agent$Languages)) {
      t <- agent$Languages[[language]]; # print(t);
      m[language,"d"]  <- m[language,"d"]  + tr[f=1,s=1,t=t] / agent$NumberOfLanguages;
      m[language,"nA"] <- m[language,"nA"] + tr[f=2,s=1,t=t] / agent$NumberOfLanguages;
      m[language,"nB"] <- m[language,"nB"] + tr[f=3,s=1,t=t] / agent$NumberOfLanguages;
      m[language,"ct"] <- m[language,"ct"] + 1.0 / agent$NumberOfLanguages;
    }
  }
  for (language in rownames(m)) {
    m[language,] <- m[language,] / m[language,"ct"];
  }
  return( m );
});

#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
# PopulationSimulation$setDistribution
#
PopulationSimulation$set("public","sampleFromDistribution", function(distribution,exact=FALSE) {
  lA <- length(self$Agents_A);
  lB <- length(self$Agents_B);
  m <- matrix(nrow=lA+lB,ncol=3);
  colnames(m) <- c("d","nA","nB");
  epsilon <- 10^-12;
  #
  #
  for (l in 1:2) { # language
    e <- lA; b <- 1;
    if (l > 1) { e <- lA+lB; b <- lA+1; }
    epsilonv <- rep(epsilon,e-b+1);
    if (exact) {
      m[b:e,"d"]  <- distribution[l,"d"];
      m[b:e,"nA"] <- distribution[l,"nA"];
      m[b:e,"nB"] <- distribution[l,"nB"];
    } else {
      n <- rep(self$SamplesPerAgent,e-b+1);
      k1 <- rep(1,e-b+1);
      p <- distribution[l,"d"];
      p <- pmax(p,epsilonv); p <- pmin(p,1-epsilonv);
      print(paste("A: ", l, min(p),max(p),min(n),max(n)));
      m[b:e,"d"]  <- rbinom(k1,n,p); n <- n - m[b:e,"d"];
      # print(paste(l,mean(m[b:e,"d"]),distribution[l,"d"], p, mean(n)));
      p <- distribution[l,"nA"] / (1.0 - distribution[l,"d"]);
      p <- pmax(p,epsilonv); p <- pmin(p,1-epsilonv);
      print(paste("B: ", l, min(p),max(p),min(n),max(n)));
      m[b:e,"nA"] <- rbinom(k1,n,p); n <- n - m[b:e,"nA"];
      # print(paste(l,mean(m[b:e,"nA"]),distribution[l,"nA"], p, mean(n)));
      p <- 1.0;
      m[b:e,"nB"] <- n;
      # print(paste(l,mean(m[b:e,"nB"]),distribution[l,"nB"], p, mean(n)));
    }
  }
  return( m );
});

#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
# PopulationSimulation$setDistribution
#
PopulationSimulation$set("public","setDistribution", function(samples) {
  i <- 1;
  # print("In setDistribution")
  for (a in self$Agents_A) {
    # print(paste("setDistribution A i",i,samples[i,"d"],samples[i,"nA"],samples[i,"nB"]));
    a$addExample("MEANING","A","d",  samples[i,"d"]);
    a$addExample("MEANING","A","nA", samples[i,"nA"]);
    a$addExample("MEANING","A","nB", samples[i,"nB"]);
    i <- i+1;
  }
  for (b in self$Agents_B) {
    # print(paste("setDistribution B i",i,samples[i,"d"],samples[i,"nA"],samples[i,"nB"]));
    b$addExample("MEANING","B","d",  samples[i,"d"]);
    b$addExample("MEANING","B","nA", samples[i,"nA"]);
    b$addExample("MEANING","B","nB", samples[i,"nB"]);
    i <- i+1;
  }
});

#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
# PopulationSimulation$getTrace
#
PopulationSimulation$set("public","getTrace", function(g) {
  popdf <- data.frame(f=c(),s=c(),t=c(),p=c(),i=c());
  for (a_i in 1:length(self$Agents)) {
    a <- self$Agents[a_i];
    df <- a[[1]]$as.data.frame();
    df$i <- a_i;
    popdf <- rbind(popdf,df);
  }
  popdf$g <- g;
  return( popdf );
});

#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
# PopulationSimulation$simulate
#
PopulationSimulation$set("public","simulate", function(exact=FALSE) {
  distribution <- matrix(c(0.55,0.50,
                           0.45,0.00,
                           0.00,0.50,
                           1.00,1.00),ncol=4,nrow=2);
  colnames(distribution) <- c("d","nA","nB","ct");
  rownames(distribution) <- c("A","B");
  #
  # Initialise the population with this distribution
  lm <- self$LanguageMode; ml <- self$MonitoringLevel;
  self$setLanguageMode(0.0); self$setMonitoringLevel(0.0);
  samples <- self$sampleFromDistribution(distribution, exact);
  self$clearLexicon();
  self$setDistribution(samples );
  self$constructDataTensor();
  self$make_p_f_st__bm();
  #
  # Start recording
  self$Trace <- self$getTrace(0);
  for (g in 1:self$NumberOfGenerations) {
    distribution <- self$productionDistribution();
    self$setLanguageMode(lm); self$setMonitoringLevel(ml); # Turn on language mode, monitoring
    print(distribution);
    #
    samples <- self$sampleFromDistribution(distribution, exact);
    self$clearLexicon();
    self$setDistribution(samples );
    self$constructDataTensor();
    self$make_p_f_st__bm();
    #
    self$Trace <- rbind(self$Trace, self$getTrace(g));
    # print(self$Trace);
    print(paste("Generation",g,"done"));
  }
  return( self$Trace );
});
