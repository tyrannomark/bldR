require(R6)
require(tensorA);

#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
# TensorModel - an implementation of Ellison & Miceli (2017)
#   model of multi-/bi-lingual lexical selection
# Most uptodate versions available from http://bld.markellison.net/
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
#
#' TensorAgent: a model of bilingual lexical selection.
#'
#' This class implements the model of individual lexical selection described in Ellison & Miceli (2017 - Language 93(2):255-287) - hereafter EM. It is implemented using the TensorA, because that class facilitates linear calculations parameterised over a variable number of parameter dimensions.
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @return Object of class \code{\link{TensorAgent}}.
#' @format \code{\link{R6Class}} object.
#' @section Fields:
#' \describe{
#'   \item{Documentation}{Here are \code{TensorModel}'s fields. EM abbreviates Ellison & Miceli (2017).}
#'   \item{\code{$LanguageMode}}{This is the language mode parameter on a scale \eqn{[0,1]}.}
#'   \item{\code{$MonitoringLevel}}{The is the effort put into monitoring on a scale \eqn{[0,1]}.}
#'   \item{\code{$Meanings}}{The meanings currently in the lexicon (a list of strings).}
#'   \item{\code{$Languages}}{The languages modelled in the lexicon (a list of strings).}
#'   \item{\code{$NumberOfLanguages}}{The number of languages modelled in the lexicon (integer).}
#'   \item{\code{$LexicalTensor}}{The tensor with the frequency of occurrence for each language-meaning-form combination.}
#'   \item{\code{$version}}{An integer tracking whether updates to values to avoid repeated calculation of the same values.}
#'   \item{\code{$delta_lt}}{Kronecker delta (1 if \eqn{l=t} zero otherwise), see Equation 2 in EM.}
#'   \item{\code{$p_l_t__b}}{Probability of using language \eqn{l} given a target language \eqn{t} and language mode \eqn{b}.}
#'   \item{\code{$p_f_sl}}{Probability of using form \eqn{f} to represent meaning \eqn{s} in language \eqn{l}, see Equation 3 in EM.}
#'   \item{\code{$p_f_st__b}}{Probability of using form \eqn{f} to representing meaning \eqn{s} when trying to use language \eqn{t} and the bilingual mode is \eqn{b}, see Equation 4 in EM.}
#'   \item{\code{$p_l_fst__bm}}{Probability of identifying language \eqn{l} as the source of form \eqn{f} when trying to express meaning \eqn{s} in language \eqn{t}, see Equation 8 in EM.}
#'   \item{\code{$p_f_st__bm}}{Probability of using form \eqn{f} to express meaning \eqn{s} when aiming to speak language \eqn{t}, given bilingual mode \eqn{b} and monitoring level \eqn{m}, see Equation 11 in EM.}
#'   \item{\code{$p_l_t__b_version}}{The version number associated with the current value of \code{p_l_t__b}.}
#'   \item{\code{$p_f_sl_version}}{The version number associated with the current value of \code{p_f_sl}.}
#'   \item{\code{$p_f_st__b_version}}{The version number associated with the current value of \code{p_st__b}.}
#'   \item{\code{$p_l_fst__bm_version}}{The version number associated with the current value of \code{p_l_fst__bm}.}
#'   \item{\code{$p_f_st__bm_version}}{The version number associated with the current value of \code{p_f_st__bm}.}
#' }
#' @section Methods:
#' \describe{
#'   \item{\code{$new()}}{Creates a new, empty \code{TensorModel} object.}
#'   \item{\code{$clearLexicon()}}{Empties the lexical memory, removing all meaning-language-form triples.}
#'   \item{\code{$setLanguageMode(languageMode)}}{Set the level of interaction between target and other languages.}
#'   \item{\code{$setMonitoringLevel(monitoringLevel)}}{Set the level of monitoring exerted by the agent.}
#'   \item{\code{$addExample(meaning,language,form,ct=1)}}{Adds meaning-language-form triple to the model if it does not exist already. It then adds \code{ct} to the frequency recorded for this triple.}
#'   \item{\code{$normalise(t,overIndices)}}{Normalise a tensor \code{t} over some vector of indices \code{overIndices}.}
#'   \item{\code{$constructDataTensor()}}{Uses the tuples entered using \code{$addExample(..)} to build a tensor representing the distribution over experienced meaning-language-form combinations.}
#'   \item{\code{$makeMeLanguagePairs()}}{Constructs a rank-2 tensor, both of whose indices range over languages, and whose values for each dimension combination is 1.0.}
#'   \item{\code{$make_p_l_t__b()}}{Calculate the probability of a language given a target language.}
#'   \item{\code{$make_p_f_sl()}}{Calculate the probability of using form \eqn{f} to represent meaning \eqn{s} in language \eqn{l}.}
#'   \item{\code{$make_p_f_st__b()}}{Calculate the probability of using form \eqn{f} to representing meaning \eqn{s} when trying to use language \eqn{t} and the bilingual mode is \eqn{b}.}
#'   \item{\code{$make_p_l_fst__bm()}}{Calculate the probability of identifying language \eqn{l} as the source of form \eqn{f} when trying to express meaning \eqn{s} in language \eqn{t}.}
#'   \item{\code{$make_p_f_st__bm()}}{Calculate the probability of using form \eqn{f} to express meaning \eqn{s} when aiming to speak language \eqn{t}, given bilingual mode \eqn{b} and monitoring level \eqn{m}.}
#'   \item{\code{$as.data.frame()}}{Create a data frame with columns \code{Meaning}, \code{Language}, \code{Form} and probability of form \code{p}, giving the distribution \code{p_f_st__bm}.}
#' }
#' @examples
#' library(bldR)
#' ta <- TensorAgent$new();
#' ta$clearLexicon();
#' ta$addExample("PHOTO","English","foto",    ct=0.5);
#' ta$addExample("PHOTO","English","picture", ct=0.5);
#' ta$addExample("PHOTO","Dutch",  "foto",    ct=1.0);
#' ta$constructDataTensor();
#' ta$setLanguageMode(0.4);
#' ta$setMonitoringLevel(0.8);
#' ta$make_p_f_st__bm();
#' df <- ta$as.data.frame();
#' print(df);
#
TensorAgent <- R6Class("TensorAgent",
                       public = list(
                        LanguageMode = 0.0,
                        MonitoringLevel = 0.0,
                        Meanings = c(),
                        Languages = c(),
                        NumberOfLanguages = 0,
                        Forms = c(),
                        ExampleCt = c(),
                        meLanguages = NULL,
                        meLanguagePairs = NULL,
                        LexicalTensor = NULL,
                        version = 0,
                        delta_lt = NULL,
                        p_l_t__b = NULL,
                        p_f_sl = NULL,
                        p_f_st__b = NULL,
                        p_l_fst__bm = NULL,
                        p_f_st__bm = NULL,
                        p_l_t__b_version = 0,
                        p_f_sl_version = 0,
                        p_f_st__b_version = 0,
                        p_l_fst__bm_version = 0,
                        p_f_st__bm_version = 0,
                        initialize = function() {
                        }
                       )
);

#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
# TensorAgent$pfilter
# - Collapse two indices by matching them together
TensorAgent$set("public","pFilter", function(t,fromIndex,toIndex) {
  indexLength <- dim(t)[fromIndex];
  for (i in 1:indexLength) {
    z <- slice.tensor(t, fromIndex, i);
    z <- slice.tensor(z, toIndex,   i);
    if (i == 1) { t_out <- z; }
    else { t_out <- bind.tensor(t_out, toIndex, z); }
  }
  t_out <- margin.tensor(t_out, fromIndex);
  return( t_out );
});

#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
# TensorAgent$setLanguageMode
# - Set the level of interaction between language lexica
TensorAgent$set("public","setLanguageMode", function(languageMode) {
  self$LanguageMode <- languageMode;
  self$version <- self$version + 1;
});

#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
# TensorAgent$setMonitoringLevel
# - Set the effort expended on language monitoring
TensorAgent$set("public","setMonitoringLevel", function(monitoringLevel) {
  self$MonitoringLevel <- monitoringLevel;
  self$version <- self$version + 1;
});

#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
# TensorAgent$clearLexicon
# - Ensure that the given meaning is indexed in the lexicon
TensorAgent$set("public","clearLexicon", function() {
  self$Meanings <- c();
  self$Languages <- c();
  self$Forms <- c();
  self$ExampleCt <- c();
  self$LexicalTensor <- NULL;
  self$version <- self$version + 1;
});

#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
# TensorAgent$ensureMeaning
# - Ensure that the given meaning is indexed in the lexicon
TensorAgent$set("public","ensureMeaning", function(meaning) {
  if (!(meaning %in% names(self$Meanings))) {
    self$Meanings <- c(self$Meanings, length(self$Meanings)+1);
    names(self$Meanings)[[length(self$Meanings)]] <- meaning;
  }
});

#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
# TensorAgent$ensureLanguage
# - Ensure that the given language is indexed in the lexicon
TensorAgent$set("public","ensureLanguage", function(language) {
  if (!(language %in% names(self$Languages))) {
    self$Languages <- c(self$Languages, length(self$Languages)+1);
    names(self$Languages)[[length(self$Languages)]] <- language;
    self$NumberOfLanguages <- length(self$Languages);
    self$makeMeLanguagePairs();
  }
});

#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
# TensorAgent$ensureForm
# - Ensure that the given form is indexed in the lexicon
TensorAgent$set("public","ensureForm", function(form) {
  if (!(form %in% names(self$Forms))) {
    self$Forms <- c(self$Forms, length(self$Forms)+1);
    names(self$Forms)[[length(self$Forms)]] <- form;
  }
});

#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
# TensorAgent$ensureExample
# - Ensure that the given example is indexed in the lexicon
TensorAgent$set("public","encodeExample", function(meaning,language,form) {
  example <- paste("ex",
                   self$Meanings[[meaning]],":",
                   self$Languages[[language]],":",
                   self$Forms[[form]],
                   sep = "");
  return( example );
});

#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
# TensorAgent$ensureExample
# - Ensure that the given example is indexed in the lexicon
TensorAgent$set("public","ensureExample", function(meaning,language,form) {
  self$ensureMeaning(meaning);
  self$ensureLanguage(language);
  self$ensureForm(form);
  example <- self$encodeExample(meaning,language,form);
  if (!(example %in% names(self$ExampleCt))) {
    self$ExampleCt <- c(self$ExampleCt,0.0);
    names(self$ExampleCt)[[length(self$ExampleCt)]] <- example;
  }
  return( example );
});

#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
# TensorAgent$addExample
# - Add an example to the lexicon
TensorAgent$set("public","addExample", function(meaning,language,form,ct=1) {
  example <- self$ensureExample(meaning,language,form);
  self$ExampleCt[[example]] <- self$ExampleCt[[example]] + ct;
  self$version <- self$version + 1;
});

#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
# TensorAgent$normalise
# - Normalise a tensor over some set of indices
TensorAgent$set("public","normalise", function(t, overIndexes) {
  margin <- t;
  for (i in overIndexes) {
    margin <- margin.tensor(margin, i=i);
  }
  mdim <- dim(margin);
  mv <- to.matrix.tensor(margin,c(),names(margin))[1,];
  mv[mv == 0.0] <- 1.0;
  margin <- to.tensor(mv,dims=mdim);
  # print("Before Normalisation");
  # print(ftable(t));
  normalised <-  add.tensor(t,margin, op="/");
  # print("After Normalisation");
  # print(ftable(normalised));
  return( normalised )
});

#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
# TensorAgent$constructDataTensor
# - Rebuild the meaning-language-form distribution tensor
TensorAgent$set("public","constructDataTensor", function() {
  # Construct tensor
  t_list <- c();
  for (s in self$Meanings) {
    for (l in self$Languages) {
      for (f in self$Forms) {
        ct <- 0.0;
        example <- self$encodeExample(s,l,f);
        if (example %in% names(self$ExampleCt)) {
          ct <- self$ExampleCt[[example]];
        }
        t_list <- c(t_list, ct);
      }
    }
  }
  # print("t_list:");
  # print( t_list );
  R <- to.tensor( # lxf tensor for the first meaning
    t_list, c(f=length(self$Forms),
              l=length(self$Languages),
              s=length(self$Meanings)
              )
  );
  self$LexicalTensor <- self$normalise(R, c("f"));
  self$version <- self$version + 1;
  return( self$LexicalTensor );
});


#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
# TensorAgent$makeG
#
TensorAgent$set("public","makeMeLanguagePairs", function() {
  eLangList <- rep(1.0,self$NumberOfLanguages);
  self$meLanguages <- to.tensor(eLangList,c(l=self$NumberOfLanguages));
  t <- to.tensor(eLangList,c(t=self$NumberOfLanguages));
  self$meLanguagePairs <- mul.tensor(self$meLanguages,c(),t,c());
});

#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
# TensorAgent$make_p_l_t__b
#
TensorAgent$set("public","make_p_l_t__b", function() {
  if (self$version == self$p_l_t__b_version) return();
  d <- delta.tensor(c(l=self$NumberOfLanguages));
  names(d) <- c("l","t");
  self$p_l_t__b <- self$LanguageMode * self$normalise(self$meLanguagePairs,c("l"));
  self$p_l_t__b <- self$p_l_t__b + (1.0 - self$LanguageMode) * self$normalise(d,c("l"));
  self$delta_lt <- d;
  self$p_l_t__b_version <- self$version;
});

#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
# TensorAgent$make_p_f_sl
#
TensorAgent$set("public","make_p_f_sl", function() {
  if (self$version == self$p_f_sl_version) return();
  self$p_f_sl <- self$normalise(self$LexicalTensor, c("f"));
  self$p_f_sl_version <- self$version;
});

#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
# TensorAgent$make_p_f_st__b
#
TensorAgent$set("public","make_p_f_st__b", function() {
  if (self$version == self$p_f_st__b_version) return();
  self$make_p_l_t__b();
  self$make_p_f_sl();
  p_f_st__b <- einstein.tensor(self$p_f_sl,self$p_l_t__b);
  self$p_f_st__b <- self$normalise(p_f_st__b,c("f"));
  self$p_f_st__b_version <- self$version;
});

#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
# TensorAgent$make_p_l_fst__bm
#
TensorAgent$set("public","make_p_l_fst__bm", function() {
  if (self$version == self$p_l_fst__bm_version) return();
  self$make_p_l_t__b();
  self$make_p_f_sl();
  t1 <- one.tensor(c(t=self$NumberOfLanguages));
  # p_l_fst__b <- self$p_f_sl * self$p_l_t__b;
  p_l_fst__b <- self$p_f_sl * t1;
  labelset <- c("f"=length(self$Forms),
                "s"=length(self$Meanings),
                "l"=self$NumberOfLanguages,
                "t"=self$NumberOfLanguages)
  N <- prod(labelset);
  flat <- to.tensor(rep(1.0/self$NumberOfLanguages,N),labelset);
  self$p_l_fst__bm <-
    self$MonitoringLevel * self$normalise(p_l_fst__b,c("l")) +
    (1.0 - self$MonitoringLevel) * flat;
  self$p_l_fst__bm_version <- self$version;
});

#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
# TensorAgent$make_p_f_st__bm
#
TensorAgent$set("public","make_p_f_st__bm", function() {
  if (self$version == self$p_f_st__bm_version) return();
  self$make_p_l_fst__bm();
  self$make_p_f_st__b();
  p_t_fst__bm <- self$pFilter(self$p_l_fst__bm,"l","t");
  # print("p_l_fst__bm");
  # print(ftable(self$p_l_fst__bm));
  # print("p_t_fst__bm");
  # print(ftable(p_t_fst__bm));
  # print("p_f_st__b");
  # print(ftable(self$p_f_st__b));
  p_f_st__bm <- self$p_f_st__b * p_t_fst__bm;
  # print("p_f_st__bm");
  # print(ftable(p_f_st__bm));
  self$p_f_st__bm <- self$normalise(p_f_st__bm,c("f"));
  self$p_f_st__bm_version <- self$version;
});

#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
# TensorAgent$as.data.frame
# Create a data frame with information
TensorAgent$set("public","as.data.frame", function() {
  if (self$version != self$p_f_st__bm_version)
    self$make_p_f_st__bm();
  ns <- names(dim(self$p_f_st__bm));
  df <- as.data.frame(pos.tensor(dim(self$p_f_st__bm)));
  df$p <- as.vector(to.matrix.tensor(self$p_f_st__bm,ns,c()));
  df$Meaning <- names(self$Meanings)[df$s];
  df$Language <- names(self$Languages)[df$t];
  df$Form <- names(self$Forms)[df$f];
  return( df );
});

