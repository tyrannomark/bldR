#' bldR
#'
#' \code{bldR} is a package for modelling bilingual-led divergence of lexica.
#'
#' bldR provides three R6 classes:
#' \code{\link{TensorAgent}},
#' \code{\link{PopulationSimulation}}, and
#' \code{\link{L2017}}.
#'
#' Each are described on their own pages.
#'
#' @docType package
#' @name bldR
#' @seealso Most uptodate version at http://bld.markellison.net/
NULL

#' EM2017_Stimuli
#'
#' This data set contains the stimuli used in the experiment described in
#' Ellison & Miceli (2017 - Language 93(2):255-287).
#'
#'
#' @name EM2017_Stimuli
#' @docType data
#' @author T. Mark Ellison \email{m.ellison@anu.edu.au}
#' @format A data frame with 41 rows and 7 columns. The columns are as follows:
#' \describe{
#' \item{StimulusId}{An identifying number associated with this particular stimulus.}
#' \item{DutchDoppel}{The Dutch half of the doppel which is a potential response to the stimulus.}
#' \item{EnglishDoppel}{The English half of the doppel which is a potential response to the stimulus. This is a potential response to the stimulus.}
#' \item{EnglishNonDoppelExample}{An example word which could also be a response to the stimulus, but which has no doppel in Dutch.}
#' \item{DutchContext}{This is the paragraph immediately preceding the sentence frame, tightening the semantic context and (when used with EnglishFrame) pushing the participants into bilingual mode.}
#' \item{EnglishContext}{This is the paragraph immediately preceding the sentence frame used with monolingual English speakers.}
#' \item{EnglishFrame}{This is the sentence frame (with a non-initial gap) used to elicit a response from the participants.}
#' }
#' @references \url{http://bld.markellison.net/}
#' @keywords data, experiment
NULL

#' EM2017_Responses
#'
#' This data set contains the responses returned in the experiment described in Ellison & Miceli (2017 - Language 93(2):255-287).
#'
#'
#' @name EM2017_Responses
#' @docType data
#' @author T. Mark Ellison \email{m.ellison@anu.edu.au}
#' @format A data frame with 2049 rows and 5 columns. The columns are as follows:
#' \describe{
#' \item{Condition}{Either "Monolingual" or "Bilingual" indicating the linguistic background of the participant.}
#' \item{ParticipantId}{The identifying code for the participant.}
#' \item{StimulusId}{The identifying code for the stimulus.}
#' \item{Response}{The response given by this participant to this stimulus.}
#' \item{IsDoppel}{A numeric value with 1 if the response is a doppel, and 0 if not.}
#' }
#' @references \url{http://bld.markellison.net/}
#' @keywords data, experiment
NULL

#' EM2017_ParameterFit
#'
#' This data set contains the fit of the cognitive model described in Ellison & Miceli (2017 - Language 93(2):255-287) - hereafter EM -
#' to the data returned by the experiment also described in the paper.
#'
#'
#' @name EM2017_ParameterFit
#' @docType data
#' @author T. Mark Ellison \email{m.ellison@anu.edu.au}
#' @format A data frame with 35968726 rows and 9 columns. Of these, 3 columns are for internal use and can be ignored. The others fields are the following:
#' \describe{
#'   \item{Form}{The form returned used (one doppel per stimulus, and abstract values indicating non-doppels used in English and Dutch).}
#'   \item{Meaning}{Names for th 41 distinct meanings tested in the experiment. These take the form of "S"+<stimulus-number+"_"+(allcaps)<meaning>.}
#'   \item{Language}{The language the simulated participant is trying to express their form in: either English or Dutch. In the experiment reported in EM,
#' articipants were only aiming to produce forms in English, so the values for \code{Language="Dutch"} are not used in parameter-fitting.}
#'   \item{lm}{Language mode varying from 0.00 to 1.00 in increments of 0.01.}
#'   \item{ml}{Monitoring level varying from 0.00 to 1.00 in increments of 0.01.}
#' \item{p}{The conditional probability of the form given the parameter values - basically, how well did the model do at predicting the frequency with which this form would be used by bilinguals.}
#' }
#' @references \url{http://bld.markellison.net/}
#' @keywords data, experiment
NULL
