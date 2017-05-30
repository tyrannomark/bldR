# bldR - modelling Bilingual-Led Divergence

This project contains code in the form of an R package for modelling bilingual-led divergence. It implements a cognitive model of bilingual lexical selection, and then builds community-level agent-based modelling on top of this implementation.

The software takes the form of an R package, called **bldR**.

This software is companion to the following publication, hereafter called *EM*:

* Ellison, TM and L Miceli (2017) **Language Discontinua: how Bilingual-Led Divergence Disrupts Cladistic Modelling**. [Language](https://muse.jhu.edu/journal/112) 93(2):255-287.

The package contains all new data reported in *EM*, and all software used in the simulations, data-processing, statistical testing and graph-drawing.

The code consists of three parts:

* the cognitive simulation, implemented in the R6 class *TensorAgent*,
* the population simulation, implemented in the R6 class *PopulationSimulation*, and
* the specific simulations run for *EM*, including the ggplot2 code to create its graphs, as implemented in the R6 class *L2017*.

Also included in the package are three data-sets:

* *EM2017_Stimuli* contains the stimuli used in the experiment reported in *EM*,
* *EM2017_Responses* contains the raw results of the experiment,
* *EM2017_ParameterFit* contains the probability the model assigns to the experimental data for 10201 parameter settings.

## Installing bldR

You can easily install this package from github directly. Here are the steps (see [this page](http://kbroman.org/pkg_primer/pages/github.html) for more details):

1. Install the [devtools](https://github.com/hadley/devtools) package. For example, start up R and run:

```R
install.packages("devtools")
```

2. Now load the devtools library:

```R
library(devtools)
```

3. Then install this R package:

```R
install_github("tyrannomark/bldR")
```

## Code Example

Here are some examples of code using the classes defined in the package. Other examples are given in the R package documentation.

### Simulating an individual agent

Here is a very simple model of a bilingual lexical production in a context where English (EN) can use either *picture* or *photo* (spelled *foto* to indicate to the software the identity with its Dutch doppel) but prefers *photo*, and where in the corresponding context, Dutch (DU) would only use *foto*.

We assume that speakers are in bilingual mode - i.e. somewhat prone to intrusions from their non-intended language if they don't monitor - at a level of 0.5. To minimise these intrusions, however, they engage in monitoring at a reasonably strenuous level, namely 0.7.


```
require( bldR );
ta <- TensorAgent$new()
ta$clearLexicon();
ta$addExample("PHOTO","EN","picture",   ct=40);
ta$addExample("PHOTO","EN","foto",      ct=60);
ta$addExample("PHOTO","DU","foto",      ct=100);
ta$constructDataTensor();
ta$setLanguageMode(0.5);
ta$setMonitoringLevel(0.7);
ta$make_p_f_st__bm();
print(ta$as.data.frame());
```

Note that the production probability of *foto* is reduced for bilinguals when speaking English, and probability of using *picture* is increased.

### Simulating a population

In this example, let's imagine a microcommunity with 20 monolingual English speakers, 30 monolingual Dutch speakers, and 40 speakers who are bilingual. The bilinguals have the same level of bilingual mode and monitoring as in the above example. Let's look at what happens to the use of alternative forms in the community over time. Since only English has initial variation, this is the only case in which we will see change.

Each agent in each generation produces a sample of 50 forms, based on their inputs and biases. These samples as a whole are used as the input (divided by language of course) for the next generation.

Bilinguals are assumed to be taking input from, and producing output in, each of their languages equally.

```
require( bldR );
p <- PopulationSimulation$new();
p$setPopulationStructure(A=20,B=30,AB=40);
p$setLanguageMode( 0.5 );
p$setMonitoringLevel( 0.7 );
p$setSamplesPerAgent( 100 );
p$setNumberOfGenerations( 30 );
p$setLexicon(A_d=0.4,A_nA=0.6,A_nB=0.0,B_d=1.0,B_nA=0.0,B_nB=0.0)
history <- p$simulate(exact = FALSE);
historyAd <- history[history$Language == "A" & history$Form == "d",]
doppelUse <- aggregate(p ~ g,data=historyAd,FUN=mean)
plot(doppelUse)
```

The resulting graph is not very pretty, but you can see the fall-off among English speakers (in this simulated microcommunity) in probability of their use of the doppel form.

### Drawing the graphs from the paper

The following code draws graph **4a** from *EM*. The functions for analysing data and constructing all the other graphs from the paper can also be called from the class ```L2017```.

```
require( bldR );
l <- L2017$new();
l$draw_4a();
```

## Future Plans

A snapshot of this package will be kept at the [repository](http://muse.jhu.edu/article/XXX/) refered to in *EM*. While the code used in the article will not change (that is the three files *TensorAgent.R*, *PopulationSimulation.R* and *L2017.R*) unless bugs are found, the package codebase as a whole will change.

Later versions will include:
* simulations of language differentiation where information about the relative frequency of forms is not available.
* enriched cognitive models incorporating other biases that can change production frequency away from input frequency.
* data, analysis and presentation code from papers detailing future experiments.

## Contributors

The code and documentation presented here were all written by [T. Mark Ellison](http://markellison.net/). Conceptual development as reported in the paper *EM* was jointly undertaken with Luisa Miceli.

GNU Public License version 3
