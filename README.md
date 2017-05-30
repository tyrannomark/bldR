## Synopsis

This project 

XAt the top of the file there should be a short introduction and/ or overview that explains **what** the project is. This description should match descriptions added for package managers (Gemspec, package.json, etc.)

## Code Example

### Simulating an individual agent

```
require( bldR );
ta <- TensorAgent$new()
ta$clearLexicon();
ta$addExample("WINTER","E", "winter",   ct=100);
ta$addExample("WINTER","SG","geamradh", ct= 73);
ta$addExample("WINTER","SG","gamh",     ct= 27);
ta$constructDataTensor();
ta$setLanguageMode(0.5);
ta$setMonitoringLevel(0.7);
ta$make_p_f_st__bm();

```

### Simulating a population

```
require( bldR );
l <- L2017$new();
l$draw_all();
```

### Drawing the graphs from the paper

The following code draws all the original graphs in the paper (mentioned below).

```
require( bldR );
l <- L2017$new();
l$draw_all();
```

## Motivation

This library was created to bring together all the code used for the paper Ellison, TM & L Miceli (forthcoming) **Language Monitoring in Bilinguals as a Mechanism for Rapid Lexical Divergence.** *Language*. 
The library includes:
- code that simulates all of the models described in the paper, allowing arbitrary distributions of lexical inputs across multiple languages,
- code that simulates a population of agents, each implementing a given model,
- results from the experiment described in the paper, in a fairly raw form, and
- code analysing of experimental results.

## Installation

This software is an R library. Install it in the usual manner for R libraries from github:

```
library(devtools);
install_github("tyrannomark/bldR");
```

## API Reference

Depending on the size of the project, if it is small and simple enough the reference docs can be added to the README. For medium size to larger projects it is important to at least provide a link to where the API reference docs live.

## Tests

Describe and show how to run the tests with code examples.

## Contributors

At the moment, we are not actively seeking contributors, but if you have contributions, get in touch.

GNU Public License version 3
