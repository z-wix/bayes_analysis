Project
================
Zack Wixom

![Super Smash
Logo](/Project/Figures/Smash/super-smash-bros-ultimate.png)

## Project Organization

`01_smash_data.Rmd` goes through my data collection process which
includes webscrapping tables, cleaning data, and combining datasets to
make a single source to model on as well as formulating the data story.
Also includes some failed attempts at gathering data before moving to
different source.

`02_smash_modeling.Rmd` The beginning of my modeling process, mostly
creating DAG’s and simple linear models.

`03_smash_mcmc.Rmd` Moving onto MCMC modeling and more complex models.

`04_smash_iterations.Rmd` More Iterations on Project with different
models.

`01_fortnite_data.Rmd` First attempt at project to use an API to extract
data on fortnite players. However, my data source ended up not having
enough data to do a proper analysis.

`01_twitch_analysis.Rmd` using data from Twitch dataset from Kaggle.
Data import and cleaning.

`02_twitch_analysis.Rmd` start of modeling. DAG and simple linear
models.

## Project Workflow

1.  Data Story

<!-- end list -->

  - Begin with a conceptual story: Where do these data come from? What
    does the theory say?

  - Translate into probability statements (i.e., the likelihood or
    observational model).

  - The resulting model is generative.

<!-- end list -->

2.  Bayesian Updating

<!-- end list -->

  - Bayesian updating is learning\! (See illustration.)

  - Every posterior is the prior for the next observation.

  - Natural consequence of probability. The sample size is automatically
    embodied in the posterior (i.e., no “degrees of freedom”).

<!-- end list -->

3.  Evaluate

<!-- end list -->

  - Inference is conditioned on the chosen model.

  - Use prior predictive checks and posterior predictive checks to
    evaluate the model.

  - Modeling is iterative. You’ll likely need to revise your story.

### Pushed to GitHub

  - `/Code` Each script should do something specific (like tidyverse
    functions), have a descriptive name, and include number prefixes if
    they are meant to be run in a certain order (e.g.,
    `01_import_data.R`, `02_clean_data.R`).
  - `/Data` While all data live here, only data that are small and can
    be shared publicly will be pushed.
  - `/Figures` Figures live here, including images (saved as PNG files)
    and data referenced or used for tables, for use in the `README` and
    report.
  - `/Report` The report, without any PDF knits.
  - `README` This page, with any other organization details to make it
    easy to navigate your repository.

### Not Pushed to GitHub

  - `/Output` Output from model runs. These files tend to be too large.
    They are also something each user can create on their own.
