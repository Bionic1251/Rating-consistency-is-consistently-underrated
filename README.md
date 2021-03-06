# Rating consistency is consistently underrated: An exploratory analysis of movie-tag rating inconsistency

## Introduction
In the corresponding paper [Kotkov et al., 2022], we investigated the inconsistency of item-tag ratings and factors that could affect consistency in the movie domain. We ran a survey on Amazon Mechanical Turk, where we asked annotators to indicate the degree to which various tags apply to movies along with the experience of these annotators with tags and movies. For more details, please consult the corresponding paper.

In this repository, we share the collected dataset along with the programming code we used to preprocess, analyze the dataset and generate charts. The repository does not contain all the code we used for the research, but it contains the key points of our analysis.

## Dataset
The dataset consists of the following four files.

### categories.csv
The file contains tags and their categories and has the following fields:
* tag – tag string
* label – tag category. Labels correspond to the following categories: 0 - objective, 1 - misc, 2 - subjective, 3 – genre

### annotator_movie.csv
The file contains annotator answers to the survey question regarding movies. The file has the following fields:
* UID – annotator id
* movie – movie title
* how_long_ago – annotator answer to the survey question “How long ago did you watch [movie]?”

### annotator_movie_tag.csv
The file contains annotator answers to two survey questions related to movie-tag pairs. The file has the following fields:
* UID – annotator id
* movie – movie title
* tag – tag string
* rating – annotator answer to the question “On a scale from 1 to 5, how strongly does the tag [tag] apply to [movie]?”
* easy – annotator answer to the question “To what degree do you agree with the statement ‘it was easy for me to rate the tag [tag] for the movie [movie]’?”

### annotator_tag.csv
The file contains annotator answers to two survey questions along with three associations annotators provided for tags. The file has the following fields:
* UID – annotator id
* tag – tag string
* familiar – annotator answer to the question “On a scale from 1 to 5, how familiar are you with the term [tag]?”
* how_often – annotator answer to the question “On a scale from 1 to 5, how often do you watch movies that could be described as [tag]?”
* first – the first association with the tag
* second – the second association with the tag
* third – the third association with the tag

## Code
The code consists of the two following parts.

* draw_and_transform.ipynb - Jupyter notebook, which contains code to generate most charts used in the paper and to transform data for the consequent regression analysis in R
* regression_analysis.R – regression analysis

## Other folders and files
* transformed.csv – dataset transformed by the Jupyter notebook
* charts – charts generated by the Jupyter notebook



## Usage license

This work is licensed under the Creative Commons Attribution-NonCommercial 3.0 License. If you are using these code and/or datasets, please cite the following paper:

- [[Kotkov et al., 2022] Denis Kotkov, Alan Medlar, Umesh Raj Satyal, Alexandr Maslov, Mats
Neovius, and Dorota Glowacka. 2022. Rating consistency is consistently
underrated: An exploratory analysis of movie-tag rating inconsistency. In Proceedings of ACM SAC Conference (SAC’22). ACM, New York, NY, USA, Article 4, 10 pages](https://doi.org/xxx)

## Acknowledgements
We would also like to thank organizations that supported publication of this dataset: the Academy of Finland, grant #309495 (the LibDat project) and the Academy of Finland Flagship programme: Finnish Center for Artificial Intelligence FCAI.