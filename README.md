# R-Inquisit-EnsembleCoding
R and Inquisit scripts for ensemble coding of trustworthiness in faces:

This repository contains data, stimuli, & code from experiments (Inquisit scripts) and analyses (R scripts) on the ensemble coding of trustworthiness in faces.  Six studies were conducted online with Inquisit Web, using participants from Amazon Mechanical Turk.  Details for these studies are as follows:

- __Study 1 (ensemble1; n = 50 subjects):__  Subjects did a task with 100 trials, where they estimate the mean trustworthiness of a group of 4 faces (grid faces) vs. a single face (compare face). We used computer-generated faces from [Alex Todorov's database of 25 maximally distinct faces on the trustworthiness dimension](http://tlab.princeton.edu/databases/25-maximally-distinct-identities-trustworthiness/). On each trial, a fixation would appear (500ms), then the 4 grid faces (2000ms), followed by the compare face (until response).  Subjects were asked "Is THIS face more (M) or less (L) trustworthy than the group?" (no time limit to respond).  The experiment program (.iqx) constrains each trial such that faces from the same identity are never shown in the same trial, and the average of trust ratings for the grid faces cannot equal the trust rating of the compare face (if so, the program flags the trial and runs another one). Subjects were given the following instructions:

_"You are going to be doing a timed perception task on pictures of different faces.  On each trial, you will first see a grid of 4 faces.  Next, you will see a single face, and we will ask you to judge whether that one face is MORE or LESS TRUSTWORTHY than the group of 4 faces that appeared before it. If you think the single face looks MORE trustworthy than the group, press the M button on your keyboard.  If you think the single face looks LESS trustworthy than the group, press the L button on your keyboard. Note that the grid of 4 faces will only appear for a couple seconds, so make sure to pay close attention at all times."_

- __Study 2 (ensemble2; n = 50 subjects):__  Same as Study 1, except that subjects were tasked with comparing the VARIANCE of trustworthiness between two grids of 4 faces each (rather than the mean trustworthiness for 4 faces vs. a single compare face). The experiment program (.iqx) constrains each trial such that faces from the same identity are never shown in the same trial, and the variance of trust ratings for the first grid of faces could not equal the variance of trust ratings for the second grid of faces (if so, the program flags the trial and runs another one).

- __Study 3 (ensemble3; n = 50 subjects):__  Same as Study 1, except we used images of White male faces from the [Chicago Face Database](http://faculty.chicagobooth.edu/bernd.wittenbrink/cfd/index.html).  We calculated a trust score for each face by z-scoring the normed trustworthiness ratings provided for the stimuli in the Chicago Face Database codebook (see "stimuli" folder).  All other aspects of the experiment were the same as Study 1.

- __Study 4 (ensemble4; n = 411 subjects):__  This was a quick norming study to get z-scored trustworthiness ratings for other faces aside from White males in the [Chicago Face Database](http://faculty.chicagobooth.edu/bernd.wittenbrink/cfd/index.html).  We did this by having subjects give trust ratings for all Black/White male/female faces in the Chicago Face Database, using a clickable 9-point scale ("How trustworthy do you find this person?").  All subjects rated 190 faces each, after being randomly assigned to 1 of 4 conditions (the R script for this study uses ratings from condition 1 to create trust z-scores, which are then outputted to a new .csv file):

1. ___Mixed race and gender___ - all faces randomized by race/gender in one block
2. ___Separate race and gender___ - all faces blocked by race & gender (i.e., 4 blocks of White male, White female, Black male, & Black female; block order randomized)
3. ___Mixed race and separate gender___ - faces blocked by gender, with races mixed (i.e., 2 blocks of all Black/White female faces and all Black/White male faces; block order randomized)
4. ___Mixed gender and separate race___ - faces blocked by race, with genders mixed (i.e., 2 blocks of all male/female Black faces and all male/female White faces; block order randomized)

- __Study 5 (ensemble5; n = XXXX subjects):__ start




