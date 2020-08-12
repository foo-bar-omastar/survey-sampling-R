# Survey Sampling using R

In this project, we are going to understand various aspects of survey sampling in R. We are using R as it allows for easy random sampling and quick calculation.

The problem statement is taken from Chapter 7 (Survey Sampling) Question no. 67 of "Mathematical Statistics and Data Analysis" by John A. Rice (University of California, Berkeley). The question conatins a lot of repetitive parts, but we are going to look at only some parts of the questions, hoping to cover maximum topics in survey sampling through this question. 

### Data Description 
The data set families contains information about 43,886 families living in the city of Cyberville. The city has four regions: the Northern region has 10,149 families, the Eastern region has 10,390 families, the Southern region has 13,457 families, and theWestern region has 9,890. For each family, the following information is recorded:
1. Family type -->
  1: Husband-wife family
  2: Male-head family
  3: Female-head family
2. Number of persons in family
3. Number of children in family
4. Family income
5. Region-->
  1: North
  2: East
  3: South
  4: West
6. Education level of head of household-->
  31: Less than 1st grade
  32: 1st, 2nd, 3rd, or 4th grade
  33: 5th or 6th grade
  34: 7th or 8th grade
  35: 9th grade
  36: 10th grade
  37: 11th grade
  38: 12th grade, no diploma
  39: High school graduate, high school diploma, or equivalent
  40: Some college but no degree
  41: Associate degree in college (occupation/vocation program)
  42: Associate degree in college (academic program)
  43: Bachelor’s degree (e.g., B.S., B.A., A.B.)
  44: Master’s degree (e.g., M.S., M.A., M.B.A.)
  45: Professional school degree (e.g., M.D., D.D.S., D.V.M., LL.B., J.D.)
  46: Doctoral degree (e.g., Ph.D., Ed.D.)

We will learn about the families of Cyberville by using sampling.
