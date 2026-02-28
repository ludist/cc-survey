# Preregistration for Creative Commons Recognition Poll & License Treatment Survey Experiment
*Thomas H. Rousse*
*March 14, 2024*
*Submitted prior to data collection*

## Accompanying Materials

* Empirical Analysis of the American Public's Perception and Evaluation of Creative Commons
	- Draft longform for publication of survey stimuli and questions with explanations for design decisions, acknowledged weaknesses, exclusion criteria for participants, etc.
* RC CC Connect 2
	- Exported Qualtrics survey used to collect data from participants

## Data Collection Targets

*N*: 1200, evenly divided across 3 control groups
Median Duration: < 20 minutes
Pay: $3, hourly rate $9
Manual Review Rejection Rate: < 95%
Connect Researcher Rating: > 4.9

**"Nationally Representative" Quota, CloudResearch Connect:**
*I decided to follow Connect's preset and not to tweak the basic quota numbers to more closely approximate Census numbers so that this panel with a standard quota could be more easily compared to other panels using the same quota.*

* Gender, F 50.0% (600)
* Age
	* 18-29: 16.7% (200.4)
	* 30-44: 25.0% (300)
	* 45-59: 33.3% (400.6)
	* 60-99: 25.0% (300)
* Ethnicity, Hispanic 8.3% (99.6)
* Race
	* 83.3% (999.6) White
	* 8.3% (99.6) Black of African American
	* 8.3% (99.6)American Indian or Alaska Native, Chinese, Filipino, Hawaiian, Korean Vietnamese, Japanese, Asian Indian, Samoan, Guamanian, An ethnicity not listed here

## Minimum Significance Level

*α* = 0.05

Adapting an approach from the rules of evidence, I will report all values as significant above p = 0.05 as a measure of minimal relevance and allow the reader to assign weight for persuasiveness to the statisical evidence with greater significance.

## Randomization Check

The three subgroups should have no statistical differences due to randomization. To test the randomization and internal validity, I will conduct:
* a one-way analysis of variance (ANOVA) on the mean of age
* Pearson's chi-squared test of categorical homogeneity on race and gender
* Kruskal-Wallis test of difference in value for education, household income, political leaning, and internet skill


## Creative Commons Logo Recognition

Users are asked to identify each of four logos in random order, including a novel logo that has never been associated with a good or service, and the Creative Commons logo. Users who respond "Yes" are prompted to name the brand in a text entry box. 

All *N* participants have been presented with the same information.

**Research Question 1.2**
Is the Creative Commons logo more recognizable than a novel logo created for this experiment?

H0: CC σ^2 = Novel σ^2
There is no statistical difference between the recognition rate for the novel logo and the Creative Commons logo.

H1: CC σ^2 > Novel σ^2 
The Creative Commons logo is significantly more likely to be recognized than the novel logo. 

Using Pearson's chi-squared test, I will test whether the difference between the means of the two groups is significant, and if so, reject the null hypothesis of no statistically meaniningful difference. 

If the difference is significant, I will report the effect size  using Funder and Ozer (2019)'s intrepretation guide for correlation coefficients.

r < 0.05 - Tiny

0.05 <= r < 0.1 - Very small

0.1 <= r < 0.2 - Small

0.2 <= r < 0.3 - Medium

0.3 <= r < 0.4 - Large

r >= 0.4 - Very large

## Vignette Survey Experiment

### Groups

Respondents are randomly assigned to one of three groups by Qualtrics:
* Control condition (Group C)- For most scenarios, a "maximal" copyright notice, EXCEPT: 
	* Public Domain (V1) vignette marked "US Government Work"
	* Wikipedia Image Search Result (V6)- all conditions are Creative Commons-licensed. Control presents only link to Wikipedia 
* Treatment A - Creative Commons, Succinct, EXCEPT:
	* Public Domain (V1)
* Treatment B - CC Education Manipulation, Creative Commons, Verbose EXCEPT:
	* Public Domain (V1)

#### Group Relationships Hypothesis

When ANOVA or K-W shows significance, I will conduct pairwise significance testing. Generally speaking, I expect:

C > A > B

### Vignettes

* A. Government-created Work/Public Domain, Non-infringing Across Conditions
* B. Personal Use, Infringing Across Conditions
* C. Educational Use, Non-Infringing (Treatments) v. Infringing (Control)
* D. Commercial Advertising, Infringing Across Conditions
* E. Large Scale, Willful Copying, Infringing Across Conditions (No Attribution, Non-Commercial)
* F. Wikipedia Image Search Result, Infringing Across Conditions (Control Not Copyright)
* G. Filesharing, Non-Infringing (Treatments) v. Infringing (Control)

### Vignette Comparison Frames

**Between Subjects Analysis by Group:**

*Independent variable:* licensing condition

* Copyright Control Pools:
	* I.   CC v. Copyright: B, C, D, E, G
	* II.  Infringing Across Conditions: B, D, E
	* III. Infringing Control, Non-infringing Treatments: C, G

* Non-Copyright Control Pools:
	* Non-Infringing Across Conditions: A
	* Infringing Across Conditions: F

**Within-Subjects Analysis, All Groups**:

*Independent variable:* legal consequence for re-use as is v. respondent's view of what it should be

* Infringement Vignettes: All except A


### Questions

For each vignette, respondents answer the following questions:

* SQ 2.1

On a scale of 1–7, with 7 being the most likely:
In your opinion, how likely is [the scenario re-user] to face legal consequences for this use?

7-pt Likert ordinal scale, 1- Extremely unlikely to 7 - Extremely likely

* SQ 2.2

On a scale from 1–7, with 7 being the most negative:
In your opinion, if [the re-user] was sued for these actions, what legal consequences do you think they **would** face?

7-point Likert scale with 1 labeled “No consequences,” 4 labeled “Some negative consequences, such a court order to stop and/or a moderate fine,” and 7 labeled “Very negative consequences, such as incarceration and/or a large fine.” Items 2, 3, 5, and 6 are not labeled. 

* SQ 2.3

On a scale from 1–7, with 7 being the most negative:
In your opinion, what legal consequences **should** the homeowner face for these actions, if sued?

7-point Likert ordinal scale with 1 labeled “No consequences,” 4 labeled “Some negative consequences, such a court order to stop and/or a moderate fine,” and 7 labeled “Very negative consequences, such as incarceration and/or a large fine.” Items 2, 3, 5, and 6 are not labeled. 

* SQ 2.4

Now imagine that before the use(s) described in the scenario, the [scenario re-user] and the owner of this work agreed on a price for a non-exclusive license, meaning that the owner can also license the work to others, that allows all of the use(s) described in the scenario.

In your opinion, what do you think would be a reasonable price for that license, if any?

Continuous variable (USD)

### Plan of Analysis

For each survey question, I will report results for research questions correpsonding to the survey questions (RQs 2.1, 2.2, 2.3A, 2.4) each vignette and each of the Pools according to the following general methods by data type. 

I will also create a personal-public legal consequences congruence score for each participant by subtracting the score for 2.3 from the score for 2.2, creating another ordinal value. I will report results by vignette and for each pool. In addition, I will report a global congruence score with a Pool containing all vignettes except A.

#### General Method of Statistical Analysis of 7-Point Scale Responses (Ordinal Value)

Because several of the vignettes are intended to invoke a minimal or maximal response, resulting data is likely to be heavily skewed, thereby violating the assumption of normality. While this weakness is mitigated by the central limit theorem, a more pressing issue is that it is unclear whether the units of analysis between the ordinal variables derived from the 7-point Likert scale are equal. The ambiguity of the difference between the items in the "negative consequences" scale amplifies this issue. 

As a result, I will employ the Kruskal-Wallis to determine if at least one group is significantly different from the others.

I will report on the K-W effect size (partial epsilon squared) using the following intepretation guidelines (Field 2013):
ES < 0.01 - Very small

0.01 <= ES < 0.06 - Small

0.06 <= ES < 0.14 - Medium

ES >= 0.14 - Large

If the K-W test is significant, I will conduct a pairwise Dunn's test with a Holm-Bonferroni correction to evaluate which groups significantly differ from each other. I will report the significant findings and the effect size using Vargha & Delaney's A  as implemented by S. Mangiafico in RCompanion as the VDA function for interpretation: 

*Vargha and Delaney’s A*
[A value of 0.50 indicates that the two groups are stochastically equal.  A value of 1 indicates that the first group shows complete stochastic domination over the other group, and a value of 0 indicates the complete stochastic domination by the second group. VDA interpretation guidelines from Vargha & Delaney (2000)]

* Small:
		A Dominant:   0.56 – < 0.64
		B Dominant: > 0.34 –   0.44

* Medium:
		A Dominant:   0.64 – < 0.71
		B Dominant: > 0.29 –   0.34

* Large:
		A Dominant: ≥ 0.71
		B Dominant: ≤ 0.29

#### Statistical Analysis of Pricing Responses (Continuous Variable)

Because price is a continuous variable, whether there is a significant difference in means can be evaluated by an analysis of variance (ANOVA). Due to the sample size, any potential issues with normality should be mitigated by the provisions of the central limit theorem. I will report the effect size using eta squared.
[If the difference in means is significant, I will use pairwise t-tests to determine which groups’ means differ significantly with a Holm-Bonferroni significance adjustment for multiple comparisons. For significant values, I will report the effect size with Cohen’s d and Cohen's rule of thumb for effect size interpretation.]

Interpretation Guidelines (Funder & Ozer 2019):
r < 0.05 - Tiny

0.05 <= r < 0.1 - Very small

0.1 <= r < 0.2 - Small

0.2 <= r < 0.3 - Medium

0.3 <= r < 0.4 - Large

r >= 0.4 - Very large
