# Load libraries
library(shiny) # shiny app functionality
library(shinythemes) # https://rstudio.github.io/shinythemes/
library(shinyMatrix) #https://cran.r-project.org/web/packages/shinyMatrix/readme/README.html
library(rhandsontable)
library(thematic)
library(tidyverse)
library(lavaan)
library(simsem)
library(QuantPsyc)
library(ggplot2)
library(semPlot)
library(patchwork) # stitches ggplots https://patchwork.data-imaginist.com/articles/guides/layout.html
library(grid)
library(ggiraph)
library(see) # assumption plots
library(webr) #https://cran.r-project.org/web/packages/webr/vignettes/plot-htest.html
library(performance) # assumption plots
library(ggrepel) # outlier plots
library(equatiomatic) # provides formulas of models
library(gridExtra) # gridarrange for plots
library(kableExtra)# for table formatting
library(parameters)# lavan output, e.g., cfa/lavaan that is tidy
library(ggiraphExtra)
library(plyr)
library(MASS)
library(NHANES)
library(glue)#print variables within string
library(Superpower)#anova simulate file:///Users/michaeldemidenko/Downloads/0.1_Simulation_Based_Power_Analysis_For_Factorial_ANOVA_Designs.pdf
library(multcomp) # anova tukey
library(ppcor)

# UI -- display that adjusts the dimensions of browser
# Shiny app application guides advanced layouts: https://shiny.rstudio.com/articles/layout-guide.html https://mastering-shiny.org/action-layout.html
# navbar example: https://shiny.rstudio.com/gallery/navbar-example.html
#   https://shanghai.hosting.nyu.edu/data/r/case-1-3-shinyapp.html

# widgets https://shiny.rstudio.com/tutorial/written-tutorial/lesson3/

# simulating data notes: https://aosmith.rbind.io/2018/08/29/getting-started-simulating-data/#simulate-data-with-a-difference-among-groups

# If you did not go through the tutorials at the links above, the shiny app can be broken down into three main components: 
#       (1) User Interface
#       (2) Local server input/output generation
#       (3) Running shiny app
# Here the navbarPage() is deployed to create tabulated panels. Within each tabPanel() is contained a fluid page with the sidebarLayout. 
#   The sidebarLayout consists of the sidebarPanel() which invokes user response and the mainPanel() which prints out the content.
#   sidebarPanel() input is used in the local server output generation, whereby input$<input_label> populates the value.
#   Similar to the input, mainPanel() pulls output$<output_label> to print the contents.
# In the local server input/output generation, datasets, plots and statistical values are generated:
#       (1) datasets are generated using: eventReactive(input$run1, {}) - eventReactive allows the data to be re-run on button push by user.
#       (2) plots are generated using: renderPlot({}).
#       (3) statistics are used generated using: renderPrint({})


set.seed(1000)
options(scipen=999) # turning off scientific notiation so extremely small values can be rounded


## Matrices used 
m <- matrix(data = c(100, 5, 2.5, "-", "-", "-",
                     100, "-", "-", 25, "-", "-",
                     100, "-", "-", 25, "-", "-",
                     100, "-", "-", "-", 0, 50), ncol = 6, byrow = TRUE, 
            dimnames = list(
              c("Normal Dist", "T Dist", "Chi-square Dist", "Random Uniform Dist"),
              c("N", "Mean", "St Dev", "DF", "Min", "Max")
            ))

r_ex <- matrix(data = c(5, .2, 
                        4, .2), ncol = 2, byrow = F, 
               dimnames = list(
                 c("Mean", "Correlation var 1 ~ Var 2 (r)"),
                 c("X1", "X2")
                 ))

## /Matrices used 



UserInferface <- navbarPage("Ooo, So Shiny!",
                            collapsible = TRUE, 
                            inverse = FALSE,
                            fluid = TRUE,
                            windowTitle = "Simulations for Research Stats",
                            theme = shinytheme("flatly"),
                            
                            tabPanel("Intro",
                                     fluidPage(
                                         sidebarLayout(position = "right",
                                             sidebarPanel(style = "display: inline-block; float: center",
                                                 actionButton("start", "Onward!",
                                                              style =  "color: #FFF; background-color: #8B0000; border-color: #FFFF00;")
                                             ),
                                             mainPanel(
                                                 h2("Another day another shiny stat"),
                                                 p("This shiny app is a work-in-progress. It was last updated November 2nd, 2021",
                                                   style = "font-family: 'times'; font-size:14px"),
                                                 p("Its purpose? To play with data. There are several tabs, select each tab to simulate some data based one the input provided, such as:
                                                 means, standard deviations, Pearson's r and/or sample size.",
                                                   style = "font-family: 'times'; font-size:14px"),
                                                 img(src="demo_intro.gif", align = "right", height = '150px', width = '300px'),
                                                 p("Most items for the app run after you click the 'Onward!' button. This button keeps the lights running. You can use the defaults, or change them and click the button again.
                                                 Whether it's to visualize the effect of the crud factor (see Meehl, 1990), variance/standard deviation (SD), sample (N) at which a correlation stabilizes 
                                                 (see Schönbrodt & Perugini (2013)), or the effect of an outlier, click a tab, play around with some parameters and see how 
                                                 some outputs change. Keep in mind, the data is generated using a normal distribution. Often our data -- in particular in the 
                                                 social sciences -- has values that are densely packed around the the mean (leptokurtic, or positive kurtosis), flatly distributed 
                                                 (platykurtic, or negative kurtosis), heavily weighted distribution left to mean values (positive skew), heavily weighted 
                                                 distribution to the right of the mean (negative skew), or zero-inflated values. Most simulations DO NOT include 
                                                   the effects of this on the resulting statistical models. In future updates, I hope to include tabs on this topic",
                                                   style = "font-family: 'times'; font-size:14px"),
                                                 p("If you read the above paragraph and thought, 'Hey, what do this mumbo-jumbo 'positive skew' mean?' then I recommend starting with the",
                                                   span("Preliminary", style = "font-weight: bold"),"tab. This will define some terminology and provide
                                                   some examples so it is easier to follow along when this terminology is used on other tabs.",
                                                   style = "font-family: 'times'; font-size:14px"),
                                                 br(),
                                                 p("OF NOTE: The data will be simulated each time a sample size value is changed. If you're wondering, why is there so much white space between
                                                 text? This will change once you click the button to run the appropriate simulation. You'll notice that the mean/sd will not be *exactly* the specified mean.
                                                   The sampling is being done from a random normal distribution, so after sampling the specified span the values in the simulated dataset will bounce 
                                                   around those numbers a tiny bit. Furthermore, each time the sample N is changed,
                                                   the data set is resimulated. Thus, they won't be exact, but approximate is the goal. It's for fun, after all...",
                                                   style = "font-family: 'times'; font-size:14px"),
                                                 p("Please complete ", 
                                                   a("this brief survey.", href="https://umich.qualtrics.com/jfe/form/SV_80zhUf4cB7wJXWC", target="_blank"),
                                                   "to tailor and refine future versions of this app.",
                                                   style = "font-family: 'times'; font-size:12px"),
                                                 p("For Citation & Documentation purposes, code will will be available on",
                                                   a("Github", href="https://github.com/", target="_blank"), "once an initial full version is complete.",
                                                   style = "font-family: 'times'; font-weight: bold; font-size:10px"),
                                                 br(),
                                                 br(),
                                                 br(),
                                                 p("© 2021 Michael Demidenko. Some rights reserved. Please contact for more details",
                                                   style = "font-family: 'times'; font-size:10px")
                                             )
                                         )
                                     )),
                            
                            
# Preliminary
                            tabPanel("Preliminary",
                                     fluidPage(
                                         mainPanel(
                                             h2("Primer/Glossary"),
                                             p("Given that everyone has a different background knowledge when it comes to research methods, I thought
                                               it would be helpful to define and provide examples for some terms so we are all
                                               on the same page for the subsequent tabs. For example, I may use terms such as 
                                               'independent variables', 'normal distribution' or 'estimates'. Some of you may not be familiar with
                                               these terms, so some definitions are in order that way we're speaking the same language",
                                               style = "font-family: 'times'; font-size:14px"),
                                             h4("Hypothesis Testing"),
                                             p("When we have a research question, we use hypothesis testing to determine whether there is support for or against our hypothesis. Before we start our analyses,
                                               we want to state what the Null and Alternative Hypothesis is. Typically an α (i.e. alpha) is used to set the cut-off for what we define as the cut-off for significant
                                               or non-significant  (i.e., our p-value threshold).",
                                               style = "font-family: 'times'; font-size:12px"),
                                             p(span("Null Hypothesis (H0):", style = "font-weight: bold"), "In statistical tests, the null hypothesis (H0) is that there is no mean difference 
                                             or association between variables. If we set our α = .05, run a Two-sample T-test and the resulting p-value is .16, we would accept the null hypothesis,
                                               that there is no difference.",
                                               style = "font-family: 'times'; font-size:12px"),
                                             p(span("Alternative Hypothesis (H1):", style = "font-weight: bold"), "In statistical tests, the alternative hypothesis (H1) is that there is a mean difference or 
                                               or association between variables. If our α = .05, and our Two-sample T-test provides a p-value of .002, we could reject the null hypothesis, which is that there is a
                                               difference in the means or our association.",
                                               style = "font-family: 'times'; font-size:12px"),
                                             p(span("p-value:", style = "font-weight: bold"), "As noted in Kruschke & Liddel (2018),
                                                 the p-value is", span("the probability that the observed effect/t-value would be observed if: a) H0/null were true and b) data were 
                                                 sampled according to the same stopping/testing methids as this data.", style = "font-style: italic"), "So in other words, the probability
                                               is the probability of observing the an equal or more extreme difference or association in the data based on that datas measures and sampling characteristics. Remember,
                                               the t-distribution, just like the f-distribution, represent the characteristics and spread based on our sample size, mean and SD. The p-value is a fairly abstract
                                               cut-off, as is discussed in Cohen (1994; DOI: 10.1037/0003-066X.49.12.997) & Amrhein et al (2017; DOI: 10.7717/peerj.3544). The p-value can be influenced by man things, as I will show in later tabs,
                                               such as the sample size, outliers and incorrect measurement. Thus, Benjamin et al (2018; DOI: 10.1038/s41562-017-0189-z) proposed a p < .005 cut-off. Lakens et al (2018; DOI: 10.1038/s41562-018-0311-x) 
                                               then argued in response that p-values should be justified. More recently, Rubin (2021; DOI: 10.1007/s11229-021-03276-4) made arguments for considering when and how
                                               to adjust p-values (or alpha levels) when conducting analyses. These are complicated issues, but what it boils down to is that the p > .05 and p < .05 is a fairly abstract cut-off.
                                               The abstractness is even more interesting when you consider that Rondal Aylmer Fisher who introduced the p-valued arrived at it when considering the probability that a lady could 
                                               tell whether the milk was poured first or last for a cup of tea. This account of the p-value is a fun topic question, but in statistical terms it's worth to consider how
                                               the cut-off determines whether something is or isn't meaningful. Take for instace that the differene in estimates that has a p-value of p = .025 and p = .0005 is subtantially larger than 
                                               the difference in estimates between p = 073 and p = .043. However, the conclusion, based on the p-value, is both are significant (i.e., p <  .05) in the former case but only one is significant
                                               in the latter case, despite the relative difference between values in the latter case being relatively small. One should be cautious when using p-values -- it's important to remember what they mean
                                               with respect to your research question and consider the magnitude of the affect to determine 'meaning'.",
                                               style = "font-family: 'times'; font-size:12px"),
                                             p("There are several caveats with hypothesis testing. In addition to the noted issues with the p-value, we can run into several errors along the way. For instance, a Type-I error (or False Positive) is 
                                               that our measure, sample or sampling distribution estimated that there is a significant  (p < .05) mean difference or association amongst our variables when in fact
                                               there is no difference. Essentially, our test misfired and we got a significance signal when one doesn't really exist. Then, a Type-II error (or False Negative) is when our measurement, sample or 
                                               sampling distribution estimated that there is no significant (p > .05) mean difference or association amongst our variables in fact there is a difference. Finally, there is a Type-III error 
                                               (or assumption of equivalence) which is assumption that our test is equivalent to another test that is conducted. Type-III errors are sometimes referred to
                                               finding a significant effect in the wrong direction, but here I focus on the term as it relates to test equivalence. Researchers often perform tests that are similar,
                                               but differ on type of sampling, measures and other study related decisions. This may result in a different conclusion which may be based on the fact that the tests were
                                               not equivalent between the two studies. This will be important in our discussion on constructs and construct validation.",
                                               style = "font-family: 'times'; font-size:12px"),
                                             h4("Variables"),
                                             p(span("Independent Variable (IV):", style = "font-weight: bold"), "is the cause or our predictor of interest. 
                                             The independent variable, or IV for short, is the variable that we suspect is contributing to an outcome in our research question.
                                             For example, say we would like to test how hunger levels impact driving speeds on the interstate. In this scenario, the IV would be
                                             hunger level of the participants.",
                                               style = "font-family: 'times'; font-size:12px"),
                                             p(span("Dependent Variable (DV):", style = "font-weight: bold"), "is the outcome or the result of interest. 
                                             If the IV is the predictor/cause, the dependent variable, or DV for short, is the outcome we are testing for in our research question. 
                                             For example, if we are testing how hunger levels impact driving speeds on the interstate, the driving speed would be the outcome. So we would measure to what extent the IV 'causes' the DV. 
                                             I write cause in quotes because causal relationships are at times difficult to demonstrate and often we only can refer to them as 
                                               associations.",
                                               style = "font-family: 'times'; font-size:12px"),
                                             p(span("Covariate:", style = "font-weight: bold"), "in research studies there are other IVs which are cooccur with our variable of interest,
                                             which is hunger in the above example, but they are not of direct interest to our research question. These are covariates, or 'confounding variables'. 
                                             Covariates can serve as IVs, but because we have a specific question about the association between hunger and driving speed, 
                                             we need to ensure these other variables are not the reason for our results. For example, hunger isn't the only thing that may cause 
                                             increased driving speeds on the interstate. There can be other variables, such as fatigue, distraction, emotion, urgency, etc. Think of all of the reasons that have caused you to speed, these can impact the conclusion we may make about the association 
                                             between hunger and driving speeding. In which case, we would need to control, or adjust for these covariates, by including them in our statistical models or by using
                                             randomization techniques in order to alleviate differences related to these confounding variables.",
                                               style = "font-family: 'times'; font-size:12px"),
                                             p(span("How to minize confounds:", style = "font-weight: bold"), "if we know our research question may contain confoundings variables, 
                                               it is worthwhile to consider how we may adjust for them in our research design. One approach that is often used is",
                                               span("randomization.", style = "color: black; font-style: italics"), "By using experimental designs, studies attempt
                                               to reduce the influence of confounds by recruiting a select number of participants from a population and
                                               randomly assigning them to either a control or intervention arm of the study. By using a robust randomization approach, 
                                               we may control for both known and unkown confounds. However, intervention approaches are not appropriate for all types of studies.
                                               If we're interested in the influence of lead in water on cognitive development, it would be inappropriate to randomly assign 
                                               people from the population to these groups. An alternative approach may be ", span("matching or restricting", style = "color: black; font-style: italics"), 
                                               "the type of sample that is included. This approach may help you to account for known differences in your sample/population(s).
                                               While matching (such as via propensity scores) helps alleviate some unknowns, it is not a silver bullet. What is commonly done in psychological research is",
                                               span("controlling", style = "color: black; font-style: italics"), "for the influence of a variable in a statistical model. The goal is to include the
                                             covariate in the model so its influence is partially adjusted in the association between the IV and DV of interest",
                                               style = "font-family: 'times'; font-size:12px"),
                                             p(span("Operationalization:", style = "font-weight: bold"), "is the process of defining our variables in a study.
                                               For instance, when we are measuring hunger and speeding, we can ask how hungry someone is in different ways but speeding can be more
                                               mechanical. For hunger, we can use a scale from 1 (not hungry at all) to 100 (as hungry as I have been), ask on a scale whether they are are more or less
                                               hungry than normal, or more explicitly asking 'are you hungry (yes/no)?'. Then, for speed, we can use the onboard system of a car (or phone GPS) to measure their speed in certain sections
                                               or have someone guage their speed using a radar gun at distinct points along the interstate The speeding is a bit more direct than hunger, as you can tell.
                                               But operationalization for some variables becomes more difficults. Consider our covariates, or confounding variables. If we wanted to account for an
                                               emotional state, how would be go about it? This latter variable is more abstract and so requires thinking through the types of emotions and ways
                                               we can ask about them to give us the best overall value for that variable. This often involve several methodological and theoretical steps
                                               that are part of validating a construct (more on this in a later tab).",
                                               style = "font-family: 'times'; font-size:12px"),
                                             br(),
                                             br(),
                                             h4("Coefficients/Estimates"),
                                             p("Each variable that we collect has provides distinct", span("estimates", style = "font-style: italic"), "in the dataset. These estimates
                                               can be described in different ways.",
                                               style = "font-family: 'times'; font-size:12px"),
                                             p(span("Mean:", style = "font-weight: bold"), "this is the average of our observations. In statistics, this is
                                               considered as one of the 'moments', or mathematical values that describe the distribution of our data. In Figure 1 (A-C), 
                                               panel A depicts a dataset that contains 100 observations for 'var1'. If we take
                                               all of these 100 values, add them up and divide it by the sample size (n = 100), we would get the mean, or average. In our this example,
                                               that mean would be 5.23. The red line in panel B depicts the mean",
                                               style = "font-family: 'times'; font-size:12px"),
                                             img(src="Deviations_3panel.jpg", align = "center", height = '375px', width = '350px'),
                                             p(span("Squared Deviation:", style = "font-weight: bold"), "the deviations are the difference between the data that we collected and the mean
                                               for that data. Going back to our Figure 1, panel C provides the deviations an example of how the deviations are calculated for each observation.
                                                To get the deviation for each participant, we subtract the mean from each participant's observed value. We could just add all of these values up, 
                                                however, the positive values would cancel out some of the negative values, which can result in zero deviations. Instead, we square 
                                                each participants value when we subtract the observed from the mean. That way all of the values are positive. Now, when we add the deviations up for our 100 observations,
                                               we will get the 'squared deviation', or the total squared differences between our *observed data points* and the *mean for Var1*. This
                                               is referred to as the 'sum of squared deviatons'.",
                                               style = "font-family: 'times'; font-size:12px"),
                                             p(span("Variance:", style = "font-weight: bold"), "this is the variability, or dispersion, in observations around the mean. Like the mean, this is
                                               considered as another moment that describes the distribution of the data. The variance is built on the square deviations. After we have calculated
                                               the squared deviation for all 100 observations in our example above and added those values together, we divide the squared deviation by the sample size minus 1, 
                                               or N - 1 (in our case 100-1). This would give us the the variance for our sample.",
                                               style = "font-family: 'times'; font-size:12px"),
                                             img(src="Data_100pt_mean_stdev.jpg", align = "right", height = '275px', width = '200px'),
                                             p(span("Standard Deviation:", style = "font-weight: bold"), "similar to the variance, the standard deviation, or SD for short, is the despersion of the
                                             data around the mean. It is standardized by taking the square root of the variance for the variable in our sample. By taking the square root,
                                             we SDis now in units that are similar to the mean. When the SD is larger, that means the observed values for the variable spread out further
                                             away from the mean, and when SD values are small, the observed values for the variable are closer to the mean. In the Figure 2, for var1, I plot the
                                             +/- 1 SD (blue lines) surrounding the mean (red line).",
                                               style = "font-family: 'times'; font-size:12px"),
                                             p("You may be asking yourself,", span("Wait.... the squared deviations, variance and standard deviation sounds very much alike. What gives?", 
                                                                                  style = "font-style: italic"),
                                               "You are correct -- all three represent a similar concept: the fit of the mean to the data, the variability in the data, how well the
                                               mean represents the data and the 'error' in our data, commonly referred to as residuals in statistical models.",
                                               style = "font-family: 'times'; font-size:12px"),
                                             p(span("Covariance:", style = "font-weight: bold"), "this is the joint variability between two variables. In simple terms, this is a
                                               measure of how much change in one variable is associated with a second variable. Take for example the ",
                                               a("'Our World in Data'.", 
                                                 href="https://ourworldindata.org/grapher/co2-emissions-vs-gdp?yScale=log"), "In this figure they are reporting the association
                                               between CO2 emissions per capita (the average per person in that country) and GDP per capita. The covariation is, as the GDP per capita increase
                                               there is an observed increase in annual C02 emissions by that country. So the nature of this fluctuation is related. If these
                                               valuse were standardized, we could get the magnitude of the association, or effect size, via a Pearson's correlation (r) value. This
                                               would give us the strength of the association, or fluctuation, among these two variables.",
                                               style = "font-family: 'times'; font-size:12px"),
                                             p(span("Distribution:", style = "font-weight: bold"), "this is the spread in observations/data that we can expect.
                                               The most common distribution is the Normal distribution (in the Figure 3 labeled as 'Random Normal') which forms a symmetric bell-shapred curve. Distribution follows a mathematic 
                                               function from which we can calculate the probability for observing a value out of all of the values in our data/sample. There are other distributions, too.
                                               For instance, in Figure 3 the data is generated based on input values in the table. For the normal distribution, the distribution is based on
                                               the sample mean (m) and standard deviation (SD). Then, the random uniform distribution is based on the distribution between 
                                               the minimum (min) and maximum (max) values.  Unlike the normal distribution, the random uniform distribution follows the 
                                               probability formula where each observation is equally likely. In other words, drawing a value of 0 and 50 is equally likely in a sample of 100. 
                                               Then, the t-distribution is what is often used in calculating statistical significance (i.e. p-value). As you can see in the figure below, 
                                               the t-distribution and normal distribution are awfully similar, both are symmetric and have a bell-shapred curve. However, the 
                                               t-distribution has wider tails and doesn't assume the population standard deviation. Instead, the t-distribution is defined by degrees of freedom
                                               (which are the logically independent values which are free to vary in a sample). Finally, the chi-square distribution is similar to normal 
                                               and t-distribution. Unlike the normal and t-distribution, the values in chi-squared distribution are squared. 
                                               Hence the name. So all negative values are zero, and always greater than 0. For more insights on distributions and the sample degrees of freedom (DF)
                                               I refer you to the Qunatitude podcast on ", 
                                               a("'Statistics Degrees of Freedom'.", 
                                                 href="https://podcasts.apple.com/us/podcast/s3e08-statistical-degrees-of-freedom-an-intimate-stranger/id1484406501?i=1000539718170", target="_blank"),
                                               "Dr. Curran and Dr. Hancock explain these issues in greater detail. Nonetheless, update the values in the table below and see how the values change.
                                               Notice, different distribution require different population parameters, e.g. sample size, mean, SD, min value, max value and/or DF.",
                                               style = "font-family: 'times'; font-size:12px"), 
                                             matrixInput(inputId = "Distribution_mat", 
                                                         value = m, 
                                                         rows = list(extend = FALSE, names = TRUE), cols = list(extend = FALSE, names = TRUE)),
                                             br(),
                                             plotOutput(outputId = "dist_plot"),
                                             p(span("Skew:", style = "font-weight: bold"), "this describes the symmetry of the data. Just like the mean and variance,
                                               the skew is another moment of the data. Figure 4 provides examples of what skewed data would be relative to the normal
                                               distribution of the data. Whereas the positive skew reflects an increased frequency of smaller values, negative skew reflects an increase
                                               frequency of larger values, or larger than the mean.",
                                               style = "font-family: 'times'; font-size:12px"),
                                             img(src="Fig_SkewKurtosis.jpg", align = "right", height = '350px', width = '300px'),
                                             p(span("Kurtosis:", style = "font-weight: bold"), "this describes the curvature and tails of the data. Like the mean, variance and skew,
                                               kurtosis is a moment that describes the distribution of the data. Data can have positive and negative kurtosis. Positive kurtosis, known as a 
                                               leptokurtic distribution, has a tighter peak with less distribution in the tails. Negative kurtosis, known as platkurtic distribution, is a flatter
                                               distribution relative to the normal distribution with fatter tails. One way to remember the difference is 'platty fatty' for platykurtic",
                                               style = "font-family: 'times'; font-size:12px"),
                                             br(),
                                             br(),
                                             h4("Values from Statistical Models"),
                                             p("When statistical models are reported, specific language is often used to refer to the question, the test, the data collected and 
                                               the value that the statistical model computes",
                                               style = "font-family: 'times'; font-size:12px"),
                                             p(span("Correlation:", style = "font-weight: bold"), "as referred to above, correlation is the association between variables that is 
                                               standardized. The value is contrained to -1 to 1. This is an interpretable metric of the association between two variables in a study. 
                                               For example, in Figure 5 below, simulate 150 observations for the Means and Correlations coefficients in the table above it. 
                                               The bottom panel reflects the correlation among variables X1 and X2. You can modify the correlation coefficient (r) for the variables between -1 and
                                               1 to observe the change in the plot. (Note, is r= .3 in column X1, it should also be .3 in X2, as they reflect the same association)",
                                               style = "font-family: 'times'; font-size:12px"),
                                             p(span("Effect Size:", style = "font-weight: bold"), "this represent how small or large the association (or difference) is. When we 
                                               compare the standardized covariation between two variables via a Pearson's r correlation coefficient, we can report whether the effect is large, medium,
                                               or small. There are certain standards, but these are debated as we will note later. On the other hand, when we're comparing group means, 
                                               we can calculate the magnitude of this effect (or difference) by using a Cohen's D calculation. 
                                               Unlike the Pearson's r which ranges from -1 to 1, the Cohen's D can exceed one. This is because the association between two variables 
                                               cannot exceed a perfect linear relationship, which is r = 1.0, however, the difference in means between two variables can get larger and larger, theoretically. 
                                               If you modify the means and correlation (r) values in the table below, you can observe how the correlation can shift even when
                                               means stay the same, and vice versa. Reflecting their unique characteristic.",
                                               style = "font-family: 'times'; font-size:12px"),
                                             matrixInput(inputId = "cov_examp", 
                                                         value = r_ex, 
                                                         rows = list(extend = FALSE, names = TRUE), cols = list(extend = FALSE, names = TRUE)),
                                             plotOutput(outputId = "cov_mean_ex"),
                                             p(span("Estimated v Observed:", style = "font-weight: bold"), "statistical models attempt to estimate the association amongst the observed data.
                                               For instance, when we calculate a correlation between two variables, like we did in Figure 5, we get one coefficient (or numerical value) 
                                               that represents association between those variables. This value represents the line that is meant to 'best' approximate the data. 
                                               As you see in the bottom panel of Figure 5, the line is fit through our data, but it doesn't perfectly match with the observed data (i.e.,
                                               real data or those we simulate and know values for). Thus, the line estimates the values for our observed data. Often times they are not a perfect match",
                                               style = "font-family: 'times'; font-size:12px"),
                                             p(span("Residuals:", style = "font-weight: bold"), "the difference between what ur model estimates and the data we observed as the residual values. 
                                               For example, in Figure 5 we fit the linear line to our data. Some values it perfectly overlaps with and others no so much. Now,
                                               if we were to take the values that the value estimates for that data point and subtract it from the data point we observed, we would get a residual 
                                               for that pair. If we do this for all of the values estimate by the line and those we observed, we can get the total residuals for the model.
                                               The 'best fit' line are what some statistical methods utilize to minimize the residuals. More on this in the regression tab.",
                                               style = "font-family: 'times'; font-size:12px"),
                                             p(span("Confidence Interval:", style = "font-weight: bold"), "this is the range for which an observed estimate in a model, like a difference between two means,
                                               would vary within an upper and lower bound. Typically, the 95% confidence interval (CI) is used. For example, if we had a sampled a 100 participants, got our means
                                               and SD, if we were to resample based on the sample characteristics of the mean and SD we would get an estimate with a lower and upper bound 95% CI. This CI is related to the p-value,
                                               as the distributions are based on comparable sample characteristics. When the 95% CI does not cross zero, then the value is significant (i.e., p < .05), but if the 95% CI
                                               does cross zero, then the value is not significant (i.e. p > .05).",
                                               style = "font-family: 'times'; font-size:12px")
                                         )
                                     )),
################################ 
                            tabPanel("T-tests", 
                                     fluidPage(
                                         sidebarLayout(position = "right",
                                             sidebarPanel(
                                                 helpText("Select Parameters to Simulate Data"),
                                                 actionButton("run1", "Onward!", 
                                                              style =  "color: #FFF; background-color: #8B0000; border-color: #FFFF00"),
                                                 sliderInput("sample", label = "Sample Size",
                                                             min = 5, value = 125, max = 2000, step = 15),
                                                 sliderInput("pop_mean", label = "Population Mean (e.g., H0)",
                                                             min = 0, value = 0, max = 30, step = .5),
                                                 sliderInput("alpha", label = "Alpha α (Type I error rate)",
                                                             min = 0.0001, value = .05, max = .20, step = .025),
                                                 sliderInput("mean1", label = "Var  1 Mean ",
                                                             min = 0, value = 9, max = 30, step = .5),
                                                 sliderInput("sd1",  label = "Var 1 St Dev",
                                                             min = 0.1, value = 2.25, max = 25, step = .5),
                                                 sliderInput("mean2", label = "Var 2 Mean",
                                                             min = 0, value = 20, max = 30, step = .5 ),
                                                 sliderInput("sd2", label = "Var 2 St Dev",
                                                             min = 0.1, value = 4, max = 25, step = .5),
                                                 sliderInput("grp_m1", label = "Group A Mean",
                                                             min = 0, value = 22, max = 30, step = .5),
                                                 sliderInput("grp_sd1",  label = "Group A St Dev",
                                                             min = 0.1, value = 6.2, max = 25, step = .5),
                                                 sliderInput("grp_m2", label = "Group B Mean",
                                                             min = 0, value = 3.5, max = 30, step = .5 ),
                                                 sliderInput("grp_sd2", label = "Group B SD",
                                                             min = 0.1, value = 5, max = 25, step = .5)
                                                 
                                             ),
                                             mainPanel(
                                                 h2("Description: Distribution & Effect for Mean-SD-Sample"),
                                                 p("What is the purpose of this tab? Based on the means, standard deviations (SD), and sample size (N) that you selected 
                                                   (or their defaults), a normally distributed dataset is generated. This data is then used to plot a boxplot of the two 
                                                   variables to visualize means/SD to see how data points are distributed relative to the mean. Then, the distribution of 
                                                   a single variable and the population mean is plotted. After which, we compute a One-Sample t-test on the mean(s) and SD(s). 
                                                   Thereafter, using our simulated data we consider the unique group means for the combined means of two variables. 
                                                   This data is plotted, and a Two-Sample independent t-test is conducted. For both the One-Sample and Two-Sample t-test 
                                                   their respective formulas are provided and the effect of sample size is considered. In general, this tab is to represent 
                                                   changes in the values based on: Mean, Standard Deviation, Sample Size, Alpha and Population Mean (which is often 
                                                   hypothesized to be nil). If you're from the industry and non-academic side, in one way, this is equivalent to A/B testing. 
                                                   With an important distinction, the t-test is the way to compare the effects of scheme you follow to collect your data.",
                                                   style = "font-family: 'times'; font-size:14px"),
                                                 p("Note: To run/rerun, simply specify your values and click 'Onward!'. White space will remain until data can be 
                                                   simulated by pushing the 'Onward!' button and the respective Figures/Tables are populated",
                                                   style = "font-family: 'times'; font-size:10px; font-style: italic"),
                                                 br(),
                                                 br(),
                                                 h4("Simulated output",
                                                    style = "font-family: 'times'"),
                                                 p("First, we started by plotting two boxplots for the simulated Var1 and Var2 in Figures 1 and Figure 2. The boxplots 
                                                   below represent the means and SDs for Var 1 and Var 2. You can see the data points normally distributed around the 
                                                   mean. As you toggle the sample size, you'll notice the central tendency arise. However, the central tendency is more 
                                                   obvious below in Figure 3.",
                                                   style = "font-family: 'times'; font-size:14px"),
                                                 br(),
                                                 fluidRow(
                                                     splitLayout(cellWidths = c("50%", "50%"),
                                                                 plotOutput(outputId = "hist_var1"),
                                                                 plotOutput(outputId = "hist_var2")
                                                     )),
                                                 br(),
                                                 h4("Plotted Distribution: Variable 1",
                                                   style = "font-family: 'times'"),
                                                 p("Here, Figure 3 reflects the simulated distribution of Var 1 (based on specified mean) and 
                                                   +/- 1, 2 and 3 standard deviations relative to this mean. For reference, the specified population mean 
                                                   (that is used in the One-Sample t-test below) is also plotted in the figure. Since the data is resampled 
                                                   for this normal distribution (i.e., density plot) the mean will vary a touch. One thing to notice from 
                                                   the normal distribution is where most of the data falls. For example, approx. 68% of the observations 
                                                   are between -/+1 SDs of the sample mean, or the dashed blue line, and approx. 95% of observations are 
                                                   within -/+ 2 SDs of the sample mean, or the green line. Then, approx. 5% of the observations are beyond 
                                                   the -/+2 SD line. These add up to the cumulative distribution function for the normal distribution and 
                                                   may help in calculating outliers, which is the occurrence of extreme and unexpected values.",
                                                   style = "font-family: 'times'; font-size:14px"),
                                                 plotOutput(outputId = "t_test_dist"),
                                                 h4("One Sample t-test: Formula & Effect of Parameters",
                                                   style = "font-family: 'times'"),
                                                 p("Keeping in mind how the data shifts around the mean, we can now consider the One-Sample t-test formula. If you ever came 
                                                   across the language, “So-and-so sample demonstrated a response (Mean = 2.9, SD = 2.0) that was significantly greater than the null 
                                                   t(49) = 49/0, p < .00001”, then you had experience a One-Sample t-test. The purpose of the One-Sample t-test is to estimate a 
                                                   t-value (t) based on the population mean (μ) and the sample mean (m) relative to proportion of variance/SD (s) scaled by 
                                                   the population size (√n)",
                                                   style = "font-family: 'times'; font-size:14px"),
                                                 uiOutput(outputId = "one_t_formula"),
                                                 p("For example, say I am a grocery store manager that is interested in creating a sign that'll implicitly motivate customers to stop 
                                                   at a particular location inside the store. We're not trying to compare the effect of one sign over the other *yet*, but rather 
                                                   we are curious whether there is any effect of the sign in this part of the store. We created a sign and machine learning 
                                                   (i.e., a research assistant with a stopwatch) that determines when a participant (customer) enters the defined vicinity using their 
                                                   stop watch calculates (approximately) how long they stopped for. After a few long weeks and a really tired research assistant and worn 
                                                   out stop watch, we will have a dataset of length N sample of participants that stopped for an average time (Var 1 mean) and these 
                                                   participants varied in their stopping time to some degree (Var 1 St Dev). Unless we have a prior estimate of how long populations 
                                                   randomly stop in this given area, we will assume that population mean stopping time is zero (implicit assumption in a lot of scientific research). 
                                                   So, we can propose the null (H0) hypothesis, that there would be no difference between how long people normal stop in this space (zero/null) 
                                                   and with our sign there. The alternative (H1) hypothesis would be the opposite, that we expect a significant difference between the population 
                                                   mean and our observed mean with the sign there. ",
                                                   style = "font-family: 'times'; font-size:14px"),
                                                 p("To obtain our estimate of significance we would use the One-Sample t-test. To simplify the formula above, we would like to obtain a 
                                                   t-statistic that compares the signal in the numerator (what we want to maximize, stopping at sign) relative to the noise in the denominator 
                                                   (what we want to limit, the variability between people). This maximization of the signal-to-noise ratio is central to experimental psychology 
                                                   (Cronbach, 1957). To maximize the signal, we can increase the sample mean in comparison to the population mean (μ). Another way to increase 
                                                   the signal is by minimizing the noise, or our SD. This can be done by reducing variability *between* subjects or measurement error. 
                                                   The error can also be minimized by a larger N, as the SD is scaled by sample size (see Figure 4 for example).",
                                                   style = "font-family: 'times'; font-size:14px"),
                                                 tableOutput(outputId = "one_t_table"),
                                                 p("The above table provides the sample size (should be equal), means and SDs you input into the fields on the upper right hand. 
                                                   Since we're working with a One-sample t-test, let's pay attention to Var1 for now. You can follow along by entering each value 
                                                   into the One-Sample t-test formula manually (since it’s relatively easy). Such that, for the formula your ‘Var1 Mean’ will replace 
                                                   'm' and ‘Var 1 SD’ will replace ‘s’, your ‘sample size’ will replace ‘n’ and the population mean you entered will replace 'μ'. 
                                                   For simplicity purposes, I have a Probability density plot for a two-tailed t-test below that includes your t-statistic (t), 
                                                   your degrees of freedom (df), which are the number of observations minus 1 (n-1) and the p-value. In the One-Sample t-test 
                                                   probability density plot below, you will see the critical cut off (alpha/p-val) of 95% (p < .05) and the blue dot represents 
                                                   the t-statistics from our variable. If you did your math correctly, your t-statistic should be the same. Unless something went wrong… 
                                                   and it’s not unknown for things to go wrong. By pulling up a t-distribution, you can find the provided significant, too. You simply 
                                                   identify the t-statistic, the degrees of freedom (n – 1) and one-tailed or two-tailed. ",
                                                   style = "font-family: 'times'; font-size:14px"),
                                                 p("What we can gather from this is the significance of our effect for *this* sample and its size and variability. This significance, again, 
                                                   is relative to our population mean. So, while it is common to say that the population mean is zero, this often is not true for many scenarios. 
                                                   For instance, we may find that people in some parts of the store tend to stop for no reason, so the population mean may be, say, 11 seconds. 
                                                   So, if we had known that the population mean was closer to 11 seconds, but assumption that a population mean of zero would be incorrect and 
                                                   thus provide the wrong conclusion. We may, in effect (pun very much intended), find significance when in fact there may be none if we were to 
                                                   use a specific (point estimate) versus a zero (point null) estimate. This topic has been extensively discussed and is a point of contention 
                                                   in several statistical tests.",
                                                   style = "font-family: 'times'; font-size:14px"),
                                                 plotOutput(outputId = "t_test_dist_t"),
                                                 p("It’s worthwhile to look at the probability densities for the t-statistics. Given that you have two variables (Var 1 & Var 2) 
                                                   that you can adjust the mean and SD for, play around with these parameters. Look at the above T-test formulate again and observe 
                                                   how the 'significance' of the effect sways based on these values. You can compare the above plot for Var 1 and the below plot for 
                                                   Var 2. While these tests can tell us something meaningful, if we’re not thoughtful when using them we may misunderstand the 
                                                   meaningfulness of an effect because we are at times consumed by finding that “p < .05” golden ticket.",
                                                   style = "font-family: 'times'; font-size:14px"),
                                                 plotOutput(outputId = "t_test_dist_t2"),
                                                 br(),
                                                 p("I mentioned earlier to consider the denominator of the One Sample t-test formula. If you messed around with different 
                                                   combinations of mean, SD and sample size (N) values, you may have noticed an important trend: the larger the sample, the 
                                                   larger t-statistic and thus the larger the p-value. This represents the issue of the ‘crud factor’, whereby the larger number 
                                                   of participants the lower the critical threshold for significant is. Thus, larger N provide significance for small effects. 
                                                   Then you reach the point of deciphering, is an effect this tiny important? Sometimes, sadly, the answer is a clear: no. 
                                                   Other times it’s a resounding… maybe?",
                                                   style = "font-family: 'times'; font-size:14px"),
                                                 p("Here the effect of the crud fact is in essence achieved by the SD (our variability) being shrunk by the N. To demonstrate this, 
                                                   we can take the mean of Var 1 and see how the N proportionally reduces the influence of the SD from the denominator on the numerator. 
                                                   In term of fractions, if we make the denominator SMALLER we can make the result of the numerator LARGER. Figure 4 shows how the 
                                                   SD shrinks in magnitude from N = 5 to 2000.",
                                                   style = "font-family: 'times'; font-size:14px"),
                                                 uiOutput(outputId = "one_t_form_denom"),
                                                 plotOutput(outputId = "t_test_reduc_sd_1"),
                                                 br(),
                                                 br(),
                                                 h4("Two Independent Group Means",
                                                   style = "font-family: 'times'"),
                                                 p("For now, we have focused on the One-Sample t-test, which considers a mean and SD for one sample/group of subjects. However, say we wanted to consider 
                                                 the difference BETWEEN groups, so two means and SDs. Going back to our earlier example from the grocery store (keep coming back to the grocery example, 
                                                 trying to remind myself to get groceries…). Let's say that we wanted to determine whether there was a meaningful difference in stopping by signs that 
                                                 were placed at two stores in different states, say Dallas, Texas and Lincoln, Nebraska. So now we have the same sign, we have the same grocer, same part 
                                                 of the store, but would like to know whether there is [in simple terms] something meaningfully different between the store in Dallas and Lincoln. If our 
                                                 groups consist of independent samples and variability that is similar (i.e., sample of people in each group are unique and SD is comparable), 
                                                 we can conduct a Two-Sample t-test.
                                                   we hade two groups. ",
                                                   style = "font-family: 'times'; font-size:14px"),
                                                 uiOutput(outputId = "two_mean_t_formula"),
                                                 p("As we saw for the One-sample t-test two means in the numerator, we see the same in the Two-Sample t-test. However, unlike the One-sample t-test 
                                                   in the Two-Sample t-test these are both samples that we have observed data for, Sample A and Sample B. So, for both samples, unlike the population, 
                                                   we have their actual means but ALSO their SD(s). Effectively, we can follow similar procedures to compare the sample means to determine the difference, 
                                                   or effect, between groups.",
                                                   style = "font-family: 'times'; font-size:14px"),
                                                 tableOutput(outputId = "two_mean_table"),
                                                 p("Above is a Table that provides information about simulated data for our groups (A & B), their sample size (N), group means and group SDs. 
                                                   The means for Var 1 and Var 2 are simulated in a manner that will provide you with specified means for Group A and Group B. ",
                                                   style = "font-family: 'times'; font-size:14px"),
                                                 plotOutput(outputId = "two_mean_dist"),
                                                 p("Figure 5 are boxplots with the mean and SD for each group (A & B). Like Figure 1 & 2, you can observe the spread of the raw data points 
                                                   that are simulated for each group. Then, Figure 6 is a visual representation how ignoring groups and looking at variables alone may 
                                                   provide a different association (black line) amongst Var1 and Var2 than if you were to consider those associations across groups 
                                                   (green and yellow). In this scenario the contrast may now be as stark, but I will expand on how exaggerated 
                                                   there differences may become in data. For example, when the groups are combined the association 
                                                   may be zero, but when separated, the association may be positive for one group and negative for the other. More on correlations in the 
                                                   ‘Correlation’ and “Stability of Effect’ tabs.",
                                                   style = "font-family: 'times'; font-size:14px"),
                                                 plotOutput(outputId = "two_mean_test1"),
                                                 p("As we did for the One-Sample t-test, we again can acquire the t-statistic from our formula and determine the p-value, or significance, of the difference 
                                                   between our groups for [this] sample and distribution. Revisiting our example, we may find the means and SD for stopping at a sign 
                                                   between stores are nearly identical. While we may have observed a significant in a One-Sample t-test, or the effect of stopping due to our sign, 
                                                   this did not change between the samples for grocery store A and grocery store B. ",
                                                   style = "font-family: 'times'; font-size:14px"),
                                                 plotOutput(outputId = "twomean_reduc_sd_1"),
                                                 p("As you keep playing with the parameters, you'll find a similar cautionary tale of 'significance' (or the golden ticket, p < .05). 
                                                   For the Two-Sample t-test, the t-statistic that we get can again be impacted by the scaling of SD in the denominator by the N. 
                                                   As we say in Figure 4 for the One-Sample t-test, in Figure 7, for the Two-Sample t-test, we observe a similar scaling by the 
                                                   sample size. ",
                                                   style = "font-family: 'times'; font-size:14px"),
                                                 br(),
                                                 br(),
                                                 h4("Meaning of p-value in NHST?", style = "font-samily: 'time'"),
                                                 p("As we were going along, one thing you may have noticed is the phrasing of ‘for this sample'. This is an important thing to keep in mind. 
                                                 Most statistics we use in the null hypothesis significance testing (NHST) are based on the parameter (mean, SD, Pearson’s r) distributions. These distributions are often specific to our 
                                                 observations/data/samples.",
                                                   style = "font-family: 'times'; font-size:14px"),
                                                 p("In this and other tabs in this shinyapp the p-value is used to represent the impact of means/errors and the 
                                                 crud factor (sample size) on the p-value in the NHST framekwork. As noted in Kruschke & Liddel (2018),
                                                 the p-value is", span("the probability that the observed effect/t-value would be observed if: a) H0/null were true and b) data were 
                                                 sampled according to the same stopping/testing methids as this data.", style = "font-style: italic"), "As mentioned earlier, the mean or 
                                                 mean difference (our signal) is in the numerator and the SD (the noise) is in the denominator. So larger values in numerator and 
                                                 smaller values in the denominator increase the t-statistic and thus 'significance'. The sample size (in the numerator) also does a lot of heavy 
                                                 lifting as it attenuates the impact of BETWEEN subject (or intersubject) variability. So when we use the significance is based on NHST,
                                                 if we meet, we are bound the the interpretation within that sample. A sampling or measurement technique from a different sample
                                                   can produce different distributions and hence results. 
                                                   So generalizability should be tested and not assumed.",
                                                   style = "font-family: 'times'; font-size:14px"),
                                                 br(),
                                                 br(),
                                                 p("Packages used - Plotting: ggplot w/ tidyverse | plotting htest t-distributions: webr via plot() by Keon-Woong Moon",
                                                   style = "font-family: 'times'; font-size:10px")
                                             )
                                         )
                                     )
                            ),
################################
################################
################################

tabPanel("ANOVA",
         fluidPage(
           sidebarLayout(position = "right",
                         sidebarPanel(
                           h4("Select Parameters to Simulate Data"),
                           sliderInput("aov_sample", label = "Sample Size",
                                       min = 5, value = 50, max = 2000,step = 15),
                           sliderInput("aov1_m", label = "Between(1): Var 1 Mean",
                                       min = -20, value = 5, max = 20, step = .5),
                           sliderInput("aov1_sd", label = "Between(1): Var 1 SD",
                                       min = .01, value = 1, max = 10, step = .1),
                           sliderInput("aov2_m", label = "Between(1): Var 2 Mean",
                                       min = -20, value = 11, max = 20, step = .5),
                           sliderInput("aov2_sd", label = "Between(1): Var 2 SD",
                                       min = .01, value = 3.8, max = 10, step = .1),
                           sliderInput("aov3_m", label = "Between(1): Var 3 Mean",
                                       min = -20, value = 6.5, max = 20, step = .5),
                           sliderInput("aov3_sd", label = "Between(1): Var 3 SD",
                                       min = .01, value = .75, max = 10, step = .1),
                           actionButton("run4", "Run One-Way ANOVA!", 
                                        style =  "color: #FFF; background-color: #8B0000; border-color: #FFFF00"),
                           sliderInput("aov4_m", label = "Between(2): Var 1 Mean",
                                       min = -20, value = 4.2, max = 20, step = .5),
                           sliderInput("aov4_sd", label = "Between(2): Var 1 SD",
                                       min = .01, value = 1.8, max = 10, step = .1 ),
                           sliderInput("aov5_m", label = "Between(2): Var 2 Mean",
                                       min = -20, value = 17, max = 20, step = .5),
                           sliderInput("aov5_sd", label = "Between(2): Var 2 SD",
                                       min = .01, value = 2.7, max = 10, step = .1),
                           sliderInput("aov6_m", label = "Between(2): Var 3 Mean",
                                       min = -20, value = 12, max = 20, step = .5),
                           sliderInput("aov6_sd", label = "Between(2): Var 3 SD",
                                       min = .01, value = 6, max = 10, step = .1),
                           actionButton("run4.1", "Run 2x3 ANOVA!"),
                         ),
                         mainPanel(
                           h2("One-way & Two-Way ANOVAs"),
                           p("What does this tab represent? In the previous tab, T-tests, I reviewed the comparison of data with a single mean
                             compare to the population (One Sample t-test) and then the comparison between two sample means (Two Sample t-test).
                             In this tab, I expand on this via the Analysis of Variance, or ANOVA. The ANOVA is an extension of mean
                             comparisons, in that we consider the difference across 2+ means. 
                             ANOVA compares the difference on a continuous outcome (DV)  across a categorical predictor (IV). Say for instance we were running ads online
                             and we wanted to compare a difference in engagement, a continuous vairable, across three or more ad types. This could be image type, text type,
                             day of week, time of day, etc. You can take any variable that you think may influence the average engagement, and consider those.
                             Here, let's say we will vary on subject of photo (i.e. Image type): Nature, City and People. This is categorical value, because it does
                             not have an inherent numerical value that is ordered or continuous. We may run several Two Sample T-test to determine how the means for continuous values 
                             (i.e., engagement) may vary across the categorical values (i.e., image types). But, this increases the likelihood of a false positive (or Type II error). To avoid this,
                             we instead considered differences across ALL of our means simulatenously. This would be our One-way (1x3) ANOVA,
                             three means across one level. Then, I extend the One-way ANOVA, to a two by three ANOVA (2x3), 
                             whereby we consider the mean difference across a continous variables across ad types (our three types above) by two group levels, such as sex (male versus female)..",
                             style = "font-family: 'times'; font-size:14px"),
                           p("Start off by hitting 'One-Way, Run!' and 'Two-by-Three, Run!' using the [preset] default values. I pair the text below with these defaults in mind.
                             Then, change the default values to re-simulate and see how the values change for a One-way and 2x3 ANOVA. Play around with it until you feel
                             you have the grasp for these two basic ANOVA concepts.",
                             style = "font-family: 'times'; font-size:14px"),
                           br(),
                           h4("ANOVA Formula"),
                           p("Let's start off by considering the formula for an ANOVA. To simplify things, we will use the One-way ANOVA
                             as an example. In the ANOVA, there are several factors that are considered. These are, the Sum of Squares total (SST), which 
                             is the deviation across all of our data compared to the overall mean. The Sum of Squares Between (SSB), which is the deviation of the
                             group means (k) relative to the overall mean multiplied by the group size (N), the Sum of Squares Within (SSW), which is deviation of scores
                             within the group means. Then the mean squared error within (MSW) and mean squared error between (MSB) are used to calculate
                             an F-statistic. We will go over these in more detail below.",
                             style = "font-family: 'times'; font-size:14px"),
                           p("The full number of deviations in the ANOVA are captured in the Sum of Squares Total (SST).
                           The formula for this is below. It is the sum of squared deviations (or differences) of each score from the grand mean (grand = overall). This
                           is the total deviations in our dataset that are then decomposed in the deviations of interested, between group differences, and deviations of little interest,
                           the within group differences. 
                           As I will show in Figure 2, this is simply taking all of your observations, across all image types, etc, and calculating your overall mean. 
                           Now that you have your overall mean, you calculate your square deviations by subtracting the mean and the observed score. This equivalent to the
                           sum of square variance that were shown in the 'Preliminary' tab. The only difference here is now we have values assoicated with our groups (k). In the 
                           formula, notation for groups is j, and the notation for individual is i. So we take each value for each individual from each group, and subtract it from this
                           overall 'grand' (X-bar), square these values, and sum them all up to get the total sum of squares [total].",
                             style = "font-family: 'times'; font-size:14px"),
                           uiOutput(outputId = "aov_SST"), 
                           p("Next, we calculate the Sum of Squares Between (SSB), or the factor effect. Using our image example above for ads, this would be the image type. 
                           Unlike the SST which subtracts each individual score from the grand mean, the SSB is the sum of squared individual group means (k) minus 
                           the grand mean. I provide an example of this in Figure 3 below. So, we calculate our overall grand mean and our overall group means (k). 
                           Then, we subtract the group mean and the grand mean, square those values and then multiply that by the repective group size N (j). More on this in the 
                             example below.",
                             style = "font-family: 'times'; font-size:14px"),
                           uiOutput(outputId = "aov_SSB"), # subtract individual grp means from grand mean, sqrd diff by grp N
                           p("Next we calculate the Sum of Squares Within (SSW), or the sum of squares error (SEE). This is the squared differences of scores from within the group/factor (k).
                            Like you would do more a normal mean to calculate the squared deviations, we do this here. This is simply the within-group variation. So, like I show in Figure 4, we calculate the
                             the mean for each group, then within each group we subtract each observation it's respective group mean and get that squared deviation within that group. Then we add these
                             values across our groups. You'll notice, this is similar to the SST formula, with the exception that here we are subtract the observe values from the group mean (k)
                             instead of the grand mean.",
                             style = "font-family: 'times'; font-size:14px"),
                           uiOutput(outputId = "aov_SSW"), # sqrd diff of scores from mean score within group (i.e., SSE)
                           p("Now that we have the SST, SSB and SST, we have the necessary information to proceed in calculating a an F-statistic. 
                           The F-statistic, similar to the T-statistic, is what we use to define the null distribution for our sample. For ANOVA, we use the F-test
                           to compare whether the mean and standard deviation of our grops is equal to the normally distribution for the population. Before we get to getting our F-statistic,
                             we need to complete two more steps",
                             style = "font-family: 'times'; font-size:14px"),
                           p("First, we need to calculate the numerator of the F-statistic, which is the Mean Squared Error Between (MSB). The SSB is the difference between our groups divide by the
                             number of groups (k-1). So as you can see in the formulate below, the size of MSB increases with a greater SSB value and lower number of groups (k).",
                             style = "font-family: 'times'; font-size:14px"),
                           uiOutput(outputId = "aov_MSB"),
                           p("Second, we calculate the denominator of the F-statistics, which is the Mean Squared Error Within (MSW). The SSW is the variability within groups divided by 
                             by the total sample (N) minus the number of groups (k). Thus, when the difference between sample size and number of groups is larger, MSW is smaller.",
                             style = "font-family: 'times'; font-size:14px"),
                           uiOutput(outputId = "aov_MSW"),
                           p("Now that we have calculated the MSB and MSW, we have the necessary ingredients to calculate the F-statistic. The F-stat quantifies the differences between 
                           population means (Null = no difference between means). As such, the null hypothesis is that the between group difference (i.e. MSD) and the within group difference 
                           (i.e. MSW) should be relatively similar. However, if the means differ, it is expected that the between group variability (i.e. MSB) is larger than the within group variability (MSW). 
                           The F-distribution then is the ratio of MSB/MSW with the degrees of freedom, k - 1 and N - k. This will hopefully become a bit more apparent
                             in explicit example below.",
                             style = "font-family: 'times'; font-size:14px;"),
                           uiOutput(outputId = "aov_F_form"),
                           br(),
                           h4("One-way ANOVA"),
                           p("Let's use the formula and information from above to provide an explicit example for a One-way ANOVA. If you're using the default values and
                             pushed 'Run One-way ANOVA!' you should see the tables and figures below. First, let's consider our sample and distribution of data first. 
                             Using the example from the start, we'll consider the engagement for different image types. Say we ran 50 ads for each group, respective. The average
                             (SD) for engagement for City, Nature and People images are reported in Table 1 below. In addition, I include the overall N, mean, and SD in Table 2. 
                             I know the engagement is unrealistically low, but imagine that this is MySpace and my ad skills are poor. Now it may seem more realistic :) ",
                             style = "font-family: 'times'; font-size:14px;"),
                           tableOutput(outputId = "aov_sim_one"),
                           tableOutput(outputId = "aov_sim_one_overal"),
                           p("Okay, so we now have our group sizes, group means and group SDs. We can visualize this data using a boxplot to see how our means and the range of observations
                             for our data. This is plotted for your reference in Figure 1. As you will see, as the Table 1 indicates, that data for People and Nature 
                             are somewhat similar, in that the distribution has some overlap. However, we see that for City images, the mean is nearly 2x higher, but the SD is 4x higher.
                             We may make a visual conclusion based on it, but we can use the One-way ANOVA to confirm the statistical conclusions about this difference.",
                             style = "font-family: 'times'; font-size:14px;"),
                           plotOutput(outputId = "aov_one_plot"),
                           p("We wont get into here, but it's worthwhile to remember that the ANOVA can be written as a formula linear formula, which we will discuss more in the 'Regression' tab",
                             style = "font-family: 'times'; font-size:14px;"),
                           uiOutput(outputId = "aov_one_formula"),
                           p("Nevertheless, let's run the one-way ANOVA model and evaluate whether the mean differences are significant. 
                             In Table 3 is the reported output from the aov() function in R. It provides us with the Terms: Image_Type (our factor/group variable) and residuals. Based on the
                             F-statistic (i.e., statistic) and the p-value, this model is signifiance (p < .00001). Instead of going over the Sum of Square and Mean Sum of Squares now, I will
                             break each of these down as we work through the figures 2-4 below. I will refer back to values from this table as we go along ",
                             style = "font-family: 'times'; font-size:14px;"),
                           tableOutput(outputId = "aov_out_one"),
                           p("When we discussed the One-way ANOVA formula, the Sum of Squares Total, or SST, was brought up. While we do not see this in the
                             aov() output above, we can get it by adding the Sum of Squares values for Image Type and Residuals values in Table 3. As described for SST, this value is calculated by squaring the 
                             difference between each observation in Figure 1 (total obs = 150) and subtracting it from the mean (red line).
                             Accounting for rounding error, the values should sum up to the SST reported in Figure 2. Doing this for all 150 observations,
                             we add the values up and get the SST. For our example, this is simply the engagements that we observed across ALL of our ad types (i.e. person, nature, city).
                             Then, we subtract the number of engagements for each ad from the average of engagements for all ads we ran. 
                             You can do this manually, but it would take some time, so it's easier to take some shortcuts using the power of technology.",
                             style = "font-family: 'times'; font-size:14px;"),
                           plotOutput(outputId = "aov_SST_plot"),
                           p("We got a visual of how SST is calculated for this simulated data, now let's consider the SSB, or the deviations between our groups. 
                             If we refer back to the formula for the SSB, the SSB is the factor (or group) effect. In Figure 3, we can see the group means for City, 
                             Nature and People image types in the boxplots (thicker center line of boxplot is the mean) for each group, and the overall mean across all of the observations (red line). 
                             To get the SSB, for *each* group we subtract the group mean (Table 1) from the overall mean (Table 2), square the values and multiply it by the group size, then add
                             the result for each group together. For our example, if we go to Table 1 with the means and N for City, Nature,  People and Table 2 for overall mean, we can manually get the SSB.
                             Note: ^2 means 'squared'. So, we would do N([City mean]-[overall mean])^2 & Nature N([Nature mean]-[overall mean])^2 & N([People mean]-[overall mean])^2. 
                             Once we have these three values for each image type, we would add them up. This would match the Sum Sq. Between value reported in Figure 3 and the value
                             in the aov() reported table above for the row 'Image_Type' and column 'Sum of Squares'. In this aov() table we also see the degrees of freedom (DF) reported, which is k-1, 
                             or three image types minus one.",
                             style = "font-family: 'times'; font-size:14px;"),
                           plotOutput(outputId = "aov_SSB_plot"),
                           p("Okay, great. So we now have calculated the SSB with the DF, which will help us in calculating the numerator for the F-statistic, or the  MSB (i.e., Mean Squared Error Between). 
                             But we still need the denominator, or the MSW (i.e., mean squared error within) for the F-statistic. This is calculated via the SSW, or sum of squares within.
                             Recall the formula from above and consider Figure 4. We have three group means (our factor), and each group has it's own mean, which is the average number of engagements
                             for our ads. This average is calculated based on our 50 observations, or independent ads that we ran. It is likely that there may be within group variability in how the ad performed
                             for the image type. This variability may not be related to our groups but perhaps other factors. 
                             So to calculate the SSW, we would calculate the squared deviation by subtraction the observation, or number of engagements, from the group mean, or average number of engagement, 
                             for it's respective group. Once we have this, we can add it up and get the within group (or factor) variation. 
                             This should match to the 'residuals' row and 'Sum of Squares' column from Table 3. Now we have the SSW, but for the MSW we also need the DF. If you recall, this is the 
                             total sample size (overall N) minus the number of groups (k), in our example then the DF would be 150-3 -- which matches the DF in Table 3.",
                             style = "font-family: 'times'; font-size:14px;"),
                           plotOutput(outputId = "aov_SSW_plot"),
                           p("Now we have all of the ingredients to calculate the MSB and MSW for our F-statistic formula. If you've been following along, you may already have this. If you
                             need a boost, we can refer back to the aov() table, Table 3. You will mind a 'Mean Sum of Squares' column, this contains the information for the MSB ('Image_Type' row) and the MSW 
                             ('Residuals' row). Since the information is all in the table you can calculate and confirm it yourself. For the numerator, ['Image_Type' row & 'Sum of Squares' column] / 
                             ['Image_Type' row & 'DF' column]. Then the denominator, ['Residuals' row & 'Sum of Squares' column] / 
                             ['Residuals' row & 'DF' column]. Each row that we're working on should match the value in 'meansq' column for the respective role. We divide these, and we will
                             get our F-statistic. Now we can infer whether there is a meaningful differences for amongst our factor type (images) in the data we had acquired.",
                             style = "font-family: 'times'; font-size:14px;"),
                           p("But you may be saying, 'Cool story, bro. I still want to know how how these image types meaningful differ between each other. My boss is all over me, and I need
                             to the order of performance across these three.' This is an appropriate question. Once we learn that our image types types vary in the one-way ANOVA, there
                             may not be enough clear evidence from the Figures to see whether one image type, say people, is superior to another, say nature. So we want to get some 
                             qunatitative measure of this. One way to do this is to contrast the different means as we had done before. Fortunately, something like the
                             Tukey Test can calculate what is called the 'Tukey's honest Significance Difference test', which is simply the comparison of the group means for our factor. 
                             The Table 4 reports all of the possible comparisons for our grouping variable. In the table we get each contrast type for our term, i.e. Nature versus City.
                             We get the associated estimate, which is the direction of the effect. For example for Nature-City, -6.0 would indicate the unit difference in means, or number of engagements.
                             Compare this back to the reported descriptives in Table 1. Around this unit change, we have the 95% confidence interval. Given the variability in our sample, when
                             get a distribution of values and estimate where 95% of the values are in this distribution for the data. This has the lower and upper range. Which is directly associated
                             with ethe p-value, which is estimated from the F-distribution given the sample sample, mean and SD (i.e., variability).",
                             style = "font-family: 'times'; font-size:14px;"),
                           tableOutput(outputId = "aov_out_one_tukey"),
                           p("With the ANOVA, it is worth to remember that when we have a lot of groups we can end up making many comparsions which would increase the false positive rate.
                             Therefore, it's worthwhile to consider whether you should perform some type of correction to the significance values (i.e., Bonferroni or Benjamini-Hochberg correction),
                             or set up some contrasts initially that dont require a comparsion of all group means. But this depends on your question, goal and concern about making the incorrect conclusion.",
                             style = "font-family: 'times'; font-size:14px;"),
                           br(),
                           br(),
                           h4("Two-way ANOVA"), 
                           p("Quick note: regarding what inputs impact which outputs. The Var 1 to Var 3 mean/SD values for One-way ANOVA, with Prefix 'Between(1) 
                             will alter the means/SD for Males for City (Var1), Nature (Var2) and People (Var 3). Then, the second set of variables with Prefix
                             'Between(2)' will alter the means/SD for Females City (Var1), Nature (Var2) and People (Var 3). Again, I suggest observing the defaults and
                             then playing around with your own values next. If you have not yet, hit 'Run, Two-way ANOVA!' to simulate the tables and results below.",
                             style = "font-family: 'times'; font-style: italic; font-size:14px;"),
                           p("Above we discussed the One-way ANOVA, which compared the difference on a continuous outcome, engagement on ads, across a categorical variables,
                             our image types. However, we may be interested to add an additional categorical variable, sex. We may have the suspicion that females may respond differently
                             than males across our three types of ads. The Two-way ANOVA provides a statistical test to consider the difference among multiple group means for categorical variables on our outcome (DV), ad engagement.
                             By knowing whether this difference exists and then the direction of this difference, we may acquire valuable information to target our ads with more precision. 
                             Given that the conceptually elements don't substantially change from the One-way ANOVA, this section will be a bit more brief. Remember, the goal is to estimate
                             the difference between our difference of interest (SSB/MSB) for our groups relative to the deviations within groups that we are not interested in (SSW/BSW)",
                             style = "font-family: 'times'; font-size:14px;"),
                           p("As we saw for the one-way ANOVA, our Two-way ANOVA can be written as a linear model as well. I will not go into this here, but we will
                             go over this linear formula more in the 'Regression' tab. I provide it here to simply remind you have aov() can also be thought of as
                             a linear model.",
                             style = "font-family: 'times'; font-size:14px;"),
                           uiOutput(outputId = "aov_two_formula"),
                           p("We will not cover indepth how to compute each sum of squared residuals and mean sum of squared residuals as we
                             did above for the one way another. The conceptual issues are quite similar, we are trying to decompose our signal in the numerator
                             that is related to our factors (or groups). Whereas before we had a single factor, image type, we now have two factors,
                             image type (A) and sex (B). Then, we also have their interaction AB. So similar to the Figures 2 - 4, we can calculate these values
                             the same way but now for an additional factor. To not over burden you with statistical formulas, I will not include them here,
                             but if you are interested in the formula I defer you to the explanation by Susan Holmes at Stanford in this",
                             a("PDF", 
                               href="https://statweb.stanford.edu/~susan/courses/s141/exanova.pdf", target="_blank"),
                             style = "font-family: 'times'; font-size:14px;"),
                           p("Similar to what we saw above for the One-way ANOVA, we can take a look at our means and SDs for each image type and sex group
                             combination. Our image type has 3 levels (city, nature, people and sex has 2 levels (female, male), hence we have a table reflecting the
                             3x2, or 6 means and SDs.",
                             style = "font-family: 'times'; font-size:14px;"),
                           p("By glancing at Table 5, we can see that we continue to have differences across image types, but also some subtle differences between sex
                             for city and people image types. In Table 6 we find the overall N, mean and SD across groups in Table 5.
                             Now we have a numerical representation, we can visually observe these averages and variability within/between groups in Figure 5.",
                             style = "font-family: 'times'; font-size:14px;"),
                           tableOutput(outputId = "aov_sim_two"),
                           tableOutput(outputId = "aov_sim_two_overal"),
                           p("Now that we have taken a look at the numerical points of distribution in Table 5, 
                             it is worthwhile to visually observe the averages and variability within/between groups in Figure 5. We can begin to see how we hace difference
                             across multiple factors, which may suggest a group-by-group or factor-by-factor interaction. What this interaction suggests is that at difference
                             levels of image types and sex we observe greater ad engagement. For instance, we can see that the people image tupe is greater than nature and city,
                             but also that this average is greater for females than males. Furthermore, whereas for city images, the performance is greater for females than males, too.",
                             style = "font-family: 'times'; font-size:14px;"),
                           plotOutput(outputId = "aov_two_plot"),
                           p("To get a statistical representation of this difference, especially in cases where it's difficult to draw these conclusions visually, we an run our 3x2 ANOVA
                             using the aov() in R as we did before. As we saw in the One-way ANOVA results in Table 3, we see similar values for the Two-way ANOVA in Table 7. The only difference
                             is we have two additional rows. These two rows are for our second factor (i.e., group), sex, and then the interaction between our two groups, Image Type and
                             Sex. As before, we have the the SSB values (i.e. Term 'Image_Type', 'Sex' and Interaction Image_Type:Sex), and the SSW value (i.e. reisuals term). Then the association
                             MSB and MSW across these values.",
                             style = "font-family: 'times'; font-size:14px;"),
                           tableOutput(outputId = "aov_out_two"),
                           p("Given there multiple levels at which we're comparing means, we can compute the estimate between each Image Type and Sex combination to parse where
                             the specific difference lie and to what magnitude. Note, there are A LOT (!) of combinations. You'll notice, the estimate here, as it was for the one-way
                             ANOVA, is the mean difference between the two association contrasts. Compare the estimates in Table 8 and Table 5 to observe what these differences mean.",
                             style = "font-family: 'times'; font-size:14px;"),
                           tableOutput(outputId = "aov_out_two_tukey"),
                           br(),
                           br(),
                           p("Packages used - Plotting: ggplot | Simulating data based on means & correlations: mvnorm() via MASS",
                             style = "font-family: 'times'; font-size:10px")
                         )
           )
         )
),

################################
################################
################################   

                            tabPanel("Correlation",
                                     fluidPage(
                                         sidebarLayout(position = "right",
                                             sidebarPanel(
                                                 h4("Select Parameters to Simulate Corr"),
                                                 actionButton("run2", "Onward!",
                                                              style =  "color: #FFF; background-color: #8B0000; border-color: #FFFF00"),
                                                 sliderInput("corr_sample", label = "Sample Size",
                                                             min = 5, value = 50, max = 2000,step = 15),
                                                 sliderInput("corr1",
                                                             label = "Pearson's r between Var 1 & Var 2",
                                                             min = -1, value = .20, max = 1, step = .005),
                                                 sliderInput("corr2",
                                                             label = "Pearson's r between Var 3 & Var 4",
                                                             min = -1, value = -.42, max = 1, step = .005),
                                                 sliderInput("corr_m1", label = "Var 1 Mean",
                                                             min = -20, value = 5, max = 20, step = .5),
                                                 sliderInput("corr_sd1", label = "Var 1 SD",
                                                             min = .01, value = 1.2, max = 10, step = .1),
                                                 sliderInput("corr_m2", label = "Var 2 Mean",
                                                             min = -20, value = -8, max = 20, step = .5 ),
                                                 sliderInput("corr_sd2", label = "Var 2 SD",
                                                             min = .01, value = 2.2, max = 10, step = .1),
                                                 sliderInput("corr_m3", label = "Var 3 Mean",
                                                             min = -20, value = 3.5, max = 20, step = .5),
                                                 sliderInput("corr_sd3", label = "Var 3 SD",
                                                             min = .01, value = .75, max = 10, step = .1),
                                                 sliderInput("corr_m4", label = "Var 4 Mean",
                                                             min = -20, value = -11, max = 20, step = .5),
                                                 sliderInput("corr_sd4", label = "Var 4 SD",
                                                             min = .01, value = 3.3, max = 10, step = .1)
                                                 #selectInput('group_by', label = 'Group By',
                                                 #            choices = c('NULL', 'Sex', 'Race'),
                                                 #            selected = NULL),
                                             ),
                                             mainPanel(
                                                 h2("Correlation plots and effects from specified data"),
                                                 p("What does this tab represent? Similar to the T-test tab and normally distributed dataset is simulated. Unlike the T-test
                                                   tab where we simulate the data solely based on your mean and standard deviation (SD), here your data is generated based on your means
                                                   for four variables and some pre-specified correlation between Var1 & Var2 and Var3 & Var4.",
                                                   "Below, the effect size and p-value is printed to see if/how these change",
                                                   style = "font-family: 'times'; font-size:14px"),
                                                 p("The association between two variables (hence forth indicated by '~') is plotted. Specifically, the correlation between 
                                                   Var1 ~ Var2 and Var3 ~ Var4. We could have specified a correlation among other combinations, such as Var1 ~ Var3, but we will 
                                                   simplify things and not get into that just yet. For now, we will plot the two correlations.",
                                                   style = "font-family: 'times'; font-size:14px"),
                                                 br(),
                                                 p("Before we get into the plots, let's first take a look at the formulation for the Pearson's r correlation. The pearson correlation
                                                 provides a coefficient for a linear association between two variables, which can be zero, positive, or negative.
                                                   At first glance, the below formula may look a bit overwhelming. But we can simplify the equation into the numerator and 
                                                   the denominator. The denominator calculates the overall covariation among our variables (such as, when Var1 goes up by 2, Var2 
                                                   may go up 4). Let's first break down (x−m x)(y−m y). 'x' and 'y' are the observed values for our variables. So if we had 10 observations
                                                   of X and Y variables, we would have ten 'x' and 'x' values. Let's say in this case X is the measure of how many miles
                                                   that I ran during a span of 2 hrs for a given day and Y is a measure of how tired I reported I was after a run. I did this 10 times across two weeks.
                                                   Then, the 'm x' and 'm y' are the sample means for each of those values. So if I did 10 runs, for X and Y I would get the mean
                                                   across the 10 observations, respectively.",
                                                   style = "font-family: 'times'; font-size:14px"),
                                                 uiOutput(outputId = "corr_formula_p"),
                                                 p("Because I like fried chicken and am inconsistent with my running, we will see variability in the distance and fatigue that I report.
                                                   This variability we can leverage to determine the magnitude of the relationship between my running and self-reported fatigue. 
                                                   If I always ran the same distance and never varied in my fatigue, first that would be really weird and second that would offer
                                                   no variability that we may leverage. To get the covariance of across values for Var1 (running) and Var2 (fatigue), we would take the ten sets of (x−m x)(y−m y), such as (3 - 2)(9 - 8), (1.5 - 2)(5 - 8), 
                                                   (3 - 2)(9 - 8), (3 - 2)(8.5 - 8), etc... and sum (∑ simply means summation over)  across those ten iterations. Now we have the numerator. 
                                                   Normally, to get the sample covariance the numerator would be (n - 1), so for ten observations we would divide by 10-1, or 9. 
                                                   HOWEVER, this is a sample constrained numerical value and we want something more standardized and interpretable across studies. 
                                                   The Pearson's correlation achieves this standardized unit via the denominator. It standardizes the numerator by the samples standard deviations for the
                                                   sample. By doing so, we achieve values that will always be constrained to +/- 0.0 to 1.0. ",
                                                   style = "font-family: 'times'; font-size:14px"),
                                                 p("Now that we have collected the data, we can calculate the correlation between Var1 and Var2 by hand, or use Excel, R, SPSS or some other software
                                                   to do the math for us. For simplicity, if you have pushed 'Onward' you will see scatter plots of the simulated data for the specified correlation.",
                                                   style = "font-family: 'times'; font-size:14px"),
                                                 plotOutput(outputId = "corr_plot1"),
                                                 p("Figure 1 and Figure 2 provides the strength of your correlation represented by the linear line and the Pearson's r coefficient. Both represent the
                                                   direction and magnitude for the associations. You can play around with sample size, means, and Pearson r coefficient sizes to see how things change.
                                                   You'll notice the bar lines on the perimeter of the graph represent the distribution of the data points, so as data points cluster you'll see the bars
                                                   get darker (which is just clustering of values.",
                                                   style = "font-family: 'times'; font-size:14px"),
                                                 p("Just like other statistical tests, people like to see whether a correlation, or association, among two variables is significant. So is there a
                                                 significant association between the distance I ran and how tired I am? The table below reports whether the association between your variables is 
                                                 or is not significant (note: if you see p.value = 0, that simply means that p < 0000000). The p-value is calculated using a t-distribution with n – 2 degrees of freedom.
                                                 The statistical test makes the assumption that the correlation between two variables in the population is zero. So when p < .05 between Var1 and Var2, 
                                                 for example, we are saying the association among these variables in our 
                                                   sample is significantly greater than zero.",
                                                   style = "font-family: 'times'; font-size:14px"),
                                                 p("An important caveat with the correlation is while we do get an association between two variables, we cannot know whether one causes the other.
                                                   In our example, if we find that the correlation between running distance and self-reported fatigue is p < .0001, one may make the claim
                                                   that the distance caused the fatigue. While it is a safe bet in my case, as I am out of shape. There are caveats in something even as basic as this.
                                                   Perhaps I have an injury that with longer distances is imacting my fatigue. Or, maybe it's that I'm not well hydrated or ate appropriately
                                                   that is causing distant to make me more tired. So it's less the running, my more the issue of not having enough carbs or water.",
                                                   style = "font-family: 'times'; font-size:14px"),
                                                 br(),
                                                 tableOutput(outputId = "corr_test"),
                                                 br(),
                                                 p("It is important to keep in mind the importance of a correlation rather than solely focusing on a p-value.
                                                   Funder & Ozer (2019; DOI: 10.1177/2515245919847202), discussed the sense and nonsense of effect sizes.
                                                   How plausible and meaningful of an  effect depends on the context. While we may get really large or really small
                                                   effects in our individual samples, we have to reflect on these values to determine whether it is meaningful to what we are 
                                                   trying to measure. For example, does it make sense that age and height correlate r = .65 in a sample of <25 yr olds, but a measure
                                                   of some gene and some behavior correlate r = .45? Not necessarily -- height and age are a pretty large and observable effects in the
                                                   real world. We can see the association. But genes and behaviors are extremely complex, so to see a large correlation should make us
                                                   think 'how is this plausible?' and 'why did this occur in our sample?' In the tab on 'stability of effects', I try to
                                                   provide an example how large effects may arise that are likely to be small in the real-world",
                                                   style = "font-family: 'times'; font-size:14px"),
                                                 p("One other thing to keep in mind with correlations is to pay attention to our sample, varibles and the correlation we find,
                                                   rather than searching for the p-value. As you can see in Figure 3 below, a Pearson's correlation of r = .15 can be
                                                   non-significant and ignored until a sample threshold is hit. The p-value can be misleading.",
                                                   style = "font-family: 'times'; font-size:14px"),
                                                 plotOutput(outputId = "corr_n_p"),
                                                 p("Then, the types of people and/or groups that we include for an 'overall' correlation may
                                                   skew our interpretation from the underlying group correlations, esp. when these groups differ
                                                   on means. For example, say we have a sample of 150 who self-reported on two variables. The below table
                                                   provides the means for each of these two variables. In the overall sample, the means are 6.3 for X1 and 4.8 for X2,
                                                   and the correlation between these variables is r = - .13. So as X1 goes up, X1 goes down by some small amount. However,
                                                   in this sample, we have three distinct groups. For simplicity, I made these groups equal in size. Group 1 and Group 2 have correlations
                                                   that are positive, X1 mean is similar and X2 mean is different between the two groups. On the otherhand, group 2 has a negative correlation
                                                   between X1 and X2, which is different from both group 1 and group 2. Then for group 2 the mean for X1 is higher than the other two, but X2 is somewhere in the middle.",
                                                   style = "font-family: 'times'; font-size:14px"),
                                                 tableOutput(outputId = "corr_grp_tbl"),
                                                 p("What we observe when we plot all of the data points (n = 150), is this relatively flat but negative association between X1 and X2.
                                                   But when we parse this out by groups, we can see that for most of our participants there is a positive association, but because of the
                                                   difference in the means between these groups, this masks their relative associations. Hence, when calculating correlations, it's good to plot the data
                                                   and then consider what important information you are missing, e.g., treatment groups and sampling nuance.",
                                                   style = "font-family: 'times'; font-size:14px"),
                                                 plotOutput(outputId = "corr_grp_plt"),
                                                 h4("Alt to Pearson's r: Spearman's rho",
                                                    style = "font-family: 'times'"),
                                                 p("Alternative to Pearson's r, one can use the spearman's formula which computes the
                                                   correlation by the rank of x and y. The formula is similar with the exception of x = x', 
                                                   whereby x' is the rank(x)... Will expand more on this soon...",
                                                   style = "font-family: 'times'; font-size:14px"),
                                                 br(),
                                                 uiOutput(outputId = "corr_formula_rho"),
                                                 br(),
                                                 tableOutput(outputId = "corr_test3"),
                                                 br(),
                                                 br(),
                                                 p("Packages used - Plotting: ggplot | Simulating data based on means & correlations: mvnorm() via MASS",
                                                   style = "font-family: 'times'; font-size:10px")
                                             )
                                         )
                                     )
                            ),

################################
################################
################################

                            tabPanel("Stability of Effect",
                                     fluidPage(
                                         sidebarLayout(position = "right",
                                             sidebarPanel(
                                                 h4("Select Parameters to Simulate Data at Specified Pearson's r"),
                                                 actionButton("run3", "Onward!", 
                                                              style =  "color: #FFF; background-color: #8B0000; border-color: #FFFF00"),
                                                 sliderInput("corr3",
                                                             label = "Pearson's r between Var 1 & Var 2",
                                                             min = 0, value = .2, max = 1, step = .005),
                                                 #sliderInput("samp_range",
                                                 #            label = "Sample (N) Range for Simulation",
                                                 #            min = 5, max = 10000, value= c(10,1000)),
                                                 sliderInput("stabcorr_m1", label = "Var 1 Mean",
                                                             min = 0, value = 5, max = 30, step = .5),
                                                 sliderInput("stabcorr_sd1", label = "Var 1 SD",
                                                             min = .01, value = 1.2, max = 10, step = .1),
                                                 sliderInput("stabcorr_m2", label = "Var 2 Mean",
                                                             min = 0, value = 9, max = 30, step = .5),
                                                 sliderInput("stabcorr_sd2", label = "Var 2 SD",
                                                             min = .01, value = 2.2, max = 10, step = .1)
                                                 ),
                                             mainPanel(
                                                 h2("Stability of Correlation based on N"),
                                                 p("What does this tab represent? In some values (or use defaults) to generate a normally distributed dataset for a specified Pearson's r correlation. 
                                                 Using this simulated data (N = 1500), a correlation is resampled at random sample intervals from 5 to 1500 and a plot is created to demonstrate 
                                                 how much a correlation can deviate from the full sample correlation. Full disclosure, this tab was inspired by Schönbrodt & Perugini 
                                                 (2013, Journal of Research in Personality). The formula here is based on the Pearson's r formula, but for comparison a Spearman rho 
                                                 is also provided. Because we are simulating different data and running several functions that take time, it may take up to 6-8 seconds 
                                                   for the figures to load.",
                                                   style = "font-family: 'times'; font-size:14px"),
                                                 br(),
                                                 br(),
                                                 br(),
                                                 h4("Stability of Pearson r and Spearman's rho",
                                                    style = "font-family: 'times'"),
                                                 p("In research, it's often difficult to get large enough samples (due to budget constraints) to estimate the 'true' population value.
                                                 When we do collect our samples, ensuring a representative sample is also difficult... among other issues. Consider reading more about the persistent
                                                 biases of samples in Developmental Psychology by Nielsen et al (2017, DOI: 10.1016/j.jecp.2017.04.017).
                                                 Because of this, small and/or not representative samples of the population may result in associations that are true in to the 
                                                 sample (i.e data collected data), but perhaps deviate a bit (to quite a bit) from what may be observed for the entire 
                                                 population (or different samples). This can be demonstrated in the stability of a correlation.",
                                                   style = "font-family: 'times'; font-size:14px"),
                                                 p("In this tab, for the first example a dataset is generated for N = 1500 based on the correlation and means provided. 
                                                 From this sample, we can randomly select samples (i.e. subsample) for the correlation between two variables, Var1 and Var2. 
                                                 By doing this repeatedly from sample size 5 to 1500, and re-estimating the Pearson's correlation, we can see how much the 
                                                 correlation deviates from what you pre-specified. Figure 1 captures this variability using the correlation that is specified. 
                                                 In the figure you will likely observe the most variability variability in samples that contain less than 100 participants.
                                                 However, as you increase the sample size, the correlation that is true (i.e., what you know is true, because we specified it!), 
                                                   this value becomes apartment.",
                                                   style = "font-family: 'times'; font-size:14px"),
                                                 p("You might be thinking, 'well, in the Pearson's correlation tab you talked about Pearson's and Spearmans correlations, 
                                                 does one correlation method provide better results than another?' That's a fair question. To provide this example here, 
                                                 in Figure 2 I plot the stability using both Perason and Spearman correlation. Compare them for the correlation you specified. 
                                                 Unfortunately, what you will likely notice is a consistent pattern across both correlations methods. While Spearson uses ranking to calculate
                                                   the correlation between two variables, this doesn't necessarily improve the problem we observe here.",
                                                   style = "font-family: 'times'; font-size:14px"),
                                                 plotOutput(outputId = "EffNchange"),
                                                 br(),
                                                 p("One thing you may be thinking is we one sampled a 'single' value at each sample size. Surely this randomness may influence what we see.
                                                 This is a good point. Fortunately, since we're working with simulated data here, we can restimate the correlation at a given sample multiple times
                                                 get an average of the correlation for each sample size. In other words, Figure 1 and Figure 2 are based on individual correlations for each sample size. 
                                                 We selected sample size of 5, 10, 15, 20, etc., only one time and calculated a correlation. Instead, we can repeat this sampling 25 times for each sample size.
                                                 So we can randomly select 25 different datasets that have a sample size of 5, 10, 15, 20, etc. To do this, I simulate a sample size of 6,000 for a correlation 
                                                 of r = .35. Then, for a sample size of 5 to 1000, I repeatedly sample (25x) a random number of participants from the dataset with 6,000 participants and calculate 
                                                 25 correlations between Var1 and Var2 at that sample size. This way we can get an average of the 25 correlations for each sample size and the respective 
                                                 confidence interval (or range of correlations). I plot thes results in Figure 3. Similar to Figure 1 and Figure 2, you will observe variability at smaller samples, 
                                                 with a wider range of values, and more smoothing out with a larger sample size. Of note, the dashed line in Figure 3 is an average of 25 correlations so it will be a tad
                                                   more smooth than what appears in Figures 1 & 2. Since for this example we estimated 5,000 Pearson's r values between Var1 and Var2, we can plot
                                                   the density, or frequency, with which certain values occur. This frequence/density plot is provided in Figure 4. You will see that the most 
                                                   frequent occurening r value is at or near r = .35 (what I simulated the data to have), however, around this there is also a range of values.
                                                   Some of these are near .35, others, substantially away from it.",
                                                   style = "font-family: 'times'; font-size:14px"),
                                                 plotOutput(outputId = "EffNchange2"),
                                                 p("You might be thinking, 'Yeah, well, you know, that's just like, your opinion, man. This is simulated data that is inherently
                                                   not real.' To which I would reply, you are right. The Dude abides, so let me provide a real world example. We can take the 
                                                   publicly available NHANES dataset (https://wwwn.cdc.gov/nchs/nhanes/). The data has over 9000 participants, so we can randomly 
                                                   select 6,000 to match our simulation. To compare an r value that is closer to the .35 in Figure 3, I consider two variables: 
                                                   average Systolic and average Diastolic blood pressure readings. These values in the NHANES dataset represent
                                                   a Pearson's r = .39. So this is close enough to what we use for the Figure 3 simulation. It's not an apples-to-apples comparison,
                                                   but it's close enough.",
                                                   style = "font-family: 'times'; font-size:14px"),
                                                 p("As we did with a random simulated dataset with two variables with a correlation of r = 35 in Figure 3, we do the same thing for the NHANES data.
                                                   We repeatedly (25x) sample a subsample of participants, ranging from 5 to 1000, and calcualte a correlation between the two blood pressure
                                                   ratings, Systolic and Diastolic blood pressure. In Figure 4, as in Figure 3, I plot the average of these correlations for each sample size 
                                                   and the range (95% confidence interval). What you will notice in the real world data is quite similar to what we observed in the simulated data! 
                                                   The correlation, again, varies between r = .30 to r = .50 until roughly a sampe size of ~200 when it begins to stabilize a bit Eventually, 
                                                   we see more stability. This is important, because you may have seen in the Pearson's correlation tab, that what is and isn't significant 
                                                   may jump around. Furthermore, when these values become smaller and more noisy, you will see correlations between two variables change 
                                                   from positive to negative. You can see this in the frequency plot in Figure 4, most r values are positive, but a few were negative. 
                                                   This nuance is critical, because depending on our sample it may directly impact our conclusions.",
                                                   style = "font-family: 'times'; font-size:14px"),
                                                 plotOutput(outputId = "EffNchange3"),
                                                 p("Now, does this mean that what you found in your dataset for a small sample is incorrect? Well, no. You found an estimate that is true 
                                                   for YOUR dataset/sample. Whether this value is true or not in subjects in the real world is... less clear. Issues of small samples and
                                                   eratic findings (or spurious effects, as it is commonly referred to) is not all that new in research. As discussed in Button et al (2013, DOI:
                                                   10.1038/nrn3475), Ioannidis (2005, DOI: 10.1371/journal.pmed.0020124); Vul et al. (2009; 10.1111/j.1745-6924.2009.01125.x), abnormally large
                                                   correlations in small sample sizes shouldn't be all that surprising. Empirical research for a while has relied on p-values to quantify what is
                                                   and isn't important. However, as shown in the T-test and Pearson's correlation tab, p-values are susceptible to your sample size (or power) but also
                                                   the size an effect size (or the magnitude of an association). So, if you have a small sample size, say 60 participants, for a two-tailed significance test 
                                                   and power of 80 (i.e., 80%), the correlation that will meet this p < .05 cut-off is r = .35. In other words, the only associations that you will highlight
                                                   as significant are those that met your p < .05 threshold, so they will have no choice but to be large.",
                                                   style = "font-family: 'times'; font-size:14px"),
                                                 p("As I hopefully have demonstrated, it doesn't mean you are unlikely to find significant correlations in small samples. What you will find are
                                                   the spurious correlations that deviate from the true correlation. Does this mean that for your sample the correlation is wrong? Again, no. But
                                                   if you would have collected more data, your correlation of r = .65 could very well become r = .15, or worse, r = -.15. There is a lot of variability
                                                   between samples, so what may be driving these 'spurious effects' are issues of measuremen or other unique characteristics of your sample. This is why
                                                   it is important to interpret large effects with some level of caution. Like noted in Funder & Ozer (2019; DOI: 10.1177/2515245919847202),
                                                   it's imperative that we consider what is a plausible and meaningful relationship (or effect size) in your respective field. If what you find
                                                   something that deviates from what is expected, it's good to evaluate what contributed to this deviation in your data or the prior
                                                   reported literature.",
                                                   style = "font-family: 'times'; font-size:14px"),
                                                 br(),
                                                 br(),
                                                 p("Packages used - Plotting: ggplot | Simulating data based on means & correlations: mvnorm() via MASS",
                                                   style = "font-family: 'times'; font-size:10px")
                                                 )
                                             )
                                         )
                                     ),


################################
################################
################################

                                    tabPanel("Regression",
                                             fluidPage(
                                                 sidebarLayout(position = "right",
                                                     sidebarPanel(
                                                         h4("Select Parameters to Simulate Data at Specified Pearson's r"),
                                                         actionButton("run5", "Run Regression!", 
                                                                      style =  "color: #FFF; background-color: #8B0000; border-color: #FFFF00"),
                                                         sliderInput("m_reg_sample",
                                                                     label = "Sample (N) for Simulation",
                                                                     min = 2, max = 10000, value= 150),
                                                         sliderInput("m_reg_m1", label = "DV Mean",
                                                                     min = -20, value = 5, max = 20, step = .5),
                                                         sliderInput("m_reg_sd1", label = "DV SD",
                                                                     min = .01, value = 1.15, max = 10, step = .1),
                                                         sliderInput("m_reg_m2", label = "IV 1 Mean",
                                                                     min = -20, value = 7.5, max = 20, step = .5 ),
                                                         sliderInput("m_reg_sd2", label = "IV 1 SD",
                                                                     min = .01, value = 2.2, max = 10, step = .1),
                                                         sliderInput("m_reg_m3", label = "IV 2 Mean",
                                                                     min = -20, value = 19.2, max = 20, step = .5),
                                                         sliderInput("m_reg_sd3", label = "IV 2 SD",
                                                                     min = .01, value = 3.8, max = 10, step = .1),
                                                         sliderInput("m_reg_corr1",
                                                                     label = "Pearson's r between DV & IV 1",
                                                                     min = 0, value = .22, max = 1, step = .005),
                                                         sliderInput("m_reg_corr2",
                                                                     label = "Pearson's r between DV & IV 2",
                                                                     min = 0, value = .45, max = 1, step = .005),
                                                         sliderInput("m_reg_corr3",
                                                                     label = "Pearson's r between IV 1 & IV 2",
                                                                     min = 0, value = .41, max = 1, step = .005),
                                                         selectInput("outlier", label = "Include Outliers on IVs?", 
                                                                     choices = c("None","One","Two","Three"),
                                                                     selected = "None")
                                                         ),
                                                     mainPanel(
                                                         h2("Regression"),
                                                         p("The previous tabs cover mean differences in the context of T-tests and ANOVA and linear associations via 
                                                           correlations. In some of those tabs I referred to 'linear' models/formulas, so in this tab we will cover Regression model which
                                                           measures the association between continuous variables.
                                                           First, we will go over linear regression, whereby we explain the association between a single IV and a DV variable. Then,
                                                           we consider multiple regression, which is the association between more than one IV and a DV. The linear regression will provide conceptual
                                                           information that we will build on with multiple regression. Given how linear regression coefficients are calculated, as discussed in
                                                           the",
                                                           a("S3E09 Quanatitude podcast.", 
                                                             href="https://www.buzzsprout.com/639103/9460323", target="_blank"),
                                                           "The tab here simulates data
                                                           using a normal distribution based on means and their correlations. So you can control the association between your IVs and DVs
                                                           to determine how these associations impact your results. I also include an 'outlier' option which adds in up to three random values
                                                           that are five SDs larger than than the mean for that data. For the linear regression I try to provide the context of how these
                                                           models related to what we discussed before, correlations. Finally, I will include a section on logistic regression which measures
                                                           the probability, or odds, of a binary outcome (0/1) occuring given some IVs.",
                                                           style = "font-family: 'times'; font-size:14px"),
                                                         p("The tab here simulates data
                                                           using a normal distribution based on means, SDs and their correlations. So you can control the association between your IVs and DVs
                                                           to determine how these associations impact your results. I also include an 'outlier' option which adds up to three random values
                                                           that are five SDs larger than than the mean for that data. For the linear regression I try to provide the context of how these
                                                           models related to what we discussed before, correlations. Finally, I will include a section on logistic regression which measures
                                                           the probability, or odds, of a binary outcome (0/1) occuring given some IVs.",
                                                           style = "font-family: 'times'; font-size:14px"),
                                                         p("As before, consider starting with the default values and then adjusting them once you've made it through this section to see how
                                                           estimates change depending on the values we entered. To populate the fields below, remember to hit 'Run Regression!'.",
                                                           style = "font-family: 'times'; font-size:14px"),
                                                         br(),
                                                         h4("Linear Regression",
                                                           style = "font-family: 'times'"),
                                                         p("When we have two variables, we may be interested to estimate their relationship, or more explicitly, how one variable predicts another.
                                                           This can be, for instance, drinking quantities and reward seeking behaviors. There is plenty of empirical
                                                           evidence that reward seeking behaviors are associated with increased drinking quanities. As researchers, we may survey a 
                                                           sample of high school and undergraduate students and estimate how well reward seeking predicts drinking. If we're only interested in the
                                                           explicit association between reward seeking (IV) and the outcome (DV) we can run a linear regression on the observed data to quantify
                                                           this relationship.", 
                                                           style = "font-family: 'times'; font-size:14px"),
                                                         p("A linear regression model is expressed in the equation below. Using our example, we can obtain the estimated value of drinking by identifying the intercept
                                                           (β0) where our line intersects the vertical axis and slope for our IV (β1) with some amount of error/variability (ϵ).
                                                           While our observations are known, our intercept, slope and error are unknown.
                                                           Linear regression uses ordinary least squares (OLS) to estimate these 'unknowns'. In essence what this means is the OLS attempts to fit
                                                           a linear line to our IV that explains the maximum amount of variation (sums of squares) in our DV. Commonly referred to as the 'line of best fit'. 
                                                           This line reduces the deviations between the predicted values and observed observed values. The line of best fit is intended to performed better
                                                           than something like just a mean. For instance, we can explain more deviations (i.e., sum of squared deviations) for drinking behaviors by fitting a line
                                                           that uses a variable to predict this outcome. If this predictor helps us explain more of the deviations in drinking behaviors, we improve our prediction.
                                                           This will make a bit more sense when we go over the figures below.", 
                                                           style = "font-family: 'times'; font-size:14px"),
                                                         uiOutput(outputId = "m_reg_formula_1"),
                                                         p("To better understand our regression coefficients, and see how they related to correlations, we can break down the formula above a bit. For instance, to get the 
                                                           β1, or estimated β1 (b-hat), you will observe that this is the covariation between our independent variable (x) and our dependent variable (y)
                                                           that is scaled by the variance of our independent variable (x). Then, the estimated β0 is the difference between the mean of our dependent variable minus the product of
                                                           the weighted (by β1) mean of our independent variable (x). Hopefully, the formula is easier to follow now.", 
                                                           style = "font-family: 'times'; font-size:14px"),
                                                         uiOutput(outputId = "m_reg_calc_b1"),
                                                         uiOutput(outputId = "m_reg_calc_b0"),
                                                         p("Below are the descriptives: sample size, mean and SD for the data that is simulated here. Of note, this data is simulated based our parameters mean, SD and pearson's r
                                                         based on a normal distribution. This often is unlikely to be the case in most real-world data. 
                                                           But this again is a simple simulation to demonstrate the basic concepts. Note, when outliers are introduced, the values in this table will shift.", 
                                                           style = "font-family: 'times'; font-size:14px"),
                                                         tableOutput(outputId = "m_reg_1_descr"),
                                                         p("We can plot the observed data for Drinking (DV) and Reward Seeking (IV). Then, we can fit a linear line to this data to get a visual representation
                                                           of this association. If you had read other tabs to this point and have followed along here, you will see that this looks a lot like a correlation. Which is correct, because
                                                           both are in the general linear model and represent relationships among two variables. I will expand on this a bit more in the multiple regression section below, but broadly,
                                                           in correlational/cross-sectional data the Pearson correlation and the linear regression are very similar. Nonetheless, in in linear regression the best fit model 
                                                           attempts to explain a greater amount of deviations in the DV than the mean for the DV. So in Figure 1, the best fit line would be the green line and the mean is the redline. While you can't
                                                           always see it with so many datapoints, the green line explains more variability in Drinking than the redline.",
                                                           style = "font-family: 'times'; font-size:14px"),
                                                         plotOutput(outputId = "m_reg_plot_1"),
                                                         p("If we were to compute a regression line for our data, predicting Drinking (DV) by Reward Seeking (IV), we would get several associated regression coefficients. Table 2
                                                           reports these coefficients for the linear regression model. First, we get an intercept (β0), which tells us where the line crosses the vertical axis on the graph.
                                                           Notice, the β0 coefficient, or our intercept, does not map onto a mean from Table 1. This is because, as the formula demonstrated above, β0 is the when our IV, reward seeking, is zero. 
                                                           So β0 is in the abscence of β1. So you can interpret the intercept as 'when Reward seeking is zero, the average drinking quantity is...'. 
                                                           Then we get our  β1, or the estimated coefficient/slope for our predictor, Reward seeking. This is often interpreted as the main effect of IV
                                                           on the DV. The interpretation, in the unstandardized coefficient is one unit increase in our IV (i.e, X), reward seeking, is associated with the slope coefficient increase
                                                           in our DV (i.e., Y). The units of this scale will depend on your scales/measures. If we standardize this unit, we will get a coefficient that is identical to the pearson's r.
                                                           For this predictor we get an associated p-value which is based on our T-statistic. This indicates whether this unit increase is statistically significant",
                                                           style = "font-family: 'times'; font-size:14px"),
                                                         tableOutput(outputId = "m_reg_1_coef"),
                                                         p("In addition to our regression coefficients, what the linear regression model estimates are several overall model fit statistics. For instance,
                                                           in Table 2 we get the associated coefficients and their significance values. However, this model doesn't tell us whether this is a meaningful improvement on
                                                           a simpler model, which would be a mean based model for our DV. What I mean by this is, in Figure 1 we saw the green and red line, representing the line of best fit
                                                           and the mean, respectively. The model fit tells us whether this model explains more than just using the mean. The F-statistic and associated degreees of freedom, DF1 and DF2,
                                                           allow us to see whether this is a significant improvement. DF1 is calculated using the number of IVs - 1 (or p - 1), and DF2 is calculated using the sample N - 2. The model also estimates
                                                           an R-squared value, or what is also referred to as the 'Coefficicent of Determination' and an adjusted R-squared. The R-squared value tells us about the improvement of the best fit line
                                                           over a mean line, and the adjusted R-squared is a penalized version of this. I explain this in greater detail next.",
                                                           style = "font-family: 'times'; font-size:14px"),
                                                         tableOutput(outputId = "m_reg_1_model"),
                                                         p("The R-squared is calculated using the below formula. It is the fraction of Sum of Squares Residuals (SSR) and the Sum of Squares Total (SST). The value will be between 0 and 1. 
                                                         It is often interpreted as the percent of variance in our DV that can be explained by the regression line.
                                                         The SST is the maximum amount of deviation that we have around our DV, drinking. This is the mean model. Like we went over in previous tabs, 
                                                         if we were to calculate the mean and subtract each observed value from the mean and square it, the sum of these values would give us the SST. 
                                                         Now, the SSR (residuals for regression line) are the sum of squared difference between the line predicted value and the observed value. If our regression
                                                           line is REALLY good at predicting Drinking (DV), we will find the we get a small value for SSR. Hence, SSR/SST = 33/320 = 1 - .10 = .90 R squared, or the
                                                           percent of variance in the DV that the IVs explain. ",
                                                           style = "font-family: 'times'; font-size:14px"),
                                                         uiOutput(outputId = "r_sqrd"),
                                                         p("Now, the more IVs you add you will inherently explain more variance. So the models with more predictors will always 'appear' better when we look at R-squared. Most models 
                                                           prefer parsimony over complexity, and thus complex models that have more predictors are penalized. This is done using the formula below. The key value that punishes
                                                           the R-squared value are the number of predictors (p), or IVs. As the sample gets larger, the severity of this punishment decreases, but in some cases may still be meaningful.
                                                           So you are rewarded for sample size and parisomy, so your model has more parameters to estimate the coefficient from.",
                                                           style = "font-family: 'times'; font-size:14px"),
                                                         uiOutput(outputId = "adjst_r_sqrd"),
                                                         p("One unique thing about the R-squared value in a linear regression is that it will be related to the pearson's correlation. In other words,
                                                           since we specified a correlation between our IV and DV to be r =.22, if we take the square root of the r-squared value, this will match our r-value, which is
                                                           reported in Table 4.",
                                                           style = "font-family: 'times'; font-size:14px"),
                                                         tableOutput(outputId = "m_reg_1_corr"),
                                                         br(),
                                                         p("If we wanted to, we could extract the the predicted values and the residuals and see how these relate to the observed data (i.e., what we collected).
                                                           Figure 2 represents the OLS predicted values by the reward observed values. These overlap substantially because the reward values were
                                                           used to predict the outcome. In Figure 3 we can observe the residuals, or the deviation of the observed from the
                                                           predicted values. The black line represents our prediction. You will see that not all observations are along this line. The deviations are the black lines
                                                           that connect the observed point to the fitted line. If we wanted to calculate them manually and add them up, we would get our SSR. In Figure 4 we have the distribution of residuals
                                                           across our IV, reward seeking. These should not have a relationship and the residuals should be evenly scattered across. If there was some relationship,
                                                           there may be some distinct thing going on at different levels of our predictor which may influence our estimates. Finally, Figure 5 are the associations between our residuals and drinking. 
                                                           Given that the regression model explains a small amount of variance, the observed drinking value and the difference between the predicted and observed are highly related. 
                                                           On the otherhand, if the regression model explained a large mount of variance, then the difference between predicted and observed values would not be as related.",
                                                           style = "font-family: 'times'; font-size:14px"),
                                                         plotOutput(outputId = "m_reg_plot_2"),
                                                         br(),
                                                         br(),
                                                         h4("Multiple Regression Example: ",
                                                           style = "font-family: 'times'"),
                                                        p("Up until now, we have focused on the linear relationship between a single predictor (IV) and outcome (DV) using linear regression. However, what if we wanted to predict
                                                          drinking in high school students and undergraduate students but we knew other variables were at play? For instance, there is a literature on Reward Seeking being a
                                                          predictor of Drinking, but there is also a literature on Age predicting Drinking. Moreover, this Age variable is frequently reported to be interrelated to Reward Seeking, too.
                                                          So we may want to predict Drinking using Reward Seeking but accounting for other known variables. 
                                                          This is where Multiple Regression comes into play.",
                                                          style = "font-family: 'times'; font-size:14px"),
                                                        p("Multiple regression is an extension of linear regression. As is apparent in the formula below, multiple regression is the linear regression formula
                                                          with an added predictor. Whereas in linear regression we estimated a line of best fit for only Reward Seeking (β1), in multiple regression we keep β1 but also include
                                                          Age, or β2. If we added another, we'd have a β3, β4, etc. The critical difference between linear and multiple regression si that the outcome (DV) is predicted by
                                                          a combination of the intercept (β0), beta coefficients (β2, β3, etc...) and the error (ϵ) . However, the goal remains the same -- we want a linear line that uses
                                                          the combination of IVs, Age and Reward Seeking, to maximally predict the outcome, Drinking.",
                                                          style = "font-family: 'times'; font-size:14px"),
                                                          uiOutput(outputId = "m_reg_formula_2"),
                                                        p("Above, we discussed how linear regression is very much alike to correlation. Specifically, I provided an example of how β1 is covariation that is scaled by the IV (x).
                                                          Below is the reminder of the Pearson's correlation..",
                                                          style = "font-family: 'times'; font-size:14px"),
                                                          uiOutput(outputId = "mult_corr"),
                                                        p("Now, consider the partial correlation of xy when control for z (expressed as yx.z below). The numerator adjusts the yx correlation by
                                                          accounting for relationship that z has with x and y. Then, the denominator keeps the variable (z) constant for x and y.
                                                          Multiple regression using a similar framework, it estimates the coefficint for an indepdent variable while keeping all other
                                                          values constant (or at zero). We will discuss this more below.",
                                                          style = "font-family: 'times'; font-size:14px"),
                                                        uiOutput(outputId = "mult_part_corr"),
                                                        p("The descriptives are in the table below. Notice, all of the information is similar to what we had for the linear regression,
                                                          now we just added an IV, Age.",
                                                          style = "font-family: 'times'; font-size:14px"),
                                                        tableOutput(outputId = "m_reg_2_descr"),
                                                        p("Now we can take a look at the regression coefficients for our model in Table 5 consisting of DV (Drinking), IV1 (Reward) and IV2 (Age).
                                                          The intercept reflects the same thing as in the linear regression, it is at the point that the line crosses the vertical axis, which is when
                                                          both Reward Seeking and Age are at zero, or help constant. The interpretation of the regression coefficients (or estimates), is the same here as
                                                          in the linear regression, EXCEPT now we adjust for the other variable. We still interpret the coefficient as a one unit increase in the IV, say Age, is related
                                                          to the coefficient increase in the DV, drinking, when all else is held constant. In our case, all else is the other IV, or age. So
                                                          when interpreting Reward Seeking, we say that a one unit increase on our scale Reward Seeking is related to the [coefficient] increase in our
                                                          on scale of Drinking when Age is at zero.",
                                                            style = "font-family: 'times'; font-size:14px"),
                                                        tableOutput(outputId = "m_reg_2_coef"),
                                                        p("As before, we can request the model fit statistics, which tell us how much better we're doing than a mean based model of Drinking.
                                                          We get a significance value based on our F-statistic and degrees of freedomn, the associated p-value (FYI - this will show zero when it exceed several zeros),
                                                          and the R squared and adjusted R squared, our variance explained. Using the coefficients we can gauge the magnitude of the relationship and the
                                                          model fit information provides us with information on how meaningful our model is. Again, mean often depends on context -- what you're studying.",
                                                          style = "font-family: 'times'; font-size:14px"),
                                                        tableOutput(outputId = "m_reg_2_model"),
                                                        p("We can also calculate our standardized coefficients, which adjusts for our scale variability. Providing a value that is more interpretable
                                                          between studies. This standardized value with be comparable to what we'd estimate using a partial correlation, which adjusts for a third variable.",
                                                          style = "font-family: 'times'; font-size:14px"),
                                                        tableOutput(outputId = "m_reg_2_std_beta"),
                                                        p("I report the partial correlation among the variables below, which uses the ppcor package in R. You'll notice that these values will approximate
                                                          our standardized coefficients. Given that we're using a combination of lm.beta() and pcor() functions here, the calculations will not be perfect. But it 
                                                          nicely demonstrates how these two linear problems are related -- which is pretty cool!",
                                                          style = "font-family: 'times'; font-size:14px"),
                                                        tableOutput(outputId = "m_reg_2_semi_par"),
                                                        p("To be continued...",
                                                          style = "font-family: 'times'; font-size:14px"),
                                                        plotOutput(outputId = "m_reg_plot_3"),
                                                        br(),
                                                          br(),
                                                          p("Packages used - Plotting: ggplot | Linear/multiple regression via: lm() | simulating data: mvnorm() via MASS | 
                                                            Stitching ggplots via patchwork by Thomas Lin Pedersen ",
                                                            style = "font-family: 'times'; font-size:10px")
                                                         )
                                                     )
                                                 )
                                             ),
################################
################################
################################

tabPanel("Construct Validation",
         fluidPage(
             sidebarLayout(position = "right",
                           sidebarPanel(
                               h4("Select Parameters to Simulate Data at Specified Pearson's r"),
                               actionButton("runN", "TBD!", 
                                            style =  "color: #FFF; background-color: #8B0000; border-color: #FFFF00")
                           ),
                           mainPanel(
                               h2("Construct Validation: TBD"),
                               p("Considering how I would like to present this. The structure and function is TBD. In the meantime, here 
                                 are some quotes on constructs",
                                 style = "font-family: 'times'; font-size:14px"),
                               p("Cronbach & Meehl (1957) 'Validation takes place when research believes an instrument 
                                 reflects a particular construct to which a meaning is attached' (pg 290)",
                                 style = "font-family: 'times'; font-size:14px"),
                               p("Loevinger (1957) 'Traits exist in people; constructs 
                                 (here usually about traits) exist in the minds and magazines of psychologists.' pg 642",
                                 style = "font-family: 'times'; font-size:14px"),
                               p("Shadish, Cook & Campbell (2002), a)	Constructs are almost always 
                                 couched in terms that are more abstract than elements in the experiment ",
                                 style = "font-family: 'times'; font-size:14px"),
                               p("Flake & Fried (2020) Refer to measurement as, “any approach that researchers take to create a number 
                                 to represent a variable under study” pg (458)",
                                 style = "font-family: 'times'; font-size:14px"),
                               p("Proulx & Morey (2021) 'In some cases, psychologists may change theoretical labels to obscure the 
                                 origins in order to meet some level of novelty/ground breaking finding' (pg 673)",
                                 style = "font-family: 'times'; font-size:14px"),
                               br()
                           )
             )
         )
),
################################
################################
################################
tabPanel("CFA & PCA",
         fluidPage(
             sidebarLayout(position = "right",
                           sidebarPanel(
                               h4("Select Parameters to Simulate Data Factor Loads"),
                               actionButton("run6", "Simulate Population!", 
                                            style =  "color: #FFF; background-color: #8B0000; border-color: #FFFF00"),
                               sliderInput("sem_sample",
                                           label = "Sample (N) for Simulation",
                                           min = 2, max = 10000, value= 100),
                               sliderInput("sem_reps",
                                           label = "Repititions (N) to Generate Population Data",
                                           min = 1, max = 300, value= 5),
                               selectInput(inputId = "standardize", label = "Standardize Lavaan Output?",
                                            choices = c('all','latent','FALSE'), selected = "FALSE"),
                               actionButton("run6.1", "Run Specified Sample Model", 
                                            style =  "color: #FFF; background-color: #ee9a00; border-color: #8B0000")
                           ),
                           mainPanel(
                               h2("CFA/PCA: Simulated Data via CFA Factor Structure"),
                               p("What does this tab represent? A dataset is generated via specified population parameters: loadings, covariance, and variances.
                                 Then, using this data CFA and then PCA (soon to come) models are presented using this simulated data.",
                                 style = "font-family: 'times'; font-size:14px"),
                               br(),
                               p("Please use the below input field to specify laavan syntax of a model that you wish to generate population data for.
                                 If you are unfamiliar with laavan syntax, review tips at: https://lavaan.ugent.be/tutorial/syntax1.html.
                                 As noted at the aforementioned link, '=~' is a measured by indicator used for latent variable definitions; '~'
                                 is 'regressed on' used to specify regression models; '~~' is 'correlated with' indicating covariance in cases of Var1~~Var2,
                                 and residuals in cases of Var1~~Var1; and '~ 1' specifies the intercept.
                                 The following text field and the CFA field below operate similar as they would in Rstudio. Non-laavan strings
                                 will not work. However, lavaan code for starts/constraint of parameters should work. Of note, the text fields start you off,
                                 but you can build/revise these by adding additional factors, items, covariances, residuals, etc. Depending on the sample size, 
                                 repititions, model complexity and variable numbers, expect delays to (re)generate data.",
                                 style = "font-family: 'times'; font-size:14px"),
                               textAreaInput(inputId = "sim_fact_coeff", 
                                             label = "Input Factors, Items loadings and Correlations for Sim Data",
                                             value = "\n#Factor item loadings \nFA1 =~ .5*a1 + .5*a2 + .5*a3 \nFA2 =~ .5*b1 + .5*b2 + .5*b3\n#Factor correlation\nFA1 ~~ .5*FA2
                                             ", 
                                             width = '100%', rows = 8),
                               h4("Proposed CFA/SEM syntax for Population Data",
                                  style = "font-family: 'times'"),
                               textOutput(outputId = "sim_model"),
                               h4("Correlations Among Simulated Data",
                                  style = "font-family: 'times'"),
                               p("These are the correlations among the correlations in the simulated population papameters averaged
                                 *across* N number of simulations.",
                                 style = "font-family: 'times'; font-size:14px"),
                               tableOutput(outputId = "sem_corr"),
                               h4("Proposed CFA/SEM Sample Model",
                                  style = "font-family: 'times'"),
                               textAreaInput(inputId = "cfa_model", 
                                             label = "Input lavaan() model for Proposed CFA",
                                             value = "\n#Factor item loadings \nFA1 =~ a1 + a2 + a3 \nFA2 =~ b1 + b2 + b3\n#Factor correlation\nFA1 ~~ FA2
                                             ", 
                                             width = '100%', rows = 8),
                               tableOutput(outputId = "sem_table"),
                               p("Returning Plot with Standardized Values",
                                  style = "font-family: 'times', font-size = 14px"),
                               plotOutput(outputId = "sem_plot"),
                               p("Packages used: Data was simulated via simsem() by Sunthud Pornprasertmanit et al. & lavaan() by Yves Rosseel. 
                               Parameter table was generated using parameters() by Daniel Lüdecke et al. 
                                 The plot was generated using semPlot()",
                                 style = "font-family: 'times'; font-size:10px")
                           )
             )
         )
)
)




local_server <- function(input, output, session){
    thematic::thematic_shiny() # so plots/tables match shiny theme


####################################     
#################################### 
# Creating Diff data, using Reactive
###################################

  
  
  ####################################    
# Preliminary and terminology examples
  #################################### 
  
  ## Distributions
  output$dist_plot <- renderPlot({
    m <- input$Distribution_mat
    
    set.seed(1)
    Random_Normal = data.frame(Value = rnorm(n = as.numeric(m[1,1]), mean = as.numeric(m[2,1]), sd = as.numeric(m[3,1])), 
                               Distribution = "Normal Dist")
    T_Distribution = data.frame(Value = rt(n = as.numeric(m[2,1]), df = as.numeric(m[2,4])), 
                                Distribution = "T-Distribution")
    Chi_Square = data.frame(Value = rchisq(n = as.numeric(m[3,1]), df = as.numeric(m[3,4])), 
                            Distribution = "Chi-Square Dist")
    Random_Uniform = data.frame(Value = runif(n = as.numeric(m[4,1]), min =as.numeric(m[4,5]), max = as.numeric(m[4,6])), 
                                Distribution = "Random Uniform Dist")
    
    data_d = rbind(Random_Normal, T_Distribution, Chi_Square, Random_Uniform)
    data_d %>% 
      ggplot(aes(x = Value, colour = Distribution)) +
      geom_histogram(bins = 40, aes(fill = Distribution), colour = "black",) +
      facet_wrap(~Distribution, scale = "free") +
      ggtitle("Figure 3 - Distribution plot based on simulated data from table input") +
      theme_minimal()
    
  })
  
  # Mean versus covariation differences
  
  
  output$cov_mean_ex <- renderPlot({
    r_ex <- input$cov_examp
    
    set.seed(1)
    r_dat <- data.frame(  
      mvrnorm(n = 150, # input$samp_range[2]
              mu = c(as.numeric(r_ex[1,1]),as.numeric(r_ex[1,2])), # means input by user
              Sigma = matrix(c(1,as.numeric(r_ex[2,1]),as.numeric(r_ex[2,2]),1), # correlation specified by user
                             nrow = 2), 
              empirical = TRUE)
    )
    
    cohens_d <- function(mean_1, mean_2, sd_1, sd_2){
      pooled_sd<- sqrt((sd_1^2+sd_2^2)/2)
      d <- (mean_1 - mean_2)/pooled_sd
      print(paste("Cohens d = ",round(d)))
    }
    
    mean_d <- cohens_d(mean(r_dat$X1), mean(r_dat$X2), sd(r_dat$X1), sd(r_dat$X2))
    cor_r <- round(as.numeric(cor(r_dat$X1, r_dat$X2)),2)
    
    theme_set(theme_minimal())
    ex1 <- r_dat %>% 
      gather(key = "Variables", value = "Scores", X1:X2) %>% 
      ggplot(aes(x = Variables, y = Scores, colour = Variables)) +
      geom_boxplot(show.legend = FALSE) +
      geom_jitter() + 
      ggtitle("Figure 5 - Plotting Means (Top) and Correlation (Bottom)")+
      annotate("label", x = "Effect", y = mean(r_dat$X1), 
               label = paste0(mean_d))+
      theme(legend.position = "none")
    
    ex2 <- r_dat %>% 
      ggplot(aes(x = X1, X2)) +
      geom_point()+
      geom_smooth(method = "lm", formula = 'y ~ x', se = F, colour = "red") +
      annotate("label", x = max(r_dat$X1)*.9, y = max(r_dat$X2)*.9, 
               label = paste0("Pearson's r: ",cor_r))+
      xlab("X1") +
      ylab("X2")
    
    
    
    ex1 / ex2 + plot_layout(guides = 'collect')
    
    
    })


####################################    
  # T-test
#################################### 
  
  ### simulation data based on user providing 2 means & 2 sds
    data <- eventReactive(input$run1, {
        data<- data.frame(var1 = rnorm(n = input$sample,
                                       mean = input$mean1,
                                       sd = input$sd1),
                          var2 = rnorm(n = input$sample,
                                       mean = input$mean2,
                                       sd = input$sd2))
        
        
        
        grp_values <- rnorm(n = input$sample,
                            mean = c(input$grp_m1, input$grp_m2),
                            sd = c(input$grp_sd1, input$grp_sd2))
        groups <- rep(letters[1:2], length.out = input$sample)
        
        grps_df <- data.frame(grp_values, groups)
        
        data.frame(data, grps_df)
    })
  

  ### Mean Histogram plots
  output$hist_var1 <- renderPlot({
    ggplot(data(), aes(x = var1, y = "")) +
      geom_boxplot() +
      geom_jitter(color="black", size=0.4, alpha=0.9) +
      ggtitle("Fig 1. Var 1 Mean/SD") +
      theme_minimal()
  })
  
  output$hist_var2 <- renderPlot({
    ggplot(data(), aes(x = var2, y = "")) +
      geom_boxplot() +
      geom_jitter(color="black", size=0.4, alpha=0.9) +
      ggtitle("Fig 2. Var 2 Mean/SD") +
      theme_minimal()
  })
  
  
  ### One Sample t-test
  output$one_t_formula <- renderUI({
    
    withMathJax(
      print(paste0("$$ t = \\frac{m-\\mu}{s/\\sqrt{n}} $$"))
    )
  })
  
  output$one_t_table <- function(){
    data() %>% 
      gather(key = "Variable", value = "Data", var1:var2) %>% 
      group_by(Variable) %>% 
      dplyr::summarise('Sample Size (N)' = n(), 
                       'Mean' = mean(Data),
                       'SDs' = sd(Data),
                       'Population Mean' = input$pop_mean) %>% 
      kable(digits = 2) %>%
      kable_styling("striped",
                    full_width = T, font_size = 12, html_font = 'Times') 
  }
  
  output$t_test_dist <- renderPlot({
    
    data <- data.frame(n_dist = rnorm(n = input$sample, 
                                      mean = input$mean1, 
                                      sd = input$sd1))
    
    ggplot(data = data, aes(x = n_dist)) +
      geom_histogram(aes(y = ..density..), bins = 30,
                     colour = "black",
                     fill = "white") +
      stat_function(fun = dnorm,
                    args = list(mean = mean(data$n_dist), 
                                sd = sd(data$n_dist))) +
      geom_vline(xintercept = mean(data$n_dist)+sd(data$n_dist), linetype = "dashed",
                 color = "blue", size = .5)+
      geom_vline(xintercept = mean(data$n_dist)-sd(data$n_dist), linetype = "dashed",
                 color = "blue", size = .5)+
      geom_vline(xintercept = mean(data$n_dist)+(sd(data$n_dist)*2), linetype = "dashed",
                 color = "green", size = .5)+
      geom_vline(xintercept = mean(data$n_dist)-(sd(data$n_dist)*2), linetype = "dashed",
                 color = "green", size = .5)+
      geom_vline(xintercept = mean(data$n_dist)+(sd(data$n_dist)*3), linetype = "dashed",
                 color = "black", size = .5)+
      geom_vline(xintercept = mean(data$n_dist)-(sd(data$n_dist)*3), linetype = "dashed",
                 color = "black", size = .5)+
      geom_vline(xintercept = input$pop_mean, linetype = "solid",
                 color = "red", size = 1)+
      labs(title = "Fig 3. Distribution +/- SD around Sample Mean, compared to Population Mean", 
           caption = "Red solid: Pop Mean; Blue Dashed, +/- 1sd; Green Dashed, +/- 2sd; Black, +/- 3SD")+
      xlab("Normal Distribution")+
      ylab("Density (bin = 30)")+
      theme(text = element_text(size = 12, family = "times"))+
      theme_minimal()
  })
  
  
  output$t_test_dist_t <- renderPlot({
    data <- data()
    onet_1 <- t.test(x = data()$var1,
                     mu = input$pop_mean,
                     conf.level = 1-input$alpha)
    plot(onet_1)
  }
  )
  
  output$t_test_dist_t2 <- renderPlot({
    data <- data()
    onet_2 <- t.test(x = data()$var2,
                     mu = input$pop_mean,
                     conf.level = 1-input$alpha)
    plot(onet_2)
  }
  )
  
  
  output$one_t_form_denom <- renderUI({
    withMathJax(
      print(paste0("$$ denominator = {s//\\sqrt{n}} $$"))
    )
  })
  
  output$t_test_reduc_sd_1 <- renderPlot({
    data <- data()
    datalist = list()
    
    for (i in seq(5,2000,5)) {
      
      sd1 <- (input$sd1)/sqrt(i) 
      datalist[[i]] <- data.frame('SampleSize' = i, "SD" = sd1)
    }
    
    data_sd <- do.call(rbind, datalist)
    
    
    ggplot(data = data_sd, aes(x = SampleSize, y = SD)) +
      geom_line()+
      labs(title = "Fig 4. Standard deviation scaled by Sample Size for Variable 1", 
           caption = "Sample Size range: 5 to 2000 in intervals 5")+
      xlab("Sample Size")+
      ylab("Denominator: One-Sample t-test")+
      theme(text = element_text(size = 12, family = "times"))+
      theme_minimal()
  })
  
  ### Two sample t-test
  output$two_mean_t_formula <- renderUI({
    
    withMathJax(
      print(paste0("$$ \\frac{m_A - m_B}{\\sqrt{ \\frac{S^2}{n_A} + \\frac{S^2}{n_B} }} $$"))
    )
  })
  
  output$two_mean_dist <- renderPlot({
    
    plot_2grp_means <- ggplot(data = data(), aes(x = groups, y = grp_values)) +
      geom_boxplot(aes(colour = groups))+
      geom_point(size = 1/input$sample, position = "jitter", aes(colour = groups)) +
      labs(title = "Fig 5. Means and Distribution of Two Group Means for Variable 1")+
      xlab("")+
      ylab("Group Values Observed")+
      theme(text = element_text(size = 12, family = "times"))+
      theme_minimal()
    
    plot_2grp_corr <- ggplot(data = data(), aes(x = var1, y = grp_values)) +
      geom_point(aes(colour = groups)) +
      geom_smooth(method = "lm", se = FALSE, aes(colour=groups))+
      geom_smooth(method = "lm", se = FALSE, color = "black")+
      labs(title = "Fig 6. Correlation between Var 1 & Var 2 (Grouped) and for Individual Groups")+
      xlab("Variable 1")+
      ylab("Variable 2")+
      theme(text = element_text(size = 12, family = "times"))+
      theme_minimal()
    
    plot_2grp_means / plot_2grp_corr + plot_layout(guides = 'collect')
  }) 
  
  
  output$two_mean_table <- function(){
    data <- data()
    
    data %>% 
      group_by(groups) %>% 
      dplyr::summarise('Sample Size (N)' = n(), 
                       'Group Means' = mean(grp_values),
                       'Group SDs' = sd(grp_values)) %>% 
      kable(digits = 2) %>%
      kable_styling("striped",
                    full_width = T, font_size = 12, html_font = 'Times') 
  }
  
  output$two_mean_test1 <- renderPlot({
    
    data <- data()
    two_mean_t <- t.test(data$grp_values ~ data$groups,
                         conf.level = 1-input$alpha)
    
    plot(two_mean_t)
  }
  )
  
  output$two_mean_t_form_denom <- renderUI({
    
    withMathJax(
      print(paste0("$$ {\\sqrt{ \\frac{S^2}{n_A} + \\frac{S^2}{n_B} }} $$"))
    )
  })
  
  output$twomean_reduc_sd_1 <- renderPlot({
    data <- data()
    datalist = list()
    
    for (i in seq(5,2000,5)) {
      
      sd1 <- sqrt((input$grp_sd1^2/i)+(input$grp_sd2^2/i))
      datalist[[i]] <- data.frame('SampleSize' = i, "SD" = sd1)
    }
    
    data_sd <- do.call(rbind, datalist)
    
    
    ggplot(data = data_sd, aes(x = SampleSize, y = SD)) +
      geom_line()+
      labs(title = "Fig 7. Standard deviation scaled by Sample Size for two groups with equal N", 
           caption = "Sample Size range: 5 to 2000 in intervals 5")+
      xlab("Sample Size")+
      ylab("Denominator: Two-Sample t-test")+
      theme(text = element_text(size = 12, family = "times"))+
      theme_minimal()
  })

  
  
####################################  
  
  # ANOVA 
  
#################################### 
  
  # Simulation sample for ANOVAs
  
  aov_one <- eventReactive(input$run4, {
    set.seed(1)
    
    dat_one <- ANOVA_design(
      design = "3b", 
      n = c(input$aov_sample),
      mu = c(input$aov1_m, input$aov2_m,input$aov3_m), 
      sd = c(input$aov1_sd,input$aov2_sd,input$aov3_sd),
      labelnames = c("Image_Type", "Nature", "City", "People"),
      plot = FALSE,
      r = .05
    )
    
    data.frame(dat_one$dataframe)
  }
  )
  
  aov_two <- eventReactive(input$run4.1, {
    set.seed(1)
    
    dat_two <- ANOVA_design(
      design = "3b*2b", 
      n = c(input$aov_sample),
      mu = c(input$aov1_m, input$aov4_m,
             input$aov2_m, input$aov5_m,
             input$aov3_m, input$aov6_m), 
      sd = c(input$aov1_sd,input$aov4_sd,
             input$aov2_sd,input$aov5_sd,
             input$aov3_sd,input$aov6_sd),
      labelnames = c("Image_Type", "Nature", "City", "People",
                     "Sex", "Male", "Female"),
      plot = FALSE,
      r = .05
    )
    
    data.frame(dat_two$dataframe)
  }
  )
  
  ##########################
  ### ANOVA
  ##########################
  
  output$aov_one_formula <- renderUI({
    fit<-aov(y ~ Image_Type, data = aov_one()) 
    eq <- paste0(extract_eq(fit))
    withMathJax(
      print(paste0("$$",eq,"$$"))
    )
  })
  
  
  output$aov_SST <- renderUI({
    withMathJax(
      print(paste0("$$ SS_{Total}= \\sum\\limits_{j=1}^{k}\\sum\\limits_{i=1}^{n_i} \\left(x_{ij} - \\overline{X}\\right)^2 $$"))
    )
  })
  
  
  output$aov_SSB <- renderUI({
    withMathJax(
      print(paste0("$$ SS_{Between} = \\sum\\limits_{j=1}^{k}n_j  \\left(\\overline{x_j} - \\overline{X}\\right)^2 $$"))
    )
  })
  
  output$aov_SSW <- renderUI({
    withMathJax(
      print(paste0("$$ SS_{Within} = \\sum\\limits_{j=1}^{k}\\sum\\limits_{i=1}^{n_j} \\left(x_{ij} - \\overline{x}_{j}\\right)^2 $$"))
    )
  })
  
  output$aov_MSB <- renderUI({
    withMathJax(
      print(paste0("$$ MS_{Between} = \\frac {SS_{Between}}{k - 1} $$"))
    )
  })
  
  output$aov_MSW <- renderUI({
    withMathJax(
      print(paste0("$$ MS_{Within} = \\frac {SS_{Within}}{n - k} $$")) # within / error
    )
  })
  
  output$aov_F_form <- renderUI({
    withMathJax(
      print(paste0("$$ F_{stat} =  \\frac {MS_{Between}}{MS_{Within}} $$"))
    )
  })
  
  output$aov_sim_one <- function() {
    aov_one() %>% 
      group_by(Image_Type) %>% 
      dplyr::summarise("Group N" = n(),
                       "Group Mean" = mean(y),
                       "Group SD" = sd(y)) %>% 
      kable(caption = "Table 1. By Factor (i.e. group) Sample Descriptives") %>% 
      kable_styling("striped",
                    full_width = F, font_size = 12, html_font = 'Times')
  }
  output$aov_sim_one_overal <- function() {
    aov_one() %>% 
      dplyr::summarise("Overall N" = n(),
                       "Overall Mean" = mean(y),
                       "Overall SD" = sd(y)) %>% 
      kable(caption = "Table 2. Overall Sample Descriptives") %>% 
      kable_styling("striped",
                    full_width = F, font_size = 12, html_font = 'Times')
  }
  
  output$aov_out_one <- function(){
    set.seed(1)
    anova <- aov(y ~ Image_Type, data = aov_one())
    
    broom::tidy(anova) %>% 
      kable(caption = "Table 3. One-way ANOVA Results", 
            col.names = c("Term", "DF", "Sum of Squares", "Mean Sum of Squares",
                          "F-statistics", "p-Value")) %>% 
      kable_styling("striped",
                    full_width = F, font_size = 12, html_font = 'Times')
  }
  
  output$aov_out_one_tukey <- function(){
    set.seed(1)
    anova <- aov(y ~ Image_Type, data = aov_one())
    
    TukeyHSD(anova, which = c("Image_Type")) %>% 
      broom::tidy() %>% 
      kable(caption = "Table 4. Tukey's HSD contrast results for Two-way ANOVA", 
            col.names = c("Term", "Contrast", "Null Val", "Estimate",
                          "Lower 95% CI", "Upper 95% CI", "Adjusted p-value")
      ) %>% 
      kable_styling("striped",
                    full_width = F, 
                    font_size = 12, 
                    html_font = 'Times')
  }
  
  
  
  output$aov_one_plot <- renderPlot({
    aov_one() %>% 
      ggplot(aes(x = y, y = Image_Type, colour = Image_Type)) +
      geom_boxplot()+
      labs(title = "Fig 1. Boxplot for groups means")+
      theme_minimal() +
      theme(axis.text.y=element_blank())
  })
  
  
  output$aov_SST_plot <- renderPlot({
    aov_one <- aov_one()
    
    ## Plot SST
    SST<- round(sum((aov_one$y - mean(aov_one$y))^2),2)
    aov_one %>%
      ggplot(aes(x = "", y = y, colour = Image_Type)) +
      geom_point(aes(x = "", y = y), position = "jitter",
                 size =5/input$aov_sample) +
      annotate("label", x = "", y = mean(aov_one$y)*2, 
               label = paste0("Sum Sq. Total: ",SST))+
      geom_hline(yintercept = mean(aov_one$y), linetype = "solid",
                 color = "red", size = .5)+
      labs(title = "Fig 2. Sum of Squares Total  (Residuals)",
           caption = "Red Line = Mean of Outcome")+
      theme_minimal()+
      theme(axis.text.y=element_blank())
    
  })
  
  
  output$aov_SSB_plot <- renderPlot({
    aov_one <- aov_one()
    
    ## Plot SSB
    SSB<- aov_one %>% 
      dplyr::select(y, Image_Type) %>% 
      mutate(grand_mean = mean(y)) %>% 
      group_by(Image_Type) %>% 
      dplyr::summarize(n = n(),
                       factor_mean = mean(y),
                       grand_mean = mean(grand_mean)) %>% 
      mutate(SS_between = round(n * (factor_mean - grand_mean)^2),2) %>% 
      pull(SS_between) %>% 
      sum()
    
    
    aov_one %>%
      ggplot(aes(x = Image_Type, y = y)) +
      geom_boxplot(aes(y = y, group = Image_Type, colour = Image_Type))+
      geom_point(aes(x = Image_Type, y = y, colour = Image_Type), size =5/input$aov_sample) +
      annotate("label", x = "SSB", 
               y = mean(aov_one$y)*1.2, 
               label = paste0("Sum Sq. Between: ",SSB))+
      geom_hline(yintercept = mean(aov_one$y), linetype = "solid",
                 color = "red", size = .5)+
      labs(title = "Fig 3. Sum of Squares Between (Factor)",
           caption = "Red Line = Mean of Outcome")+
      theme_minimal() + 
      theme(axis.text.x=element_blank())
    
  })
  
  output$aov_SSW_plot <- renderPlot({
    aov_one <- aov_one()
    
    ## Plot SST
    SSW <-
      aov_one %>% 
      group_by(Image_Type) %>% 
      dplyr::mutate(grp_mean = mean(y)) %>% 
      dplyr::mutate(SSW = (y - grp_mean)^2) %>%
      group_by() %>% 
      dplyr::summarise(SSW = sum(SSW)) %>% 
      round(1)
    
    
    aov_one %>% 
      ggplot(aes(x = Image_Type, y = y, colour = Image_Type)) +
      geom_boxplot(aes(x = Image_Type, y = y))+
      geom_point(aes(x =  Image_Type, y = y), position = "jitter", size =1) +
      annotate("label", x = "SSW", y = mean(aov_one$y),
               label = paste0("Sum Sq. Within: ",SSW))+
      labs(title = "Fig 4. Sum of Squares Within (Error)")+
      theme_minimal() +
      theme(axis.text.x=element_blank())
    
    
  }
  )
  
  
  output$aov_two_formula <- renderUI({
    fit<-aov(y ~ Image_Type + Sex, data = aov_two()) 
    eq <- paste0(extract_eq(fit))
    withMathJax(
      print(paste0("$$",eq,"$$"))
    )
  })
  
  output$aov_sim_two <- function() {
    aov_two() %>% 
      group_by(Image_Type, Sex) %>% 
      dplyr::summarise("Group N" = n(),
                       "Group Mean" = mean(y),
                       "Group SD" = sd(y)) %>% 
      kable(caption = "Table 5. By Factor Descriptives Two-way ANOVA") %>% 
      kable_styling("striped",
                    full_width = F, font_size = 12, html_font = 'Times')
  }
  
  output$aov_sim_two_overal <- function() {
    aov_two() %>% 
      dplyr::summarise("Overall N" = n(),
                       "Overall Mean" = mean(y),
                       "Overall SD" = sd(y)) %>% 
      kable(caption = "Table 6. Overall Descriptives Two-way ANOVA") %>% 
      kable_styling("striped",
                    full_width = F, font_size = 12, html_font = 'Times')
    
  }
  
  output$aov_out_two <- function(){
    anova <- aov(y ~ Image_Type + Sex + Image_Type:Sex, data = aov_two())
    
    broom::tidy(anova) %>% 
      kable(caption = "Table 7. Two-way ANOVA Results", 
            col.names = c("Term", "DF", "Sum of Squares", "Mean Sum of Squares",
                          "F-statistics", "p-value")) %>% 
      kable_styling("striped",
                    full_width = F, font_size = 12, html_font = 'Times')
  }
  
  output$aov_two_plot <- renderPlot({
    
    aov_two() %>% 
      ggplot(aes(x = y, y = Image_Type, colour = Sex)) +
      geom_boxplot()+
      labs(title = "Fig 5. Plot of Means Across 3x2 factors, Image Type and Sex")+
      scale_y_discrete(labels=c("People","Nature","City"))+
      theme_minimal() +
      theme(axis.text.x=element_blank(),
            axis.text=element_text(size = 10))
  })
  
  output$aov_out_two_tukey <- function(){
    set.seed(1)
    anova <- aov(y ~ Image_Type + Sex + Image_Type:Sex, data = aov_two())
    
    TukeyHSD(anova, which = c("Image_Type","Sex", "Image_Type:Sex")) %>% 
      broom::tidy() %>% 
      kable(caption = "Table 8. Tukey's HSD contrast results for Two-way ANOVA", 
            col.names = c("Term", "Contrast", "Null Vall", "Estimate",
                          "Lower 95% CI", "Upper 95% CI", "Adjusted p-value")) %>% 
      kable_styling("striped",
                    full_width = F, 
                    font_size = 12, 
                    html_font = 'Times')
  }
####################################    
  # Correlation 
####################################      

    ### Simulation sample with TWO variables reflective pre specified correlation
    sim_corr <- eventReactive(input$run2, {
        set.seed(123)
        n = input$corr_sample; 
        m1 = input$corr_m1; m2 = input$corr_m2; mu <- c(m1, m2)
        sd1 = input$corr_sd1; sd2 = input$corr_sd2; stddev <- c(sd1, sd2)
        r1 = input$corr1
        
        corMat <- matrix(c(1, r1, 
                           r1,1),
                         ncol = 2)
        covMat <- stddev %*% t(stddev) * corMat
      
        df <- as.data.frame(
          mvrnorm(n = n, 
                  mu = mu, 
                  Sigma = covMat, 
                  empirical = TRUE))
        
    })
  
    sim_corr_2 <- eventReactive(input$run2, {
        set.seed(1)
        data.frame(
            mvrnorm(n = input$corr_sample, 
                    mu = c(input$corr_m3, input$corr_m4), # mean specified by user
                    Sigma = matrix(c(1, input$corr2, input$corr2, 1), ## corr specified by user
                                   nrow = 2), 
                    empirical = TRUE))
        
        set.seed(123)
        n = input$corr_sample; 
        m3 = input$corr_m3; m4 = input$corr_m4; mu <- c(m3, m4)
        sd3 = input$corr_sd3; sd4 = input$corr_sd4; stddev <- c(sd3, sd4)
        r2 = input$corr2
        
        corMat <- matrix(c(1, r2, 
                           r2,1),
                         ncol = 2)
        covMat <- stddev %*% t(stddev) * corMat
        
        df <- as.data.frame(
          mvrnorm(n = n, 
                  mu = mu, 
                  Sigma = covMat, 
                  empirical = TRUE))
    })

    
    corr_grp_dat <- eventReactive(input$run2, {
        
        set.seed(123)
        n1 = 50; 
        m1 = 5; m2 = 7; mu1 <- c(m1, m2)
        sd1 = 2.2; sd2 = 2.5; stddev1 <- c(sd1, sd2)
        r1 = .4
        
        corMat1 <- matrix(c(1, r1, 
                           r1,1),
                         ncol = 2)
        
        covMat1 <- stddev1 %*% t(stddev1) * corMat1
        
        grp1 <- as.data.frame(
          mvrnorm(n = n1, 
                  mu = mu1, 
                  Sigma = covMat1, 
                  empirical = TRUE))
      
        
        grp1$group <- 1
        
        
        m3 = 6; m4 = 2.5; mu2 <- c(m3, m4)
        sd3 = 1.2; sd4 = .75; stddev2 <- c(sd3, sd4)
        r2 = .3
        
        corMat2 <- matrix(c(1, r2, 
                            r2,1),
                          ncol = 2)
        
        covMat2 <- stddev2 %*% t(stddev2) * corMat2
        
        grp2 <- as.data.frame(
          mvrnorm(n = n1, 
                  mu = mu2, 
                  Sigma = covMat2, 
                  empirical = TRUE))
        grp2$group <- 2
        
        m5 = 8; m6 = 4.5; mu3 <- c(m5, m6)
        sd5 = 1.; sd6 = .5; stddev3 <- c(sd5, sd6)
        r3 = -.15
        
        corMat3 <- matrix(c(1, r3, 
                            r3,1),
                          ncol = 2)
        
        covMat3 <- stddev3 %*% t(stddev3) * corMat3
        
        grp3 <- as.data.frame(
          mvrnorm(n = n1, 
                  mu = mu3, 
                  Sigma = covMat3, 
                  empirical = TRUE))
        grp3$group <- 3
        
        data <- data.frame(rbind(grp1, grp2, grp3))
        data$X1 <- data$V1
        data$X2 <- data$V2
        data
    })

    
    ### Formulas
    output$corr_formula_p <- renderUI({
      
      eq <- paste0("r = \\frac{\\sum{(x-m_x)(y-m_y)}}{\\sqrt{\\sum{(x-m_x)^2}\\sum{(y-m_y)^2}}}")
      withMathJax(
        print(paste0("$$",eq,"$$"))
      )
    })
    
    output$corr_formula_rho <- renderUI({
      
      eq2 <- paste0("rho = \\frac{\\sum(x' - m_{x'})(y'_i - m_{y'})}{\\sqrt{\\sum(x' - m_{x'})^2 \\sum(y' - m_{y'})^2}}")
      withMathJax(
        print(paste0("$$",eq2,"$$"))
      )
    })
    
    ### correlation tests
    output$corr_test <- function(){
      
      co_t1 <- cor.test(x = sim_corr()$V1, 
                        y = sim_corr()$V2,
                        method = "pearson")
      
      
      co_t2 <- cor.test(x = sim_corr_2()$V1, 
                        y = sim_corr_2()$V2,
                        method = "pearson")
      
      data.frame("Pearson r"= c(round(as.numeric(co_t1$estimate),2),
                                round(as.numeric(co_t2$estimate),2)), 
                 "p-value"=c(as.numeric(co_t1$p.value),
                             as.numeric(co_t2$p.value)),
                 row.names = c("Var 1 ~ Var 2", "Var 3 ~ Var 4")) %>% 
        kable() %>% 
        kable_styling("striped",
                      full_width = F, font_size = 12, html_font = 'Times') 
    }
    
    
    output$corr_test3 <- function(){
      co_t1 <- cor.test(x = sim_corr()$V1, 
                        y = sim_corr()$V2,
                        method = "spearman")
      
      data.frame("Spearman rho"= c(round(as.numeric(co_t1$estimate),2)), 
                 "p-value"=c(as.numeric(co_t1$p.value)),
                 row.names = c("Var 1 ~ Var 2")) %>% 
        kable() %>% 
        kable_styling("striped",
                      full_width = F, font_size = 12, html_font = 'Times')
    }
    
    ### Plots/Tables
    output$corr_plot1 <- renderPlot({
      
      cor1 <- round(
        as.numeric(
          cor.test(x = sim_corr()$V1, 
                   y = sim_corr()$V2,
                   method = "pearson")$estimate),2)
      cor2 <- round(
        as.numeric(
          cor.test(x = sim_corr_2()$V1, 
                   y = sim_corr_2()$V2,
                   method = "pearson")$estimate),2)
      
      corplt1 <- ggplot(sim_corr(), aes(x = V1, y = V2)) +
        geom_smooth(method = "lm", se = FALSE)+
        geom_point()+
        geom_rug(size = .3, position = "jitter", colour = "sienna4")+
        labs(title = "Fig 1. Association between Var 1 and Var 2", 
             caption = "*Data simulated based on specified parameters")+
        xlab("Var1")+
        ylab("Var2")+
        annotate("label", x = mean(sim_corr()$V1)+1, y = mean(sim_corr()$V2)+1, 
                 label = paste0("Pearson's r: ",cor1))+
        theme_minimal()
      
      corplt2 <- ggplot(sim_corr_2(), aes(x = V1, y = V2)) +
        geom_smooth(method = "lm", se = FALSE)+
        geom_point()+
        geom_rug(size = .3, position = "jitter", colour = "sienna4")+
        labs(title = "Fig 2. Association between Var 3 and Var 4", 
             caption = "*Data simulated based on specified parameters")+
        xlab("Var3")+
        ylab("Var4")+
        annotate("label", x = mean(sim_corr_2()$V1)+1, y = mean(sim_corr_2()$V2)+1, 
                 label = paste0("Pearson's r: ",cor2))+
        theme_minimal()
      
      corplt1 / corplt2 + plot_layout(guides = 'collect')
    })
    
    output$corr_n_p <- renderPlot({
      
      data_r_list = list()
      for (i in seq(5,700,5)) {
        
        t_val <- (.15*(sqrt(i-2)))/sqrt(1-(.15^2))
        p_val <- 2*pt(-abs(t_val), df = i-2)
        
        data_r_list[[i]] <- data.frame('SampleSize' = i, "p_value" = p_val)
      }
      
      data_r_p <- do.call(rbind, data_r_list)
      
      
      ggplot(data = data_r_p, aes(x = SampleSize, y = p_value)) +
        geom_line()+
        labs(title = "Fig 3. Effect of Sample size on p-value for Pearson Correlation of r = .15", 
             caption = "Sample Size: 5 to 700; Dashed Red Line, p = .05")+
        xlab("Sample Size")+
        ylab("p-value")+
        geom_hline(yintercept = .05, linetype = "dashed", colour = "red")+
        theme(text = element_text(size = 12, family = "times"))+
        theme_minimal()
      
    })
    
    output$corr_grp_plt <- renderPlot({
      
      plot1 <- corr_grp_dat() %>% 
        ggplot(aes(x = X1, y = X2)) +
        geom_point() +
        geom_smooth(method = "lm", formula = 'y ~ x', se = F) +
        labs(title = "Fig 4. Correlation between X1 and X2 overall in sample")+
        xlab("X 1")+
        ylab("X 2")+
        theme_minimal()
      
      plot2 <- corr_grp_dat() %>% 
        ggplot(aes(x = X1, y = X2, colour = as.factor(group))) +
        geom_point() +
        geom_smooth(method = "lm", formula = 'y ~ x', se = F, aes(colour = as.factor(group))) +
        labs(title = "Fig 5. Correlation between X1 and X2 across three groups in sample")+
        xlab("X 1")+
        ylab("X 2")+
        theme_minimal()
      plot1 / plot2 + plot_layout(guides = 'collect')
      
    })
    
    
    output$corr_grp_tbl <- function(){
      tab1 <- corr_grp_dat() %>% 
        group_by(group) %>% 
        dplyr::summarise(N = n(), r = round(cor(X1, X2),3), 
                         Mean_X1 = mean(X1), Mean_X2 = mean(X2)) %>% 
        data.frame()
      
      tab2 <- corr_grp_dat() %>% 
        dplyr::summarise(N = n(), r = round(cor(X1, X2),3),
                         Mean_X1 = mean(X1), Mean_X2 = mean(X2)) %>% 
        mutate(group = "Overall") %>% 
        data.frame()
      
      rbind(tab1, tab2) %>% 
        kable() %>% 
        kable_styling("striped",
                      full_width = F, font_size = 12, html_font = 'Times') 
    }
    
    
    
    
####################################    
# Stability Correlation 
####################################   
    # simulate data
    stable_DF <- eventReactive(input$run3, {
        
        set.seed(123)
        n = 1500; 
        m1 = input$stabcorr_m2; m2 = input$stabcorr_m2; mu <- c(m1, m2)
        sd1 = input$stabcorr_sd1; sd2 = input$stabcorr_sd2; stddev <- c(sd1, sd2)
        r1 = input$corr3
        
        corMat <- matrix(c(1, r1, 
                           r1,1),
                         ncol = 2)
        
        covMat <- stddev %*% t(stddev) * corMat
        
        df <- as.data.frame(
          mvrnorm(n = n, 
                  mu = mu, 
                  Sigma = covMat, 
                  empirical = TRUE))
        
        
        datalist = list()
        
        for (i in seq(5,1500,5)) { #input$samp_range[1],input$samp_range[2]
            Correl <- sample_n(df, i)
            corr_pear <- cor.test(Correl$V1, Correl$V2, method = "pearson")
            corr_spear <- cor.test(Correl$V1, Correl$V2, method = "spearman")
            #corr_kend <- cor.test(Correl$X1, Correl$X2, method = "kendall")
            
            datalist[[i]] <- data.frame(Pearson_r = corr_pear$estimate, 
                                        Pearson_p = corr_pear$p.value,
                                        Spearman_rho = corr_spear$estimate, 
                                        Searson_p = corr_spear$p.value,
                                        #endall_t = corr_kend$estimate, 
                                        #endall_p = corr_kend$p.value,
                                        i)
        }
        do.call(rbind, datalist)
    })


    ### Plot first   
    output$EffNchange <- renderPlot({
      
      stab1 <- stable_DF() %>% 
        ggplot(aes(x = i, y = Pearson_r)) +
        geom_line(aes(y = Pearson_r)) +
        xlab("Sample Size") +
        ylab("Pearson's r")+
        scale_y_continuous(
          name = "Pearson's r") +
        geom_vline(xintercept = 100, color = "Red", size = .4)+
        labs(title = "Fig 1. Pearson's r stability w/ N", caption = "Red line = Sample Size: 100")+
        theme(text = element_text(size = 30, family = "times"))+
        theme_minimal() 
      
      stab2 <- stable_DF() %>% 
        gather(key = "CorrelationType", 
               value = "Effect", 
               Pearson_r,Spearman_rho) %>% 
        ggplot(aes(x = i, y = Effect)) +
        geom_line(aes(colour = CorrelationType))+
        xlab("Sample Size") +
        ylab("Correlation Size") +
        labs(title = "Fig 2. Pearson v. Spearman similarity w/ N")+
        theme(text = element_text(size = 30, family = "times"))+
        theme_minimal() 
      
      stab1 / stab2 + plot_layout(guides = 'collect')
      
      
    })
    

    output$EffNchange2 <- renderPlot({
      
      df <- data.frame(
        mvrnorm(n = 6000, 
                mu = c(5, 8), 
                Sigma = matrix(c(1, .35, .35, 1), 
                               nrow = 2), 
                empirical = TRUE))
      
      set.seed(123)
      r_corr_loop <- function() {
        datalist = list()
        for (i in seq(5,1000,10)) { #input$samp_range[1],input$samp_range[2]
          
          Correl <- sample_n(df, i)
          corr_pear <- cor.test(Correl$X1, Correl$X2, method = "pearson")
          
          datalist[[i]] <- data.frame(Pearson_r = corr_pear$estimate,
                                      i)
        }
        return(datalist)
      }
      
      data <- do.call(rbind,replicate(30, r_corr_loop())) 
      
      plot1 <- data %>% 
        ggplot(aes(x = i, y = Pearson_r)) + 
        stat_summary(geom="ribbon", fun.data=mean_cl_boot, 
                     fill="black", alpha = .1)+
        stat_summary(geom="line", fun=mean, linetype="dashed")+
        xlab("Sample Size") +
        ylab("Correlation Size") +
        labs(title = "Fig 3. Repeated (n=30) Sampling for r = .35 from N = 6,000 data w/ 95 Conf Interval")+
        theme_minimal()
      plot2 <- data %>% 
        ggplot(aes(y = Pearson_r)) + 
        geom_histogram(bins = 80, fill = "blue", alpha = .8)+
        geom_hline(yintercept = .35, color = "red", size = .3)+
        xlab("Frequency") +
        ylab("Pearson's r value") +
        labs(title = "Fig 4. Density of r values", caption = "Red line indicate r = .35 (bin size 80)")+
        theme_minimal()
      
      plot1 / plot2 + plot_layout(guides = 'collect')
      
    })
    
    ### NHANES example 
    output$EffNchange3 <- renderPlot({
      library(foreign)
      df <- read.csv("./sub6000_nhanes.csv")
      
      #df <- sample_n(data, 6000) %>% 
      #    dplyr::select(ID, BPSysAve, BPDiaAve)
      
      
      set.seed(123)
      r_corr_loop2 <- function() {
        datalist = list()
        for (i in seq(5,1000,10)) { #input$samp_range[1],input$samp_range[2]
          
          Correl <- sample_n(df, i)
          corr_pear <- cor.test(Correl$BPSysAve, Correl$BPDiaAve, method = "pearson")
          
          datalist[[i]] <- data.frame(Pearson_r = corr_pear$estimate,
                                      i)
        }
        return(datalist)
      }
      
      do.call(rbind,replicate(30, r_corr_loop2())) %>% 
        ggplot(aes(x = i, y = Pearson_r)) + 
        stat_summary(geom="ribbon", fun.data=mean_cl_boot, 
                     fill="blue", alpha = .1)+
        stat_summary(geom="line", fun=mean, linetype="dashed")+
        xlab("Sample Size") +
        ylab("Correlation Size") +
        labs(title = "Fig 4. NHANES, Repeated (n=30) Sampling for BP Systolic & Diastolic ")+
        theme_minimal()
      
      
    })
    

    
################################################################   
    
# REGRESSION
    
################################################################ 
    
    ### simulate data
    m_reg_data <- eventReactive(input$run5, {
      set.seed(123)
      n = input$m_reg_sample; 
      m1 = input$m_reg_m1; m2 = input$m_reg_m2; m3 = input$m_reg_m3; mu <- c(m1, m2, m3)
      sd1 = input$m_reg_sd1; sd2 = input$m_reg_sd2; sd3 = input$m_reg_sd3; stddev <- c(sd1, sd2, sd3)
      r1 = input$m_reg_corr1; r2 =  input$m_reg_corr2; r3 = input$m_reg_corr3
      
      corMat <- matrix(c(1, r1, r2,
                         r1, 1, r3,
                         r2, r3, 1),
                       ncol = 3)
      covMat <- stddev %*% t(stddev) * corMat
      
      dat1 <- as.data.frame(
        mvrnorm(n = n, 
                mu = mu, 
                Sigma = covMat, 
                empirical = TRUE)
      )
      if (input$outlier=="None") {
        m_reg_df <-
          data.frame(
            mvrnorm(n = n, 
                    mu = c(m1, m2, m3),
                    Sigma = covMat, 
                    empirical = TRUE))
        
            m_reg_df<- rename(m_reg_df, 
                              c("X1"="Drinking", "X2" ="Reward_Seek","X3"="Age"))
            m_reg_df
            
        } else if (input$outlier=="One") {
          m_reg_df <-
            data.frame(
              mvrnorm(n = n, 
                      mu = c(m1, m2, m3),
                      Sigma = covMat, 
                      empirical = TRUE))
          
            m_reg_df<- rename(m_reg_df, 
                              c("X1"="Drinking", "X2" ="Reward_Seek","X3"="Age"))
            rand_row = as.integer(runif(1,min = 1, max = length(m_reg_df$Drinking)))
            rand_col = as.integer(runif(1, min = 1, max = 2))
            m_reg_df[rand_row[1],rand_col[1]]<- m_reg_df[rand_row[1],rand_col[1]] + (5*sd(m_reg_df[,rand_col[1]]))
            m_reg_df
            
        } else if (input$outlier=="Two") {
          m_reg_df <-
            data.frame(
              mvrnorm(n = n, 
                      mu = c(m1, m2, m3),
                      Sigma = covMat, 
                      empirical = TRUE))
          
            m_reg_df<- rename(m_reg_df, 
                              c("X1"="Drinking", "X2" ="Reward_Seek","X3"="Age"))
            rand_row = as.integer(runif(2,min = 1, max = length(m_reg_df$Drinking)))
            rand_col = as.integer(runif(2, min = 1, max = 2))
            m_reg_df[rand_row[1],rand_col[1]]<- m_reg_df[rand_row[1],rand_col[1]] + (-5*sd(m_reg_df[,rand_col[1]]))
            m_reg_df[rand_row[2],rand_col[2]]<- m_reg_df[rand_row[2],rand_col[2]] + (5*sd(m_reg_df[,rand_col[2]]))
            m_reg_df
        } else  {
            m_reg_df <-
                data.frame(
                    mvrnorm(n = input$m_reg_sample, 
                            mu = c(input$m_reg_m1,input$m_reg_m2,input$m_reg_m3), 
                            Sigma = matrix(c(1, input$m_reg_corr1, input$m_reg_corr2,
                                             input$m_reg_corr1,1,input$m_reg_corr3,
                                             input$m_reg_corr2,input$m_reg_corr3,1), 
                                           nrow = 3), 
                            empirical = TRUE))
            
            m_reg_df<- rename(m_reg_df, 
                              c("X1"="Drinking", "X2" ="Reward_Seek","X3"="Age"))

            rand_row = as.integer(runif(3,min = 1, max = length(m_reg_df$Drinking)))
            rand_col = as.integer(runif(3, min = 1, max = 2))
            m_reg_df[rand_row[1],rand_col[1]]<- m_reg_df[rand_row[1],rand_col[1]] + (5*sd(m_reg_df[,rand_col[1]]))
            m_reg_df[rand_row[2],rand_col[2]]<- m_reg_df[rand_row[2],rand_col[2]] + (-5*sd(m_reg_df[,rand_col[2]]))
            m_reg_df[rand_row[3],rand_col[3]]<- m_reg_df[rand_row[3],rand_col[3]] + (5*sd(m_reg_df[,rand_col[3]]))
            m_reg_df
        }
    })
    
    

    ### Linear Regression
    output$m_reg_formula_1 <- renderUI({
      
      eq <- eq <- paste0("Drinking = {\\beta}0 + {\\beta}1_{Reward}+ {\\epsilon}")
      withMathJax(
        print(paste0("$$",eq,"$$"))
      )
    })
    
    output$m_reg_calc_b0 <- renderUI({
      
      eq <- paste0("\\hat{\\beta}0 = {\\overline{y} - {\\hat{\\beta}1}{\\overline{x}}}")
      withMathJax(
        print(paste0("$$",eq,"$$"))
      )
    })
    
    output$m_reg_calc_b1 <- renderUI({
      
      eq <- paste0("\\hat{\\beta}1 = \\frac{\\sum{(x_i - \\overline{x})(y_i-\\overline{x})}}{\\sum{(x_i-\\overline{x})^2}}")
      withMathJax(
        print(paste0("$$",eq,"$$"))
      )
    })
    
    output$r_sqrd <- renderUI({
      
      eq <- paste0("R^2 = {1-}\\frac{SS_{Residual}}{SS_{Total}}")
      withMathJax(
        print(paste0("$$",eq,"$$"))
      )
    })
    
    output$adjst_r_sqrd <- renderUI({
      
      eq <- paste0("Adjusted R^2 = {1}-\\frac{(1-R^2)(N-1)}{N-p-1}")
      withMathJax(
        print(paste0("$$",eq,"$$"))
      )
    })
    
    output$m_reg_1_descr <- function(){
      m_reg_data() %>% 
      dplyr::summarize("N" = n(), "Drinking Mean" = mean(Drinking), "Drinking SD" = sd(Drinking), 
                "Reward Seek Mean" = mean(Reward_Seek), "Reward Seek SD" = sd(Reward_Seek)) %>% 
        kable(caption = "Table 1. Descriptive Statistics for Simulated Data") %>% 
        kable_styling("striped",
                      full_width = T, font_size = 12, html_font = 'Times')
    }
    
    output$m_reg_1_coef <- function(){
      fit<-lm(Drinking ~ Reward_Seek, m_reg_data())

      summary(fit)$coefficients %>% 
        kable(caption = "Table 2. Linear Regression Coefficients", 
              col.names = c("Estimate", "Std. Error", "T-Statistics", "p-value")) %>% 
        kable_styling("striped",
                      full_width = T, font_size = 12, html_font = 'Times')
      
    }
    
    output$m_reg_1_model <- function(){
      fit<-lm(Drinking ~ Reward_Seek, m_reg_data())
      r1 <- round(summary(fit)$r.squared,3)
      r2 <- round(summary(fit)$adj.r.squared,3)
      f_stat <- round(as.numeric(summary(fit)$fstatistic[1]),3)
      df1 <- as.numeric(summary(fit)$fstatistic[2])
      df2 <- as.numeric(summary(fit)$fstatistic[3])
      model_p <- 1-pf(q = f_stat, df1 = df1, df2 = df2)
      
      
      data.frame("F-statistic" = f_stat, "df1"= df1, "df2" =df2,
               "p-value"= model_p,"R-squared"= r1,"R-adjust"= r2) %>% 
        kable(caption = "Table 3. Linear Regression Model Fit",
              col.names = c("F-statistic", "df1", "df2", "p-value", "R squared", "adjusted R squared")) %>% 
        kable_styling("striped",
                      full_width = T, font_size = 12, html_font = 'Times')
      
    }
    
    output$m_reg_1_corr <- function(){
      fit<-lm(Drinking ~ Reward_Seek, m_reg_data())
      cor(m_reg_data()$Drinking,m_reg_data()$Reward_Seek) %>%  
        kable(caption = "Table 4. Correlation Between Drinking (DV) and Reward Seeking (IV)",
              col.names = "Pearson's r") %>% 
        kable_styling("striped",
                      full_width = T, font_size = 12, html_font = 'Times')
    }
    
    
    

    ### Multiple Regression
    output$mult_corr <- renderUI({
      
      eq <- paste0("r = \\frac{\\sum{(x-\\overline{x})(y-\\overline{y})}}{\\sqrt{\\sum{(x-\\overline{x})^2}\\sum{(y-\\overline{y})^2}}}")
      withMathJax(
        print(paste0("$$",eq,"$$"))
      )
    })
    
    output$mult_part_corr <- renderUI({
      
      eq <- paste0("r_{yx.z} = \\frac{r_{yx}-(r_{yz})(r_{xz})}{\\sqrt{1-r_{yz}^2}\\sqrt{1-r_{xz}^2}}")
      withMathJax(
        print(paste0("$$",eq,"$$"))
      )
    })

    output$m_reg_2_descr <- function(){
      m_reg_data() %>% 
        dplyr::summarize("N" = n(), "Drinking Mean" = mean(Drinking), "Drinking SD" = sd(Drinking), 
                         "Reward Seek Mean" = mean(Reward_Seek), "Reward Seek SD" = sd(Reward_Seek),
                         "Age Mean" = mean(Age), "Age SD" = sd(Age)) %>% 
        kable(caption = "Table 4. Descriptive Statistics for Simulated Data") %>% 
        kable_styling("striped",
                      full_width = T, font_size = 12, html_font = 'Times')
    }
    
    
    output$m_reg_2_coef <- function(){
      fit<-lm(Drinking ~ Reward_Seek + Age, m_reg_data())
      
      summary(fit)$coefficients %>% 
        kable(caption = "Table 5. Multiple Regression Coefficients", 
              col.names = c("Estimate", "Std. Error", "T-Statistics", "p-value")) %>% 
        kable_styling("striped",
                      full_width = T, font_size = 12, html_font = 'Times')
      
    }
    
    output$m_reg_2_model <- function(){
      fit<-lm(Drinking ~ Reward_Seek + Age, m_reg_data())
      r1 <- round(summary(fit)$r.squared,3)
      r2 <- round(summary(fit)$adj.r.squared,3)
      f_stat <- round(as.numeric(summary(fit)$fstatistic[1]),3)
      df1 <- as.numeric(summary(fit)$fstatistic[2])
      df2 <- as.numeric(summary(fit)$fstatistic[3])
      model_p <- 1-pf(q = f_stat, df1 = df1, df2 = df2)
      
      
      data.frame("F-statistic" = f_stat, "df1"= df1, "df2" =df2,
                 "p-value"= model_p,"R-squared"= r1,"R-adjust"= r2) %>% 
        kable(caption = "Table 6. Multiple Regression Model Fit",
              col.names = c("F-statistic", "df1", "df2", "p-value", "R squared", "adjusted R squared")) %>% 
        kable_styling("striped",
                      full_width = T, font_size = 12, html_font = 'Times')
      
    }
    
    output$m_reg_2_std_beta <- function(){
      fit<-lm(Drinking ~ Reward_Seek + Age, m_reg_data())
      
      round(lm.beta(fit),2) %>% 
        kable(caption = "Table 7. Multiple Regression Standardized Coefficients (lm.beta in R)",
              #col.names = c("Reward Seek", "Age")
              ) %>% 
        kable_styling("striped",
                      full_width = T, font_size = 12, html_font = 'Times')
      
    }
    
    output$m_reg_2_semi_par <- function(){
      p_cor <- as.data.frame(pcor(m_reg_data(), "pearson")[1])
      round(p_cor,2) %>% 
        kable(caption = "Table 8. Partial Pearson Correlation for Regression Variables (pcor in R)",
              col.names = c("Drinking", "Reward_Seek", "Age")
              ) %>% 
        kable_styling("striped",
                      full_width = T, font_size = 12, html_font = 'Times')
      
    }
    
    output$m_reg_formula_2 <- renderUI({
       
      eq <- eq <- paste0("Drinking = {\\beta}0 + {\\beta}1_{Reward}+ {\\beta}2_{Age} + {\\epsilon}")
      withMathJax(
        print(paste0("$$",eq,"$$"))
      )
    })
    
    
    ###     Interactive plot    
    output$m_reg_plot_1 <- renderPlot({
      DV_mean = mean(m_reg_data()$Drinking)
      ggplot(m_reg_data(), aes(x = Reward_Seek, y = Drinking)) + 
        geom_point()+
        geom_smooth(method="lm", se = F)+
        xlab("Reward Seeking") +
        ylab("Drrinking")+
        geom_hline(yintercept = DV_mean, color = "red", linetype = 'dotted')+
        labs(title = "Fig 1. Data Plot of Regression Model", caption = "Red line is mean for DV, Drinking")+
        theme(text = element_text(size = 12, family = "times"))+
        theme_minimal()
      

    })
    
    output$m_reg_plot_2 <- renderPlot({
      
      fit <- lm(Drinking ~ Reward_Seek, m_reg_data())
      
      m_reg_data_2 <- m_reg_data() %>% 
        mutate(
          predict_vals = fit$fitted.values,
          resid_vals = fit$residuals
        )
      
      plot1<- ggplot(m_reg_data_2, aes(x = Reward_Seek, y = predict_vals)) +
        geom_point()+
        geom_smooth(method="lm")+
        labs(title = "Fig 2. Fit Test: OLS Predicted Values ~ Reward", 
             caption = "*Output from lm(Drinking ~ Reward, data = simulated)")+
        theme(text = element_text(size = 12, family = "times"))+
        theme_minimal()
      
      plot2<- ggplot(m_reg_data_2, aes(x = Reward_Seek, y = predict_vals)) +
        geom_line(size = 1)+
        geom_point(aes(x = Reward_Seek, y = Drinking), size =1/input$m_reg_sample) +
        geom_segment(
          aes(xend = Reward_Seek, yend = Drinking),
          size = .2, alpha = .2, lineend = "round")+
        labs(title = "Fig 3. Fit Test: OLS Predicted Values ~ Observed Drinking", 
             caption = "Fitted values out of lm()$fitted.values & observed simulated")+
        theme(text = element_text(size = 12, family = "times"))+
        theme_minimal()
      
      
      plot3<- ggplot(m_reg_data_2, aes(x = Reward_Seek, y = resid_vals)) +
        geom_point(size =1/input$m_reg_sample) +
        geom_smooth(method="loess")+
        labs(title = "Fig 4. Association between Residuals ~ Reward", 
             caption = "Residuals from lm()$residuals & observed simulated")+
        theme(text = element_text(size = 12, family = "times"))+
        theme_minimal()
      
      plot4<- ggplot(m_reg_data_2, aes(x = Drinking, y = resid_vals)) +
        geom_point(size =1) +
        geom_smooth(method="loess")+
        labs(title = "Fig 5. Association between Residuals ~ Drinking", 
             caption = "Residuals from lm()$residuals & observed simulated")+
        theme(text = element_text(size = 12, family = "times"))+
        theme_minimal()
      
      (plot1 | plot2) / (plot3 | plot4)
    })
    
    
    output$m_reg_plot_3 <- renderPlot({
      
      fit2 <- lm(Drinking ~ Reward_Seek + Age, m_reg_data())
      
      m_reg_data_2 <- m_reg_data() %>% 
        mutate(
          predict_vals = fit2$fitted.values,
          resid_vals = fit2$residuals
        )
      
      plot5<- ggplot(m_reg_data_2, aes(x = Reward_Seek, y = predict_vals)) +
        geom_point()+
        geom_smooth(method="lm")+
        labs(title = "Fig 6. Fit Test: Fitted Drinking ~ Reward", 
             caption = "*Output from lm(Drinking ~ Reward + Age, data = simulated)")+
        theme(text = element_text(size = 12, family = "times"))+
        theme_minimal()
      
      plot6<- ggplot(m_reg_data_2, aes(x = Reward_Seek, y = predict_vals)) +
        geom_line(size = 1)+
        geom_point(aes(x = Reward_Seek, y = Drinking), size =1/input$m_reg_sample) +
        geom_segment(
          aes(xend = Reward_Seek, yend = Drinking),
          size = .2, alpha = .2, lineend = "round")+
        labs(title = "Fig 7. Fit Test: Fitted Drinking ~ Observed Drinking", 
             caption = "Fitted values out of lm()$fitted.values & observed simulated")+
        theme(text = element_text(size = 12, family = "times"))+
        theme_minimal()
      
      
      plot7<- ggplot(m_reg_data_2, aes(x = Reward_Seek, y = resid_vals)) +
        geom_point(size =1/input$m_reg_sample) +
        geom_smooth(method="loess")+
        labs(title = "Fig 8. Association between Residuals ~ Reward", 
             caption = "Residuals from lm()$residuals & observed simulated")+
        theme(text = element_text(size = 12, family = "times"))+
        theme_minimal()
      
      plot8<- ggplot(m_reg_data_2, aes(x = Drinking, y = resid_vals)) +
        geom_point(size =1) +
        geom_smooth(method="loess")+
        labs(title = "Fig 9. Association between Residuals ~ Drinking", 
             caption = "Residuals from lm()$residuals & observed simulated")+
        theme(text = element_text(size = 12, family = "times"))+
        theme_minimal()
      
      (plot5 | plot6) / (plot7 | plot8)
    })
    

    
################################################################      
    
# Construct Validation 
    
############################################################### 
    

   
     
################################################################      

# CFA/EFA  

################################################################      
    
    ### Simulation sample covariance for specified loadings
    
    sim_cfa_text <- renderText({
        input$sim_fact_coeff
    })
    
    output$sim_model <- renderText({
        sim_cfa_text()
    })
    
    sem_data <- eventReactive(input$run6, {
        set.seed(1) 
        model_population <-paste0(sim_cfa_text())
        
        sim <- sim(nRep = input$sem_reps, model = "lavaan", n = input$sem_sample, 
                generate = model_population, std.lv = TRUE, lavaanfun = "sem", 
                dataOnly=T, meanstructure = TRUE, seed=123)
        
        sim_data <- data.frame(aaply(laply(sim, as.matrix), c(2,3), mean))
    }
    )
    
    
    
##########################
### CFA/PCA
##########################  
    cfa_text <- renderText({
        input$cfa_model
    })
    
    output$cfa_spec_model <- renderText({
        cfa_text()
    })
    
    output$sem_corr <- function(){
    
    cor <- cor(sem_data(), method = "pearson")
    
    cor %>% 
        kable() %>% 
        kable_styling("striped",
                      full_width = T, font_size = 12, html_font = 'Times')
    }
    
    
    cfa_data <- eventReactive(input$run6.1, {
        set.seed(1) 

        model_cfa <-paste0(cfa_text())
        
        cfa(model = model_cfa,
            data = sem_data())
    })
    
    output$sem_table <- renderTable({
        set.seed(1) 
        
        parameters(cfa_data(), standardize = print(input$standardize))

    }
    )

    
    output$sem_plot <- renderPlot({
        set.seed(100) 
        
        semPlot::semPaths(object = cfa_data(),
                 style = "lisrel", whatLabels = "std",intercepts = TRUE,residuals = TRUE,
                 font = 14, edge.label.font = 12, edge.label.cex = 1.2, edge.color = "black",
                 layout = "tree")
    }
    )


}

################################ 
# run app
shinyApp(ui = UserInferface, 
         server = local_server)
