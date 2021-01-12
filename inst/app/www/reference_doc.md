
## Notes

The majority of data used in this app are derived from the American
Community Survey (ACS). These data are survey-based estimates and are
subject to error. The errors derive from research design (including
instrument bias, data frame, and sampling), the survey data collection
(non-response bias and response errors), and processing by the Census
Bureau (data coding, compilation processes, and case weighting), as well
as statistical inference error and uncertainty (which are related to
sample size and variance within the measured attributes). US Census
Bureau’s report on “Accuracy of the Data” can be found
[here](https://www2.census.gov/programs-surveys/acs/tech_docs/accuracy/MultiyearACSAccuracyofData2017.pdf?#).

Further, producing the “weighted averages” necessarily assumed equal
distributions of populations across individual census block groups or
tracts in order. Population characteristics are likely to be
heterogeneous, rather than homogeneous, across areas. Thus, observed
population characteristics within a given buffer distance may not
reflect the weighted averages presented here, but that does not diminish
the value of the weighted averages as a starting point for conversations
and actions to increase equitable usage of parks and trails across the
Metro Region.

ACS statistics were used to calculate the percent of people with a
certain characteristic within each block group or tract. We scaled these
percentages by the 2019 small area population estimates to obtain a more
accurate number of people within each group (this more accurate measure
of population is not available in collar counties). Weighted averages
were produced by multiplying by the percent overlap of a given block
group or tract with the buffer zone around individual park and trail
units.

### ACS Variable dictionary

Specific variables from the American Community Survey which can be
evaluated in this tool include:

**Age:** The age classification is based on the age of the person in
complete years at the time of interview. People are not to round an age
up if the person is close to having a birthday. They can estimate an age
if the exact age is not known. An additional instruction on babies also
asks respondents to print “0” for babies less than one year old.

**Race:** The racial categories included in the census questionnaire
generally reflect a social definition of race recognized in this country
and not an attempt to define race biologically, anthropologically, or
genetically. In addition, it is recognized that the categories of the
race item include racial and national origin or sociocultural groups.
People may choose to report more than one race to indicate their racial
mixture, such as “American Indian” and “White.” People who identify
their origin as Hispanic, Latino, or Spanish may be of any race.

**Ethnicity:** The terms “Hispanic,” “Latino,” and “Spanish” are used
interchangeably. Some respondents identify with all three terms while
others may identify with only one of these three specific terms.
Hispanics or Latinos who identify with the terms “Hispanic,” “Latino,”
or “Spanish” are those who classify themselves in one or more of the
specific Hispanic, Latino, or Spanish categories listed on the
questionnaire (“Mexican,” “Puerto Rican,” or “Cuban”) as well as those
who indicate that they are “another Hispanic, Latino, or Spanish
origin.” People who do not identify with any of the specific origins
listed on the questionnaire but indicate that they are “another
Hispanic, Latino, or Spanish origin” are those who identify as
Argentinian, Colombian, Dominican, Nicaraguan, Salvadoran, Spaniard, or
other Spanish cultures or origins.

**Income:** Asked of the population 15 years old and over. “Total
income” is the sum of the amounts reported separately for wage or
salary income; net self-employment income; interest, dividends, or net
rental or royalty income or income from estates and trusts; Social
Security or Railroad Retirement income; Supplemental Security Income
(SSI); public assistance or welfare payments; retirement, survivor, or
disability pensions; and all other income.

**Transportation:** These data show the number of passenger cars, vans,
and pickup or panel trucks of one-ton (2,000 pounds) capacity or less
kept at home and available for the use of household members. Vehicles
rented or leased for one month or more, company vehicles, and police and
government vehicles are included if kept at home and used for
non-business purposes. Motorcycles or other recreational vehicles are
excluded. Dismantled or immobile vehicles are excluded. Vehicles kept at
home but used only for business purposes also are excluded.

**Language:** Respondents who reported speaking a language other than
English were asked to indicate their English-speaking ability based on
one of the following categories: “Very well,” “Well,” “Not well,” or
“Not at all.” Those who answered “Well,” “Not well,” or “Not at all”
are sometimes referred to as “Less than ‘very well.’” Respondents were
not instructed on how to interpret the response categories in this
question.

**Ability:** Disability status is determined from the answers from six
types of difficulty. For children under 5 years old, hearing and vision
difficulty are used to determine disability status. For children between
the ages of 5 and 14, disability status is determined from hearing,
vision, cognitive, ambulatory, and self-care difficulties. For people
aged 15 years 63 and older, they are considered to have a disability if
they have difficulty with any one of the six difficulty types (hearing,
vision, cognitive, ambulatory, self-care, independent living
difficulty).

**Place of birth:** Respondents were asked to select one of two
categories: (1) in the United States, or (2) outside the United States.

### Contributors

  - Ellen Esch, Data Scientist
    [ellen.esch@metc.state.mnu.us](mailto:ellen.esch@metc.state.mn.us)
  - Liz Roten, Associate Data Scientist <liz.roten@metc.state.mn.us>  
  - Darcie Vandegrift, Principal Parks Researcher
    <darcie.vandegrift@metc.state.mn.us>

*Regional Parks and the American Community Survey* is an open-source
project. All the code for this project available on our
[GitHub](https://github.com/Metropolitan-Council/regionalparks.acs).

### References

Bibliography includes both data sources and R packages used in app
development.

<div id="refs" class="references">

<div id="ref-config">

<p>

Allaire, JJ. 2018. <em>Config: Manage Environment Specific Configuration
Values</em>.
<a href="https://CRAN.R-project.org/package=config" class="uri">https://CRAN.R-project.org/package=config</a>.

</p>

</div>

<div id="ref-noauthor_american">

<p>

“American Community Survey 5-Year Summary File - Minnesota Geospatial
Commons.” 2019.
<a href="https://gisdata.mn.gov/dataset/us-mn-state-metc-society-census-acs" class="uri">https://gisdata.mn.gov/dataset/us-mn-state-metc-society-census-acs</a>.

</p>

</div>

<div id="ref-noauthor_american-1">

<p>

“American Community Survey Multiyear Accuracy of the Data (5-Year
2014-2018).” 2019, 29.
<a href="https://www2.census.gov/programs-surveys/acs/tech_docs/accuracy/ACS_Accuracy_of_Data_2018.pdf?" class="uri"><https://www2.census.gov/programs-surveys/acs/tech_docs/accuracy/ACS_Accuracy_of_Data_2018.pdf>?</a>

</p>

</div>

<div id="ref-magrittr">

<p>

Bache, Stefan Milton, and Hadley Wickham. 2014. <em>Magrittr: A
Forward-Pipe Operator for R</em>.
<a href="https://CRAN.R-project.org/package=magrittr" class="uri">https://CRAN.R-project.org/package=magrittr</a>.

</p>

</div>

<div id="ref-shiny">

<p>

Chang, Winston, Joe Cheng, JJ Allaire, Yihui Xie, and Jonathan
McPherson. 2020. <em>Shiny: Web Application Framework for R</em>.
<a href="https://CRAN.R-project.org/package=shiny" class="uri">https://CRAN.R-project.org/package=shiny</a>.

</p>

</div>

<div id="ref-leaflet">

<p>

Cheng, Joe, Bhaskar Karambelkar, and Yihui Xie. 2019. <em>Leaflet:
Create Interactive Web Maps with the Javascript ’Leaflet’ Library</em>.
<a href="https://CRAN.R-project.org/package=leaflet" class="uri">https://CRAN.R-project.org/package=leaflet</a>.

</p>

</div>

<div id="ref-processx">

<p>

Csárdi, Gábor, and Winston Chang. 2020. <em>Processx: Execute and
Control System Processes</em>.
<a href="https://CRAN.R-project.org/package=processx" class="uri">https://CRAN.R-project.org/package=processx</a>.

</p>

</div>

<div id="ref-data.table">

<p>

Dowle, Matt, and Arun Srinivasan. 2019. <em>Data.table: Extension of
‘Data.frame‘</em>.
<a href="https://CRAN.R-project.org/package=data.table" class="uri">https://CRAN.R-project.org/package=data.table</a>.

</p>

</div>

<div id="ref-attempt">

<p>

Fay, Colin. 2020. <em>Attempt: Tools for Defensive Programming</em>.
<a href="https://CRAN.R-project.org/package=attempt" class="uri">https://CRAN.R-project.org/package=attempt</a>.

</p>

</div>

<div id="ref-janitor">

<p>

Firke, Sam. 2020. <em>Janitor: Simple Tools for Examining and Cleaning
Dirty Data</em>.
<a href="https://CRAN.R-project.org/package=janitor" class="uri">https://CRAN.R-project.org/package=janitor</a>.

</p>

</div>

<div id="ref-golem">

<p>

Guyader, Vincent, Colin Fay, Sébastien Rochette, and Cervan Girard.
2020. <em>Golem: A Framework for Robust Shiny Applications</em>.
<a href="https://CRAN.R-project.org/package=golem" class="uri">https://CRAN.R-project.org/package=golem</a>.

</p>

</div>

<div id="ref-glue">

<p>

Hester, Jim. 2020. <em>Glue: Interpreted String Literals</em>.
<a href="https://CRAN.R-project.org/package=glue" class="uri">https://CRAN.R-project.org/package=glue</a>.

</p>

</div>

<div id="ref-fs">

<p>

Hester, Jim, and Hadley Wickham. 2020. <em>Fs: Cross-Platform File
System Operations Based on ’Libuv’</em>.
<a href="https://CRAN.R-project.org/package=fs" class="uri">https://CRAN.R-project.org/package=fs</a>.

</p>

</div>

<div id="ref-leaflet.extras">

<p>

Karambelkar, Bhaskar, and Barret Schloerke. 2018. <em>Leaflet.extras:
Extra Functionality for ’Leaflet’ Package</em>.
<a href="https://CRAN.R-project.org/package=leaflet.extras" class="uri">https://CRAN.R-project.org/package=leaflet.extras</a>.

</p>

</div>

<div id="ref-noauthor_regional">

<p>

“Regional Parks - Minnesota Geospatial Commons.” 2018.
<a href="https://gisdata.mn.gov/dataset/us-mn-state-metc-plan-parks-regional" class="uri">https://gisdata.mn.gov/dataset/us-mn-state-metc-plan-parks-regional</a>.

</p>

</div>

<div id="ref-noauthor_regional-1">

<p>

“Regional Trails - Minnesota Geospatial Commons.” 2018.
<a href="https://gisdata.mn.gov/dataset/us-mn-state-metc-trans-regional-trails-exst-plan" class="uri">https://gisdata.mn.gov/dataset/us-mn-state-metc-trans-regional-trails-exst-plan</a>.

</p>

</div>

<div id="ref-councilR">

<p>

Roten, Liz. 2020. <em>CouncilR: Functions and Templates for the
Metropolitan Council</em>.
<a href="https://github.com/Metropolitan-Council/councilR" class="uri">https://github.com/Metropolitan-Council/councilR</a>.

</p>

</div>

<div id="ref-htmltools">

<p>

RStudio, and Inc. 2019. <em>Htmltools: Tools for Html</em>.
<a href="https://CRAN.R-project.org/package=htmltools" class="uri">https://CRAN.R-project.org/package=htmltools</a>.

</p>

</div>

<div id="ref-tigris">

<p>

Walker, Kyle. 2020. <em>Tigris: Load Census Tiger/Line Shapefiles</em>.
<a href="https://CRAN.R-project.org/package=tigris" class="uri">https://CRAN.R-project.org/package=tigris</a>.

</p>

</div>

<div id="ref-testthat">

<p>

Wickham, Hadley. 2011. “Testthat: Get Started with Testing.” <em>The R
Journal</em> 3: 5–10.
<a href="https://journal.r-project.org/archive/2011-1/RJournal_2011-1_Wickham.pdf" class="uri">https://journal.r-project.org/archive/2011-1/RJournal\_2011-1\_Wickham.pdf</a>.

</p>

</div>

<div id="ref-readxl">

<p>

Wickham, Hadley, and Jennifer Bryan. 2019. <em>Readxl: Read Excel
Files</em>.
<a href="https://CRAN.R-project.org/package=readxl" class="uri">https://CRAN.R-project.org/package=readxl</a>.

</p>

</div>

<div id="ref-dplyr">

<p>

Wickham, Hadley, Romain François, Lionel Henry, and Kirill Müller. 2020.
<em>Dplyr: A Grammar of Data Manipulation</em>.
<a href="https://CRAN.R-project.org/package=dplyr" class="uri">https://CRAN.R-project.org/package=dplyr</a>.

</p>

</div>

<div id="ref-pkgload">

<p>

Wickham, Hadley, Jim Hester, and Winston Chang. 2020. <em>Pkgload:
Simulate Package Installation and Attach</em>.
<a href="https://CRAN.R-project.org/package=pkgload" class="uri">https://CRAN.R-project.org/package=pkgload</a>.

</p>

</div>

</div>

<right style="font-size: 1rem; text-align: right; display: block;">
*Last updated 2021-01-11*  
Build ID: 2021-01-11.roten.407a692  
</right>
