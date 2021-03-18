
## Notes

The majority of data used in this app are derived from the American
Community Survey (ACS). Complete
<a href="https://www2.census.gov/programs-surveys/acs/tech_docs/subject_definitions/2019_ACSSubjectDefinitions.pdf" target="_blank">variable
definitions</a> can provide more information about each response. These
data are survey-based estimates and are subject to error. The errors
derive from research design (including instrument bias, data frame, and
sampling), the survey data collection (non-response bias and response
errors), and processing by the Census Bureau (data coding, compilation
processes, and case weighting), as well as statistical inference error
and uncertainty (which are related to sample size and variance within
the measured attributes). US Census Bureau’s report on
<a href="https://www2.census.gov/programs-surveys/acs/tech_docs/accuracy/MultiyearACSAccuracyofData2017.pdf?#" target="_blank">Accuracy
of the Data</a> gives more information about potential sources of
error.  
<br> Further, producing the “weighted averages” necessarily assumed
equal distributions of populations across individual census block groups
or tracts in order. Population characteristics are likely to be
heterogeneous, rather than homogeneous, across areas. Thus, observed
population characteristics within a given buffer distance may not
reflect the weighted averages presented here, but that does not diminish
the value of the weighted averages as a starting point for conversations
and actions to increase equitable usage of parks and trails across the
Metro Region. <br></br> ACS statistics were used to calculate the
percent of people with a certain characteristic within each block group
or tract. We scaled these percentages by the 2019 small area population
estimates to obtain a more accurate number of people within each group
(this more accurate measure of population is not available in collar
counties). Weighted averages were produced by multiplying by the percent
overlap of a given block group or tract with the buffer zone around
individual park and trail units. Variables on disability (% with an
ambulatory disability, % with any other disability) and national origin
(% US-born, % foreign-born) were calculated at the tract level. All
other variables were calculated at the block group.

### Data sources

All data used in the application is hosted on the Minnesota Geospatial
Commons.

  - <a href="https://gisdata.mn.gov/dataset/us-mn-state-metc-society-census-acs" target="_blank">ACS
    data</a>
  - <a href="https://gisdata.mn.gov/dataset/us-mn-state-metc-trans-anlys-zones-frcst-taz-com" target="_blank">Long-range
    population forecasts</a>
  - <a href="https://gisdata.mn.gov/dataset/us-mn-state-metc-society-small-area-estimates" target="_blank">Annual
    small area population estimates</a>

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
2015-2019).” 2019, 29.
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
*Last updated 2021-03-18*  
Build ID: 2021-03-18.roten.51df77e  
</right>
