
## Notes

*Regional Parks and the American Community Survey* is a mini-app
designed for use by Regional Parks implementing agencies to assist in
their progressing toward a more equitable use of regional parks. The app
complements the [Regional Parks Equity
Toolkit](https://metrocouncil.org/parks/Planning/Parks-Equity-Toolkit.aspx),
a set of questions and a process to clarify how regional park projects
are advancing equity. The app facilitates direct examination of regional
parks and trails system and the demographic characteristics of the
census tracts surrounding them.

The “Map” tab displays census tracts in the Twin Cities metropolitan
area. The tracts’ colors correspond to 2014-2018 (5-year) American
Community Survey (ACS) demographic metric selected by the user. Each
demographic characteristic is shown as a percentage of the total
population, with the exception of median household income, which is
displayed in dollars. The darker the color, the higher the percentage
(or income in dollars).

Statistics produced for American Community Survey and included in ACS
tables are survey-based estimates and are subject to error. The errors
derive from research design (including instrument bias, data frame, and
sampling), the survey data collection (non-response bias and response
errors), and processing by the Census Bureau (data coding, compilation
processes, and case weighting), as well as statistical inference error
and uncertainty (which are related to sample size and variance within
the measured attributes). US Census Bureau’s report on “Accuracy of the
Data” can be found
[here](https://www2.census.gov/programs-surveys/acs/tech_docs/accuracy/MultiyearACSAccuracyofData2017.pdf?#).

### Variable dictionary

some text about the variables included/not included here.

### Contributors

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
*Last updated 2020-12-21*  
Build ID: 2020-12-21.roten.094d502  
</right>
