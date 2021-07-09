Processing data for parks.acs app
================
Ellen
09 July 2021

-   [Overview](#overview)
    -   [Park and trail geography](#park-and-trail-geography)
    -   [ACS data](#acs-data)
    -   [Create the tabular weighted average ACS
        values](#create-the-tabular-weighted-average-acs-values)
    -   [Create the spatial ACS data](#create-the-spatial-acs-data)
    -   [Create population estimates](#create-population-estimates)
    -   [Create transit layer](#create-transit-layer)
    -   [Only keep final files](#only-keep-final-files)
-   [reproject](#reproject)

# Overview

This markdown file walks through the creation of all the data files
necessary to run the parks.acs app.

**You must connect to the N drive to run this script**

## Park and trail geography

First a single file containing the geographies of the regional parks and
trail units needs to be created from files which are published to the MN
geospatial commons. *Note:* this is a single, long sf file because the
buffer map parses all combinations of agencies \* status \* type. All
code has been updated so that the leaflet map works with the long data
as well.

Running this script produces: - `park_trail_geog_LONG.rda`.

As an aside, if any parks/trails need to be reassigned to different
agencies and/or statuses, this is the place to do that.

Park amenities produces park entrance locations and public water access
locations.And rivers/lakes.

## ACS data

ACS data at both the tract and block group levels needs to be processed
for the 7 county core, as well as collar counties. We want to include
the collar counties within this processing step because the buffer area
for some parks and trails extends into areas beyond the 7 county core
region.

Running `census_tract_raw.R` produces: - `county_outlines.rda`
(leveraging the fact that we have to load the tigris package anyways) -
`census_tract_raw.rda`

Running `block_group_raw.R` produces: - `block_group_raw.rda`

If acs variables need to be added, this is one place where that should
be done.

## Create the tabular weighted average ACS values

And this is also done at the block group and tract levels. In some cases
demographic variables are suppressed at the block group. We must process
those variables at the tract level.

Running `weighted_avs_bg.R` produces: - `agency_avg_bg.rda` (the average
acs values within an implementing agency’s jurisdiction) -
`long_buffer_data_bg.rda` (acs values for each park/trail unit at each
buffer distance for the variables which exist at the block group level)
- `agency_planned_existing_avgs` (agency-level averages for each
variable including existing and planned units) - `buffer_geo.rda` (the
buffer geometries at 1, 1.5 and 3 mi radii. these will be plotted)

Running `weighted_avs_tracts.R` produces: - `long_buffer_data_tract.rda`
() - `agency_planned_existing_avgs_tract.rda` () - `agency_avg_tract` ()

and then we will join the 2 geographies together to create one cohesive
weighted average dataset.

Create some helper functions

Download small area estimates:

## Create the spatial ACS data

We only want to plot the block groups/tracts within the 7 county core
region or within the collar counties if the bg/tract intersects with a
buffer

Running this produces: - `collar_filter.rda` - `block_group.rda` -
`census_tract.rda`

## Create population estimates

This script doesn’t depend on inputs from any other script.

Running this produces: - `est_pop.rda`

## Create transit layer

## Only keep final files

# reproject
