#AMADEUS - A database of comparable financial information for public and private companies across Europe

## General information

Bureau Van Dijk product - BVD private company owned by Moody's. See [database information](https://www.bvdinfo.com/en-gb/our-products/data/international/amadeus).

Downloaded from UCL databases with UCL ID: https://library-guides.ucl.ac.uk/az.php.

This extract contains firm-level information on ownership links. It includes firms for which an ownership link exists, i.e. a "Global Ultimate Owner" (GUO) can be defined. This corresponds to around 3Mio firms out of 24Mio firms in the full database (which is already not exhaustive).


## Metadata

### Firm level

File `amadeus_nodes.csv`

  - `id` (String) unique ID of the firm
  - `name` (String) name of the firm
  - `lon` (Double) longitude of the firm (see geocoding below)
  - `lat` (Double) latitude of the firm
  - `country` (String) ISO code of the country
  - `city` (String) city of the firm
  - `zipcode` (String) zipcode of the firm
  - `nacecode` (Int) industrial NACE code ([Nomenclature des Activités économiques de la Communauté Européenne](https://ec.europa.eu/eurostat/statistics-explained/index.php/NACE_background))
  - `naicscode` (Int) industrial NAICS code ([North American Industry Classification System](https://www.naics.com/search/))
  - `nationalcode` (String) national industrial code
  - `turnover` (Double) turnover in Euros
  - `employees` (Int) number of employees
  - `latestinfo` (Int) year at which turnover and employees was last observed
  - `selfown` (Boolean) does the company owns itself


File `amadeus_links.csv`

  - `from` (String) ID of the owner
  - `to` (String) ID of the owned company
  - `direct_ownership` (Double) percentage of direct ownership
  - `total_ownership` (Double) percentage of total ownership (includes indirectly owned shares)
  - `date` (String) Month/Year at which the link was last observed (spans 2013-2018, but mostly in 2018)


### Functional Urban Areas

The data was also aggregated at the level of Functional Urban Areas to ensure a consistent urban firm network. The [Global Human Settlements Layer - Functional Urban Areas](https://ghsl.jrc.ec.europa.eu/ghs_fua.php) (GHS-FUA) database was used for this step (see description below).

File `fuacities.csv`

 - `id` (Int) id of the FUA within the GHSL databae
 - `name` (String) name of the FUA
 - `time` (Int) dummy variable
 - `x` (Double) longitude
 - `y` (Double) latitude
 - `country` (String) country
 - `turnover` (Double) total turnover within the FUA, computed as the sum of turnovers
 - `sectorX` with `X = {A, ..., U}` (Double) proportion of turnover in the FUA belong to sector `X` ([primary NACE sectors](https://ec.europa.eu/competition/mergers/cases/index/nace_all.html))

File `fualinks.csv`

 - `from_fua` (Int) id of origin FUA
 - `to_fua` (Int) id of destination FUA
 - `time` (Int) dummy variable
 - `weight` (Double) weight of the link, computed as the total owned turnover (see aggregation below)

## Statistics

Firms:

 - 3,053,540 firms
 - 2,715,188 with coordinates
 - 2,036,272 within FUAs
 - 2,033,799 within EU and FUAs
 - 1,589,593 within FUAs with a link
 - 763,151 within FUAs, linked and with turnover

Links:

 - 1,866,936 links
 - 652,258 with an origin FUA within EU, 1,326,340 with a destination FUAs within EU, 593,823 between FUAs within EU
 - 157,645 between FUAs, with ownership and destination turnover
 - 12,463 aggregated links between FUAs

For the UK:

 - 463,731 firms; 413,740 firms within FUAs; 336,804 firms within FUAs and with a link; 78,273 firms within FUAs and with turnover; 68,273 within FUAs, with turnover and a link
 - 142,975 links from UK; 323,618 links to UK; 131,819 links within UK
 - 128,066 links within FUAs from UK; 293,820 links within FUAs to UK; 116,661 within FUAs internal to UK
 - 12,480 between FUAs, with ownership and destination turnover
 - 1,524 aggregated links from UK; 1,541 to UK; 665 within UK


## Database construction

All code for data collection, processing and an example of application (empirical study of the network and calibration of a generative network model) is available on an open [git repository](https://github.com/JusteRaimbault/ABMCitiesFirms).

 - Download of the database: data was downloaded by chunks from the interface of Amadeus, with the filter "Region/Country/region in Country in European Union" (15,183,677 results) with columns: Company name, ID, Country, City, Zip code, NACE code, NAICS code, National industry code (primary), Last year (year for turnover and employer information), Turnover, Number of Employees, Name of GUO, ID of GUO, Country of GUO, City of GUO, NACE of GUO, NAICS of GUO, Percentage of direct ownership, Percentage of total ownership, Date of information on ownership, Turnover of GUO, Employees of GUO.

 - Consolidation of raw files: separate raw files were cleaned, filtered for actual companies with a location and a Global Ultimate Owner, and consolidated into a single file ([script](https://github.com/JusteRaimbault/ABMCitiesFirms/blob/master/DataCollection/process.R)).

 - Geolocation: ZIP codes are provided for destination of links (owned) and City for origin (Global Ultimate Owner). Using the [Geonames](https://download.geonames.org/) database, lookup tables for zip and places were constructed, and coordinates were attributed to companies ([script](https://github.com/JusteRaimbault/ABMCitiesFirms/blob/master/DataCollection/nwdataprocessing.R)). Note that the granularity differs highly between countries and zip/city, so non-aggregated spatial analysis must be done with caution. At the FUA level, these discrepancies disappear and the data is more consistent. At this step, firms and link tables are constructed, and the firm level database is exported as `amadeus_nodes.csv` and `amadeus_links.csv`.

 - Aggregation at the FUA level: filter firms with coordinates, overlay with FUA polygons. Links with positive destination turnover where aggregated by origin and destination FUAs, with a weight corresponding to the owned turnover, i.e. `weight = %-ownership * destination_turnover`. The industrial composition of FUAs was constructed by considering the first digit of the NACE only (22 categories), and for each sector, the proportion of turnover in the FUA within it was computed.
