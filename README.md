# A global database of pandemic- and epidemic-prone disease outbreaks
## Abstract
Here can be found a new dataset of infectious disease outbreaks collected from the Disease Outbreak News and the Coronavirus Dashboard produced by the World Health Organization. The unit of analysis in the database is the outbreak, that happens when a country has at least one case of a specific disease during a particular year. Thus, a specific country cannot have two outbreaks related to the same disease in the same year (but can have more than one outbreak of different diseases in the same year). Moreover, a country can only have more than one outbreaks of the same disease, if and only if they refer to different years. 
Target applications of this information should focus on combining our data with other information sources to study, for instance, the link between environmental, geographic, globalization, or socio-economic factors with the exposure of countries to disease outbreaks.

## List of files
### "OutbreaksData.R"
#### Description: 
R-code to replicate the extraction of data from the Disease Outbreak News (DONs), available at www.who.int/emergencies/disease-outbreak-news, and the Coronavirus Dashboard, available at https://covid19.who.int/, produced by the World Health Organization (WHO). In addition, this code includes the integration, data correction, and use of standard names and definitions from the ISO 3166 and the International Statistical Classification of Diseases and related Health Problems. 

### "DONs.RData" (or alternatively "DONs.csv)
#### Description: 
Dataset containing the raw data from 2700 DONs as extracted from www.who.int/emergencies/disease-outbreak-news. The dataset presents information from 1996 to 2021 (last DON was registered on 14 December 2021).
#### Data records:
"ID": DON unique identifier. 

"Description": Name of the DON as extracted from www.who.int/emergencies/disease-outbreak-news.

"Date": Date of registry of the DON.

"Link": Link to the website to find more information on the DON.

### "DONsOutbreaks.RData" (or alternatively "DONsOutbreaks.csv")
#### Description: 
Dataset containing 1510 observations with unique outbreaks, covering a total of 215 countries and territories, and 69 different infectious diseases. A unique outbreak happens when a country has at least one case of a specific disease during a particular year. Thus, a specific country cannot have two outbreaks related to the same disease in the same year (but can have more than one outbreak of different diseases in the same year). Moreover, a country can only have more than one outbreaks of the same disease, if and only if they refer to different years.
#### Data records:
"Country": Name of the country where the outbreak occurred.

"iso2": Alpha-2 country code from the ISO 3166.

"iso3": Alpha-3 country code from the ISO 3166.

"Year": Year of occurrence of the outbreak.

"icd10n": Name of the type of disease according to the ICD-10.

"icd103n": Name of the subtype of disease according to the ICD-10.

"icd104n": Name of the disease according to the ICD-10.

"icd10c": Code of the name of the type of disease according to the ICD-10.

"icd103c": Code of the name of the subtype of disease according to the ICD-10.

"icd104c": Code of the name of the disease according to the ICD-10.

"icd11c1": Code of the name of the type of disease according to the ICD-11.

"icd11c2": Code of the name of the subtype of disease according to the ICD-11.

"icd11c3": Code of the name of the disease according to the ICD-11.

"icd11l1": Name of the type of disease according to the ICD-11.

"icd11l2": Name of the subtype of disease according to the ICD-11.

"icd11l3": Name of the disease according to the ICD-11.

"Disease": Name of the disease.

"DONs": DONs reporting the outbreak. 

"Definition": Definition of the disease according to the ICD-11.

### "COVIDOutbreaks.RData" (or alternatively "COVIDOutbreaks.csv")
#### Description: 
Dataset containing 441 observations occurred since 2020, covering a total of 222 countries and territories in which cases of COVID-19 were reported. In particular, we dichotomized the information on the cases per country per year to assign a one if the country had at least one case of coronavirus, and zero otherwise. 
#### Data records:
"Country": Name of the country where the outbreak occurred.

"iso2": Alpha-2 country code from the ISO 3166.

"iso3": Alpha-3 country code from the ISO 3166.

"Year": Year of occurrence of the outbreak.

"icd10n": Name of the type of disease according to the ICD-10.

"icd103n": Name of the subtype of disease according to the ICD-10.

"icd104n": Name of the disease according to the ICD-10.

"icd10c": Code of the name of the type of disease according to the ICD-10.

"icd103c": Code of the name of the subtype of disease according to the ICD-10.

"icd104c": Code of the name of the disease according to the ICD-10.

"icd11c1": Code of the name of the type of disease according to the ICD-11.

"icd11c2": Code of the name of the subtype of disease according to the ICD-11.

"icd11c3": Code of the name of the disease according to the ICD-11.

"icd11l1": Name of the type of disease according to the ICD-11.

"icd11l2": Name of the subtype of disease according to the ICD-11.

"icd11l3": Name of the disease according to the ICD-11.

"Disease": Name of the disease.

"DONs": Coronavirus Dashboard.

"Definition": Definition of the disease according to the ICD-11.

### "Outbreaks.RData" (or alternatively "Outbreaks.csv")
#### Description: 
Dataset containing 1951 observations (unique disease outbreaks), occurred in a total of 233 countries and territories from 1996, and associated to 70 different infectious diseases. An unique outbreak happens when a country has at least one case of a specific disease during a particular year. 
#### Data records:
"Country": Name of the country where the outbreak occurred.

"iso2": Alpha-2 country code from the ISO 3166.

"iso3": Alpha-3 country code from the ISO 3166.

"Year": Year of occurrence of the outbreak.

"icd10n": Name of the type of disease according to the ICD-10.

"icd103n": Name of the subtype of disease according to the ICD-10.

"icd104n": Name of the disease according to the ICD-10.

"icd10c": Code of the name of the type of disease according to the ICD-10.

"icd103c": Code of the name of the subtype of disease according to the ICD-10.

"icd104c": Code of the name of the disease according to the ICD-10.

"icd11c1": Code of the name of the type of disease according to the ICD-11.

"icd11c2": Code of the name of the subtype of disease according to the ICD-11.

"icd11c3": Code of the name of the disease according to the ICD-11.

"icd11l1": Name of the type of disease according to the ICD-11.

"icd11l2": Name of the subtype of disease according to the ICD-11.

"icd11l3": Name of the disease according to the ICD-11.

"Disease": Name of the disease.

"DONs": DONs reporting the outbreak. For the case of the Coronavirus, the source is the Coronavirus Dashboard.

"Definition": Definition of the disease according to the ICD-11.

### "ESDAoutbreaks.R"
#### Description: 
R-code to replicate the exploratory spatial data analysis (ESDA).

### "GeoOutbreaks.RData"
#### Description: 
Dataset containing the total number of unique disease outbreaks per country with geo-spatial information.  
#### Data records:
"iso3": Alpha-3 country code from the ISO 3166.

"region": Region of the country.

"continent": Continent of the country.

"name": Name of the country where the outbreak occurred.

"french_shor": Name of the country in French.

"freq": Total frequency of disease outbreaks.

"geometry": Geographic coordinates of the country.
