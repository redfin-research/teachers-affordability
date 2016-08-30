########################################
# Teacher's Affordability Report
########################################

# Clear all
rm(list=ls())

#######################
# Set state for script
state_abb <- "CA"
#######################

# Set up packages
packagesRequired <- c('tigris', 
                      'htmlwidgets',
                      'readr',  
                      'stringr',  
                      'dplyr',   
                      'ggplot2',  
                      'maptools',
                      'leaflet',
                      'purrr',
                      'data.table',
                      'blscrapeR')

packagesMissing <- packagesRequired[!(packagesRequired %in% installed.packages())]

for (package in packagesMissing) {
    install.packages(package)
}

for (package in packagesRequired){
    eval(parse(text=paste0('suppressPackageStartupMessages(library(', package, '))')))
}

# Set environment
setwd("~/Google Drive/PRTeam Analysis/Misc/2016-08 Misc/Teachers Report")

# state reference data
state_match = data.frame(state.name, state.abb)

# Grab full name
state_full <- as.vector(state_match$state.name[state.abb==state_abb])

# BLS reference data
bls_areatypes <- read_tsv("http://download.bls.gov/pub/time.series/oe/oe.areatype")
bls_areas <- read_tsv("http://download.bls.gov/pub/time.series/oe/oe.area")
bls_industries <- read_tsv("http://download.bls.gov/pub/time.series/oe/oe.industry")
bls_occupations <- read_tsv("http://download.bls.gov/pub/time.series/oe/oe.occupation")
bls_datatypes <- read_tsv("http://download.bls.gov/pub/time.series/oe/oe.datatype")

# Load in BLS data (downloaded as of May 2015)
# Available from: http://www.bls.gov/oes/special.requests/oesm15all.zip
# Cleaned up manually for csv
salaries <- read_csv("teachers_salary.csv")

# BLS codes for Preschool, Primary, and Secondary Teachers (not postsecondary)
teacher_occ_codes<-c(252021, 252022, 252031)

salaries$`occ code`<-str_replace_all(salaries$`occ code`,"-","")
salaries$`occ code`<-as.numeric(salaries$`occ code`)

# Filter for teachers
salaries <- filter(salaries,`occ code` %in% teacher_occ_codes)

# Gather BLS state_code
state_code <- bls_areas$area_code[bls_areas$areatype_code=="S" &
                        bls_areas$area_name==state_full]

# BLS Metro-county mapping
bls_area_def <- read_csv("~/Google Drive/PRTeam Analysis/Geo Setup/bls_area_definitions_m2015.csv")

# Clean up column names
cn <- gsub(" ", "_", colnames(bls_area_def))
colnames(bls_area_def)<- sapply(cn, tolower)

# Function to subset BLS data by state
state_filter <- function(state_abb = "CA") {
    state_full <- as.vector(state_match$state.name[state.abb==state_abb])
    
    # Calculated weighted mean for salary & Subset for state
    state_salaries <- salaries %>%
        group_by(area, area_title) %>%
        summarise(teacher_salary = weighted.mean(a_median, tot_emp, na.rm = T)) %>%
        filter(grepl(paste0(state_abb,"|", state_full," nonmetropolitan"), area_title, ignore.case = F))
    
    state_median_salary <- salaries %>%
        filter(grepl(paste0(state_abb,"|", state_full," nonmetropolitan"), area_title, ignore.case = F)) %>%
        summarise(area = state_code, area_title = state_full, 
                  teacher_salary = weighted.mean(a_median, tot_emp, na.rm = T))
    
    bls_area_def <- bls_area_def %>% 
        filter(state == state_full) %>% 
        select(-township_code, -msa_code_for_msas_with_divisions, -msa_name_for_msas_with_divisions)

    state_salaries_merged <- merge(state_salaries, bls_area_def, by.x = "area", by.y = "msa_code_(including_msa_divisions)", all.y = T)
    
    state_salaries_merged <- merge(state_salaries_merged, state_median_salary, all = T)
    return(state_salaries_merged)
}

# Call Function
state_salaries_merged <- state_filter(state_abb)

# clean names
colnames(state_salaries_merged)[9]<-"county_name"

# Remove uneeded large objects
rm(salaries)
rm(bls_area_def)

# Set interest rate
interest_rate = 0.035

# Calculate max monthly payment and max home price 
# using 40 percent affordability criteria
state_salaries_merged <- state_salaries_merged %>%
    select(area_title, county_name, county_code, teacher_salary) %>%
    mutate(max_payment = round((teacher_salary / 12) * 0.4 / 25) * 25, 
           max_home_price = round(
               ((teacher_salary / 12) * 0.4 - 500) /
                    ( (interest_rate/12) + (interest_rate/12) /
                        (
                          (1 + (interest_rate/12))^(30*12)
                             -1)) /
                   10000) * 10000
           )

##Redfin Specific Data##
# Gather Redfin's active MLS data

# Set up Redshift connection
get_active_listings <- function(state_abb = "CA") {
    if(!(exists("rscon"))){
        message("Loading database connections")
        suppressPackageStartupMessages(
            # Load in credentials and JDBC connection
            source("~/Google Drive/PRTeam Analysis/R Scripts/Connecting to Redfin db in R.R")
        )
}
        q <- paste0("select listing_id, 
                    county_id, 
                    coalesce(list_price, original_price) as list_price, 
                    hoa_dues, 
                    pt.name,
                    num_bedrooms, 
                    num_bathrooms, 
                    approx_sq_ft,
                    'http://www.redfin.com/'::text || upper(l.state_code)::text || '/'::text || regexp_replace(initcap(trim(l.city)), ' ', '-')::text || '/home/'::text || l.property_id::text as url
                    from listings l
                    join property_types pt on pt.property_type_id=l.property_type_id
                    where listing_type_id = 1
                    and search_status_id = 1
                    AND is_short_sale = 0
                    AND is_bank_owned = 0
                    and l.property_type_id in (3,6,13)
                    and coalesce(listing_date::date, listing_added_date::date) <= current_date-1
                    and coalesce(listing_date::date, listing_added_date::date) > current_date-366
                    and coalesce(off_market_date, sale_date) is null
                    and county_id is not null
                    and coalesce(list_price, original_price) >= 10000
                    and state_code = '",state_abb,"'
                    order by list_price desc;"
        )
        message(paste0("Getting listings for ",state_abb))
        
        listings <- dbGetQuery(rscon, statement = q)
        
        # Gather county property tax rates
        q2 <- paste0("select c.county_id, c.display_name, t.rate as prop_tax_rate
        from county_property_tax_rates t
                     join counties c on c.county_id=t.county_id
                     join states s on s.state_id=c.state_id
                     where s.state_code = '",state_abb,"';")
        county_tax_rates <- dbGetQuery(srcon, statement = q2)
       
        listings <- merge(listings, county_taxe_rates, by.x="county_id", by.y="county_id", all.x = T)
        
        return(listings)
}
# Call function
listings <- get_active_listings(state_abb)

# Function to estimate monthly mortgage payments
monthly_mortgage_calculator <- function(home_price, 
                                        hoa_dues = 0,
                                        ann_property_taxes = .01125,
                                        interest_rate = 0.035, 
                                        down_payment = .20,
                                        length = 30,
                                        homeowners_insurance = 70,
                                        PMI = .0075,
                                        round = TRUE) {
    if (is.character(home_price)) {
        home_price <- as.numeric(home_price)
    }
    loan_amount <- home_price - home_price*down_payment
    N <- 12*length
    monthly_mortgage <- loan_amount*(interest_rate/12)/(1-(1/(1+interest_rate/12)^N))
    if (hoa_dues > 0 & !is.na(hoa_dues)) {
        monthly_mortgage = monthly_mortgage + hoa_dues
    }
    if (down_payment < .2 & PMI > 0) {
        monthly_mortgage = monthly_mortgage + home_price*PMI/12
    }
    if (ann_property_taxes > 0 & !is.na(ann_property_taxes)) {
        monthly_mortgage = monthly_mortgage + (ann_property_taxes*home_price/12)
    } else {
        monthly_mortgage = monthly_mortgage + (.01125*home_price/12)
    }
    monthly_mortgage = monthly_mortgage + homeowners_insurance
    if (round == TRUE) {
        monthly_mortgage = round(monthly_mortgage, 2)
    }
    
    return(monthly_mortgage)
}

# Call function to calculate mortgage payments
listings$monthly_mortgage <- as.vector(
        unlist(
            pmap(
                list(
                     home_price = listings$list_price, 
                     hoa_dues = listings$hoa_dues, #hoa dues by listing
                     ann_property_taxes = listings$prop_tax_rate, #using tax rate by county
                     interest_rate = .035, #Use current interest rates
                     down_payment = .1), #10% down payment assumption
            monthly_mortgage_calculator)))

# Join salary data, removing rows without county_names
listings <- merge(listings, state_salaries_merged, by.x = "display_name", by.y = "county_name")

# Calculate percent affordable by county
(county_affordability <- listings %>%
    group_by(display_name) %>%
    mutate(n_listings = n()) %>%
    summarise(affordable_listings = sum(
        if_else(
            monthly_mortgage <= max_payment,1,0)),
        total_listings = median(n_listings,na.rm=T),
        percent_affordable = affordable_listings/total_listings) %>%
    arrange(percent_affordable))

# Get max payment at the state level
state_max_payment <- state_salaries_merged$max_payment[
    state_salaries_merged$area_title==state_full & 
        !(is.na(state_salaries_merged$area_title))]

# Calculate percent of listings affordable for the entire state
(state_affordability <- listings %>%
    mutate(n_listings = n()) %>%
    summarise(affordable_listings = sum(
        if_else(
            monthly_mortgage <= state_max_payment,1,0)),
        total_listings = median(n_listings,na.rm=T),
        percent_affordable = affordable_listings/total_listings) %>%
    arrange(percent_affordable))

state_affordability$area_title <- state_full

# Join salary data to affordability numbers
state_salaries_merged <- merge(state_salaries_merged, county_affordability, by.x = "county_name", by.y = "display_name", all = T)

# Fill in missing data with 0
state_salaries_merged$affordable_listings[
    is.na(state_salaries_merged$affordable_listings) 
    & !is.na(state_salaries_merged$total_listings)]<-0

state_salaries_merged$percent_affordable[
    is.na(state_salaries_merged$percent_affordable) 
    & !is.na(state_salaries_merged$total_listings)]<-0

# Fill in state level data
state_salaries_merged$affordable_listings[
    state_salaries_merged$area_title==state_full]<-
        state_affordability$affordable_listings[
            state_affordability$area_title==state_full]

state_salaries_merged$total_listings[
    state_salaries_merged$area_title==state_full]<-
        state_affordability$total_listings[
            state_affordability$area_title==state_full]

state_salaries_merged$percent_affordable[
    state_salaries_merged$area_title==state_full]<-
        state_affordability$percent_affordable[
            state_affordability$area_title==state_full]

# Remove County codes
state_salaries_merged<-select(state_salaries_merged,-county_code) %>%
    filter(!is.na(area_title)) %>%
    arrange(percent_affordable)

# save output as raw csv
write_csv(state_salaries_merged, paste0(state_abb,"_affordability_by_county.csv"))

# Format data for excel output
excel_file <- state_salaries_merged

simpleCap <- function(x) {
    s <- strsplit(x, " ")[[1]]
    paste(toupper(substring(s, 1, 1)), substring(s, 2),
          sep = "", collapse = " ")
}

# Clean up column names 
cn <- gsub("_", " ", colnames(excel_file))
colnames(excel_file) <- sapply(cn, simpleCap)

# Write output as clean excel
WriteXLS::WriteXLS(excel_file, 
                   paste0(state_abb,"_affordability_by_county.xls"),
                   row.names = FALSE,
                   AdjWidth = TRUE,
                   AutoFilter = FALSE,
                   BoldHeaderRow = TRUE,
                   FreezeRow = 1,
                   verbose = TRUE)

################################################
## HTML Examples Table ##

# Create examples
affordable_listing_examples <- listings %>%
    filter(monthly_mortgage <= max_payment, !is.na(url)) %>%
    arrange(desc(list_price), desc(num_bedrooms), desc(approx_sq_ft)) %>%
    group_by(display_name) %>%
    filter(row_number()%in%c(1)) %>%
    arrange(display_name)


# Create description of examples
affordable_listing_examples$description <- paste(
    affordable_listing_examples$num_bedrooms,"bed,", 
    round(affordable_listing_examples$num_bathrooms,1), "bath,", 
    affordable_listing_examples$approx_sq_ft, "sqft", 
    ifelse(affordable_listing_examples$name=="Single Family Residential", "house",
           ifelse(affordable_listing_examples$name=="Condo/Co-op","condo","townhouse"))) 


# Create HTML table
tbl <- affordable_listing_examples %>% 
    select(display_name, teacher_salary, max_payment, max_home_price, url, description, county_id) %>%
    filter(!(is.na(display_name))) %>%
    arrange(display_name)

# Separate state-level data to add to table
state_affordable <- state_salaries_merged[state_salaries_merged$area_title==state_full 
                                         & !(is.na(state_salaries_merged$area_title)),]

tbl2<-cbind.data.frame(
    display_name=as.character(state_affordable$area_title[1]),
    teacher_salary=state_affordable$teacher_salary,
    max_payment=state_affordable$max_payment,
    max_home_price=state_affordable$max_home_price,
    url=as.character("http://www.redfin.com/"),
    description=as.character("View more on Redfin.com"),
    county_id=NA, 
    stringsAsFactors = F
    )
tbl2<-group_by(tbl2,display_name)

# Clean County Name
tbl$display_name<-str_replace(tbl$display_name, " County","")

# Join state data and county-level examples
l = list(tbl,tbl2)
tbl<-rbindlist(l)

setDT(tbl)

# Create HTML code
html <- NULL
html <- "
<h3>What a Teacher's Salary Can Buy</h3>
<table class='ScienceTable' style = 'font-size: 14px;'>
<thead>
<tr style='height: 14px !important;'>
<th style='text-align: left' class='c0 '>County</th>
<th style='text-align: right' class='c1 '>Teacher's Salary</th>
<th style='text-align: right' class='c2 '>Max Home Price</th>
<th style='text-align: right' class='c3 '>Link to Example</th>
</tr>
</thead>
<tbody>"

# loop through each county writing HTML code with examples
for (i in 1:nrow(tbl)) {
    html <- paste0(html, "
                 <tr class=''>
                 <td style='text-align: left' class='c0 '>",
                   ifelse(!is.na(tbl[i, 7, with = FALSE]),
        paste0("<a href=https://www.redfin.com/county/", tbl[i, 7, with = FALSE], "/?utm_source=blog&utm_medium=post&utm_content=real_estate&utm_campaign=1002170'>", tbl[i, 1, with = FALSE], "</a></td>"
               ),
        paste0("<b>",tbl[i, 1, with = FALSE],"</b></td>")
    ),"<td style='text-align: right' class='c1 '>", paste0("$", prettyNum(100*round(tbl[i, 2, with = FALSE]/100), big.mark = ",", scientific = FALSE)), "</td>
                 <td style='text-align: right' class='c2 '>", paste0("$", prettyNum(tbl[i, 4, with = FALSE], big.mark = ",", scientific = FALSE)), "</td>
                 <td style='text-align: right' class='c3 '><a href=", tbl[i, 5, with = FALSE], "?utm_source=blog&utm_medium=post&utm_content=real_estate&utm_campaign=1002170'>", tbl[i, 6, with = FALSE], "</a></td></tr>")
}

html <- paste0(html, "</tbody></table>")

html <- gsub("\\n", "", html)

# Save final HTML output
write.table(html, paste0(state_abb,"_affordability_by_county.html"), col.names = FALSE, row.names = FALSE, quote = FALSE)

################################################
## Visualize findings in heatmap ##

# grab the spatial data (tigris)
counties <- counties(state = state_abb, cb=TRUE)

# Remove NAs
state_salaries_merged<-state_salaries_merged[!is.na(state_salaries_merged$teacher_salary),]

# Geo join for mapping
state_salaries_geo <- geo_join(counties, state_salaries_merged, "COUNTYFP", "county_code")

# Make tooltip popup for map
popup <- paste0("County: <b>", state_salaries_geo$NAME, "</b><br>", 
                "Average Salary for Teachers: <b>", scales::dollar(round(state_salaries_geo$teacher_salary/1000,1)*1000), 
                "</b><br>Percent of listings affordable: <b>", scales::percent(round(state_salaries_geo$percent_affordable, 3)), "</b>"
                )

# Create color scale for heatmap
pal <- colorBin(
    palette = rev(RColorBrewer::brewer.pal(9, "YlOrRd")),
    domain = state_salaries_geo$percent_affordable,
    bins = 9,
    na.color = "#FFFFFF"
)

# Make Map with leaflet
state_salaries_map <-leaflet() %>%
    addProviderTiles("CartoDB.Positron") %>%
    addPolygons(data = state_salaries_geo, 
                fillColor = ~pal(percent_affordable), 
                color = "#b2aeae", # you need to use hex colors
                fillOpacity = 0.7, 
                weight = 1, 
                smoothFactor = 0.2,
                popup = popup
                ) %>%
    addLegend(pal = pal, 
              values = state_salaries_geo$percent_affordable*100, 
              position = "bottomleft", 
              title = "Percentage affordable",
              na.label = "No data",
              labFormat = labelFormat(suffix = "%",
                    transform = function(x) 100 * x)
              ) %>%
    setMaxBounds(state_salaries_geo@bbox[1], # removing paning the globe 
                 state_salaries_geo@bbox[2], # as data may become removed
                 state_salaries_geo@bbox[3], 
                 state_salaries_geo@bbox[4])

#View map in Viewer (R)
state_salaries_map 

# Save Map as fully contained HTML file
saveWidget(state_salaries_map, file=paste0(state_full,"_salaries.html"), selfcontained=T)
