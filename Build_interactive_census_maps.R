########################################
# Teacher's Affordability Report
########################################

# Clear all
rm(list=ls())

#######################
# Set state for script
state_abb <- "WA"
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
                      'blscrapeR',
                      'gdata')

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


getBLSData <- function(year="2015") {
    
    #create folder for data
    if (!file.exists("BLSdata")) {
        dir.create("BLSdata")
    }
    setwd("./BLSdata/")
    
    year_abb <- str_sub(year, start = -2)
    
    file <- paste0("oesm", year_abb, "all/all_data_M_",year,".xlsx")
    
    if (!file.exists(file)) {
        
        # Download data from the following site:
        # http://www.bls.gov/oes/tables.htm
        fileUrl <- paste0("http://www.bls.gov/oes/special.requests/oesm",year_abb,"all.zip")
        excel_file <- paste0("all_data_M_",year,".xlsx")
        download.file(fileUrl, excel_file)
        unzip(excel_file)
        file.remove(excel_file)
        #store the date of retrieval
        dateDownloaded <- Sys.Date()
    }
    
    # Set wd back to main folder
    setwd("../")
}

if(!file.exists("./BLSdata/current_teachers_salaries.csv")) {
    
    # Call get Data function
    getBLSData("2015")
    
    #Clean 2015 data
    year = 2015
    year_abb <- str_sub(year, start = -2)
        
    csv_file <- paste0("oesm", year_abb, "all/all_data_M_",year,".csv")
        
    if (!file.exists(csv_file)) {
        stop(paste0("Open ",file," manually and save 1st sheet as csv to continue."), call. = FALSE)
    }
    
    data <- read_csv(csv_file)
    data <- data[,c(1:5,7:8,10,25)]
    
    # BLS codes for Preschool, Primary, and Secondary Teachers (not postsecondary)
    teacher_occ_codes<-c(252021, 252022, 252031)
    
    data$`occ code`<-str_replace_all(data$`occ code`,"-","")
    data$`occ code`<-as.numeric(data$`occ code`)
    
    # Filter for teachers
    data <- filter(data,`occ code` %in% teacher_occ_codes, a_median != "*")
    
    # Replace NA of tot_emp to allow for weighted averages
    data$tot_emp[is.na(data$tot_emp)]<-1
    
    write_csv(data, "current_teachers_salaries.csv")
}

salaries <- read_csv("./BLSdata/current_teachers_salaries.csv")

if(!file.exists("./BLSdata/prev_teachers_salaries.csv")) {
    
    getBLSData("2012")
    
    #Clean 2012 data
    year = 2012
    year_abb <- str_sub(year, start = -2)
    
    csv_file <- paste0("all_oes_data_",year,".csv")
    
    if (!file.exists(csv_file)) {
        stop(paste0("Open ",file," manually and save 1st sheet as csv to continue."), call. = FALSE)
    }
    
    data <- read_csv(csv_file)
    data <- data[,c(1:5,7:8,10,25)]
    
    # BLS codes for Preschool, Primary, and Secondary Teachers (not postsecondary)
    teacher_occ_codes<-c(252021, 252022, 252031)
    
    data$occ_code <-str_replace_all(data$occ_code,"-","")
    data$occ_code <-as.numeric(data$occ_code)
    
    # Filter for teachers
    data <- filter(data, occ_code %in% teacher_occ_codes, a_median != "*")
    
    # Replace NA of tot_emp to allow for weighted averages
    data$tot_emp[is.na(data$tot_emp)]<-1
    
    write_csv(data, "prev_teachers_salaries.csv")

}

prev_salaries <- read_csv("./BLSdata/prev_teachers_salaries.csv")

# Gather BLS state_code
state_code <- bls_areas$area_code[bls_areas$areatype_code=="S" &
                                      bls_areas$area_name==state_full]
# Area def available from:
# http://www.bls.gov/oes/2015/may/area_definitions_m2015.xls

# BLS Metro-county mapping
bls_area_def <- read_csv("~/Google Drive/PRTeam Analysis/Geo Setup/bls_area_definitions_m2015.csv")

# Clean up column names
cn <- gsub(" ", "_", colnames(bls_area_def))
colnames(bls_area_def)<- sapply(cn, tolower)

# Area def available from:
# http://www.bls.gov/oes/2012/may/area_definitions_m2012.xls
prev_bls_area_def <- read_csv("~/Google Drive/PRTeam Analysis/Geo Setup/bls_area_definitions_m2012.csv")

# Clean up column names
cn <- gsub(" ", "_", colnames(prev_bls_area_def))
colnames(prev_bls_area_def)<- sapply(cn, tolower)

# Function to subset BLS data by state
state_filter <- function(state_abb = "CA") {
    
    state_full <- as.vector(state_match$state.name[state.abb==state_abb])
    
    # Calculated weighted mean for salary & Subset for state
    state_salaries <- salaries %>%
        dplyr::group_by(area, area_title) %>%
        summarise(teacher_salary = weighted.mean(a_median, tot_emp, na.rm = T)) %>%
        dplyr::filter(grepl(paste0(state_abb,"|", state_full," nonmetropolitan"), area_title, ignore.case = F))
    
    state_median_salary <- salaries %>%
        filter(grepl(paste0(state_abb,"|", state_full," nonmetropolitan"), area_title, ignore.case = F)) %>%
        summarise(area = state_code, area_title = state_full, 
                  teacher_salary = weighted.mean(a_median, tot_emp, na.rm = T))
    
    prev_state_salaries <- prev_salaries %>%
        dplyr::group_by(area, area_title) %>%
        summarise(prev_teacher_salary = weighted.mean(a_median, tot_emp, na.rm = T)) %>%
        dplyr::filter(grepl(paste0(state_abb,"|", state_full," nonmetropolitan"), area_title, ignore.case = F))
    
    prev_state_median_salary <- prev_salaries %>%
        filter(grepl(paste0(state_abb,"|", state_full," nonmetropolitan"), area_title, ignore.case = F)) %>%
        summarise(area = state_code, area_title = state_full, 
                  prev_teacher_salary = weighted.mean(a_median, tot_emp, na.rm = T))
    
    
    bls_area_def <- bls_area_def %>% 
        filter(state == state_full) %>% 
        dplyr::select(-township_code, -msa_code_for_msas_with_divisions, -msa_name_for_msas_with_divisions)
    
    prev_bls_area_def <- prev_bls_area_def %>% 
        filter(state == state_full) %>% 
        dplyr::select(-township_code, -township_name, -aggregate_msa_code, -aggregate_msa_name)
    

    
    state_salaries_merged <- merge(state_salaries, bls_area_def, by.x = "area", by.y = "msa_code_(including_msa_divisions)", all.y = T)
    
    
    prev_state_salaries_merged <- merge(prev_state_salaries, prev_bls_area_def, by.x = "area", by.y = "msa_code_(with_divisions)", all.y = T)
    
    prev_state_salaries_merged <- merge(prev_state_salaries_merged, prev_state_median_salary, all = T)
    
    state_salaries_merged <- merge(state_salaries_merged, prev_state_salaries_merged[,c(3,7)])
    
    state_median_salary_m <- merge(state_median_salary, prev_state_median_salary)
    
    state_salaries_merged <- merge(state_salaries_merged, state_median_salary_m, all = T)
    
    state_salaries_merged <- state_salaries_merged %>%
        mutate(teacher_salary_pct_change = teacher_salary/prev_teacher_salary - 1)
    
    # clean names        
    colnames(state_salaries_merged)[10]<-"county_name"
    
    state_salaries_merged <- state_salaries_merged %>%
        dplyr::select(area_title, county_name, county_code, teacher_salary, prev_teacher_salary, teacher_salary_pct_change)
    
    return(state_salaries_merged)
}

# Call Function
state_salaries_merged <- state_filter(state_abb)

# Set interest rate
interest_rate = 0.035

# Calculate max monthly payment and max home price 
# using 30 percent affordability criteria
state_salaries_merged <- state_salaries_merged %>%
    mutate(max_payment = round((teacher_salary / 12) * 0.3 / 25) * 25,
           prev_max_payment = round((prev_teacher_salary / 12) * 0.3 / 25) * 25,
           max_home_price = round(((teacher_salary / 12) * 0.3 - 500)/( (interest_rate/12) + (interest_rate/12) /((1 + (interest_rate/12))^(30*12)-1))/10000) * 10000,
           prev_max_home_price = round(((prev_teacher_salary / 12) * 0.3 - 500)/( (interest_rate/12) + (interest_rate/12) /((1 + (interest_rate/12))^(30*12)-1))/10000) * 10000
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
                l.county_id, 
                c.display_name,
                coalesce(list_price, original_price) as list_price, 
                hoa_dues, 
                coalesce(case when t.taxes_due_properties is not null and t.taxable_value_total_properties is not null then t.taxes_due_properties/t.taxable_value_total_properties end, 0.01125) as prop_tax_rate,
                pt.name,
                num_bedrooms, 
                num_bathrooms, 
                approx_sq_ft,
                'http://www.redfin.com/'::text || upper(l.state_code)::text || '/'::text || regexp_replace(initcap(trim(l.city)), ' ', '-')::text || '/home/'::text || l.property_id::text as url
                from listings l
                left join rf_temp.dqv3_tax t on t.property_id=l.property_id
                join counties c on c.county_id=l.county_id
                join property_types pt on pt.property_type_id=l.property_type_id
                where listing_type_id = 1
                and search_status_id = 1
                and is_short_sale = 0
                and is_bank_owned = 0
                and l.property_type_id in (3,6,13)
                and coalesce(listing_date::date, listing_added_date::date) <= current_date-1
                and coalesce(listing_date::date, listing_added_date::date) > current_date-366
                and coalesce(off_market_date, sale_date) is null
                and l.county_id is not null
                and coalesce(list_price, original_price) >= 10000
                and state_code = '",state_abb,"'
                order by list_price desc;"
    )
    message(paste0("Getting listings for ",state_abb))
    
    listings <- dbGetQuery(rscon, statement = q)
    
    return(listings)
}

# Call function
listings <- get_active_listings(state_abb)

get_prev_listings <- function(state_abb = "CA") {
    if(!(exists("rscon"))){
        message("Loading database connections")
        suppressPackageStartupMessages(
            # Load in credentials and JDBC connection
            source("~/Google Drive/PRTeam Analysis/R Scripts/Connecting to Redfin db in R.R")
        )
    }
    q <- paste0("select listing_id, 
                l.county_id, 
                c.display_name,
                coalesce(list_price, original_price) as list_price, 
                hoa_dues, 
                coalesce(case when t.taxes_due_properties is not null and t.taxable_value_total_properties is not null then t.taxes_due_properties/t.taxable_value_total_properties end, 0.01125) as prop_tax_rate,
                pt.name,
                num_bedrooms, 
                num_bathrooms, 
                approx_sq_ft,
                'http://www.redfin.com/'::text || upper(l.state_code)::text || '/'::text || regexp_replace(initcap(trim(l.city)), ' ', '-')::text || '/home/'::text || l.property_id::text as url
                from listings l
                left join rf_temp.dqv3_tax t on t.property_id=l.property_id
                join counties c on c.county_id=l.county_id
                join property_types pt on pt.property_type_id=l.property_type_id
                where listing_type_id = 1
                --and search_status_id = 1
                and is_short_sale = 0
                and is_bank_owned = 0
                and l.property_type_id in (3,6,13)
                and coalesce(listing_date::date, listing_added_date::date) <= current_date-1462
                and coalesce(listing_date::date, listing_added_date::date) > current_date-1827
                and coalesce(off_market_date::date, sale_date::date, current_date) >= current_date-1462
                and l.county_id is not null
                and coalesce(list_price, original_price) >= 10000
                and state_code = '",state_abb,"'
                order by list_price desc;"
    )
    message(paste0("Getting previous listings for ",state_abb))
    
    listings <- dbGetQuery(rscon, statement = q)
    
    return(listings)
}

# Call function
prev_listings <- get_prev_listings(state_abb)

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

# Call function to calculate mortgage payments
prev_listings$monthly_mortgage <- as.vector(
    unlist(
        pmap(
            list(
                home_price = prev_listings$list_price, 
                hoa_dues = prev_listings$hoa_dues, #hoa dues by listing
                ann_property_taxes = prev_listings$prop_tax_rate, #using tax rate by county
                interest_rate = .035, #Use current interest rates
                down_payment = .1), #10% down payment assumption
            monthly_mortgage_calculator)))

# Join salary data, removing rows without county_names
listings <- merge(listings, state_salaries_merged, by.x = "display_name", by.y = "county_name")

# Join salary data, removing rows without county_names
prev_listings <- merge(prev_listings, state_salaries_merged, by.x = "display_name", by.y = "county_name")


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

# Calculate percent affordable previously by county
(prev_county_affordability <- prev_listings %>%
    group_by(display_name) %>%
    mutate(n_listings = n()) %>%
    summarise(affordable_listings = sum(
        if_else(
            monthly_mortgage <= prev_max_payment,1,0)),
        total_listings = median(n_listings,na.rm=T),
        prev_percent_affordable = affordable_listings/total_listings) %>%
    arrange(prev_percent_affordable))

# Get max payment at the state level
prev_state_max_payment <- state_salaries_merged$prev_max_payment[
    state_salaries_merged$area_title==state_full & 
        !(is.na(state_salaries_merged$area_title))]

# Calculate percent of listings affordable for the entire state
(prev_state_affordability <- prev_listings %>%
    mutate(n_listings = n()) %>%
    summarise(affordable_listings = sum(
        if_else(
            monthly_mortgage <= prev_state_max_payment,1,0)),
        total_listings = median(n_listings,na.rm=T),
        prev_percent_affordable = affordable_listings/total_listings) %>%
    arrange(prev_percent_affordable))

prev_state_affordability$area_title <- state_full

prev_county_affordability$affordable_listings[
    is.na(prev_county_affordability$affordable_listings) 
    & !is.na(prev_county_affordability$total_listings)]<-0

prev_county_affordability$prev_percent_affordable[
    is.na(prev_county_affordability$prev_percent_affordable) 
    & !is.na(prev_county_affordability$total_listings)]<-0

county_affordability <- merge(county_affordability, prev_county_affordability[,c(1,4)], all=T)

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

state_salaries_merged$prev_percent_affordable[
    state_salaries_merged$area_title==state_full]<-
    prev_state_affordability$prev_percent_affordable[
        prev_state_affordability$area_title==state_full]


state_salaries_merged<- state_salaries_merged %>%
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
    dplyr::select(display_name, teacher_salary, max_payment, max_home_price, url, description, county_id) %>%
    filter(!(is.na(display_name))) %>%
    arrange(display_name)

# Segment out unaffordable counties
unaffordable_counties <- state_salaries_merged[state_salaries_merged$affordable_listings==0 
                                               & !(is.na(state_salaries_merged$affordable_listings)),]

# Lookup county_ids
county_ids <- unique(listings[listings$display_name %in% unaffordable_counties$county_name,c(1,3)])

# Join in county_ids
unaffordable_counties <- merge(unaffordable_counties, county_ids, by.x="county_name", by.y="display_name", all=T)

for(u in seq_along(unaffordable_counties$county_name)){
    tbl0<-cbind.data.frame(
        display_name=as.character(unaffordable_counties$county_name[u]),
        teacher_salary=unaffordable_counties$teacher_salary[u],
        max_payment=unaffordable_counties$max_payment[u],
        max_home_price=unaffordable_counties$max_home_price[u],
        url=NA,
        description=as.character("No Affordable Listings"),
        county_id=unaffordable_counties$county_id[u], 
        stringsAsFactors = F
    )
    
    tbl0<-group_by(tbl0,display_name)
    
    # join to examples table
    l = list(tbl,tbl0)
    tbl<-rbindlist(l)
}

# Order by county name
tbl<-arrange(tbl,display_name)

# Clean County Name
tbl$display_name<-str_replace(tbl$display_name, " County","")

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
                   <td style='text-align: right' class='c3 '>",
                   ifelse(!is.na(tbl[i, 5, with = FALSE]),
                          paste0("<a href=", tbl[i, 5, with = FALSE], "?utm_source=blog&utm_medium=post&utm_content=real_estate&utm_campaign=1002170'>", tbl[i, 6, with = FALSE], "</a></td></tr>"),
                          paste0(tbl[i, 6, with = FALSE], "</td></tr>")
                   )
    )
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
state_salaries_merged<-state_salaries_merged[complete.cases(state_salaries_merged),]

# Geo join for mapping
state_salaries_geo <- geo_join(counties, state_salaries_merged, "COUNTYFP", "county_code")

# Make tooltip popup for map
popup <- paste0("County: <b>", state_salaries_geo$NAME, "</b><br>", 
                "Average Salary for Teachers: <b>", scales::dollar(round(state_salaries_geo$teacher_salary/1000,1)*1000), 
                "</b><br>Percent of listings affordable: <b>", scales::percent(round(state_salaries_geo$percent_affordable, 3)), "</b>",
                "<br>Percent of listings affordable in 2012: <b>", scales::percent(round(state_salaries_geo$prev_percent_affordable, 3)), "</b>"
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
