# Community Profiles and Indicators for Fairfax

# Datasets
-masterAddress_and_href.txt
    -./data/comm_fairfax/original/masterAddress_and_href.txt
    -This text file contains the address string and href (web address identifier) for every therapist, psychologist and                psychiatrist listed under Fairfax County on psychologytoday.com
    -Addresses were converted to lat-longs which were used for mapping onto the Fairfax County high school pyramid regions
    -Comes from ./src/benjs23/therapistsFairfaxAddress.R
    
-SAMSHA Behavioral_Health_Treatment_Facility_listing_2017_06_09_150743.xlsx.xlsx
    -./data/comm_fairfax/original/SAMSHA Behavioral_Health_Treatment_Facility_listing_2017_06_09_150743.xlsx.xlsx
    -This excel spreadsheet contains the listing for every substance abuse service, mental health service, and behavioral health      statistics center managed by the U.S. Department of Health and Human Services
    -The lat-longs for centers located in Fairfax County were used for plotting mental health providers onto the high school          pyramid regions

-fairfaxTherapistMaster.RData
    -./data/comm_fairfax/original/fairfaxTherapistMaster.RData
    -This dataset contains the the address string, lat-long, list of licenses and degrees held, client focus (age, sexual             oreintation, couples/family), and client group (0-adults, 1-children, 2-pre-teen, 3-teenager) for every therapist,               psychologist and psychiatrist listed under Fairfax County on psychologytoday.com.
    -A note about client group:
        -Since our project's focus is on teenagers' mental health problems, if the therapist focused on teenagers and any other           age, then he or she was categorized as a 3. Having teenaged focus excluded the therapist from the other groups.
    -Comes from ./src/benjs23/therapistsFairfaxAddress.R
    
    
#Figures
-./output/heatMapTherapist.png
    -Overlays location of mental health providers by client focus onto the high school pyramid boundary filled by percentage of       students reporting depressive symptoms.
    -Comes from ./src/benjs23/heatMap_Therapist.R
