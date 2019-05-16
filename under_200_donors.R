#2020 committee report summary accessed 4/23 from https://classic.fec.gov/data/CampaignAndCommitteeSummary.do?format=html
#dollar signs were removed in Excel, becasue it's easier for me to do that.

library(tidyverse)

committee <- read_csv("CampaignAndCommitteeSummaryAction.csv")


#make new frame with columns I want/
lowest_unitemized <- committee %>% 
  #select the columns I want
  select(com_nam, rep_typ, com_des ,lin_ima, cit, sta, ind_ite_con, ind_uni_con,ind_con, tot_rec, par_com_con, can_loa ) %>%
  #rename them so it's easier to understand what the heck they are
  setNames(c("committee_name", "report_type", "committee_designation" ,"link", "city", "state", "over_200", "under_200","individual_contribution", "total_receipts", "party_committee_contrib", "candidate_loan")) %>%
  #filter out non Q1 reports and (P) principal campaign committee of a candidate;
  filter(total_receipts >= 100000 & report_type == "Q1" & committee_designation == "P") %>%
  #add a column that gives the percentage of total money raised by the amount of unitemized donations.
  mutate(pct_under_200 = (under_200/total_receipts)) %>%
  arrange(pct_under_200)
#give it to me in a CSV
#write_csv(lowest_unitemized, "lowest_unitemized.csv")




# these committees are in the lowest 16 list. Accessed on 4/24/2019 ---  HOYER FOR CONGRESS (C00140715)TEXANS FOR HENRY CUELLAR CONGRESSIONAL CAMPAIGN (C00371302)DAVID TRONE FOR CONGRESS, INC (C00653196)FRENCH HILL FOR ARKANSAS (C00551275)GRAVES FOR CONGRESS (C00359034)MARIO DIAZ-BALART FOR CONGRESS (C00376087)MCHENRY FOR CONGRESS (C00393629)KENNY MARCHANT FOR CONGRESS (C00393348)ROSS SPANO FOR CONGRESS (C00676668)JASON SMITH FOR CONGRESS (C00541862)CITIZENS FOR TURNER (C00373001)ANTHONY GONZALEZ FOR CONGRESS (C00654079)LANCE GOODEN FOR CONGRESS COMMITTEE (C00662601)COLE FOR CONGRESS (C00379735)BILLY LONG FOR CONGRESS (C00460063)MATSUI FOR CONGRESS (C00409219) MCKINLEY FOR CONGRESS (C00473132)RON WRIGHT FOR CONGRESS (C00662171)RICHMOND FOR CONGRESS (C00451336) AUSTIN SCOTT FOR CONGRESS INC (C00482737)



## I started with 16 and didn't change the name of the df...because I'm lazy
lowest16 <- read_csv("top_20.csv")




test <- lowest16 %>%
  group_by(committee_name, entity_type_desc)  %>%
  summarise(total = sum(contribution_receipt_amount))

#eliminates the scientific notation
options(scipen = 999)

#graphs the committees and shows where each candidate's money is coming from in a stacked graf..
test %>% 
  group_by(committee_name) %>% 
  #mutate to create a total of each committee's total. h/t to ABTRAN This will help sort the bars from least to greatest. 
  mutate(committee_total=sum(total)) %>% 
  #-total sorts less to most by committee name
  ggplot(aes(x= reorder(committee_name, -committee_total), y=total, fill= fct_rev(entity_type_desc))) +  
  #creats the stacked bar chart
  geom_col() + 
  #flips names to y axis
  coord_flip() +
  #removes axis label
  theme(axis.title = element_blank())+
  #sets legend title
  labs(fill = "Total Q1 Contributions")
  

