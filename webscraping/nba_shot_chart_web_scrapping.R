library(RSelenium)
library(XML)

rD <- rsDriver(browser = 'firefox')
renDr <- rD[['client']]

shot_chart_web_scrape <- function(player){
  
  renDr$navigate("https://stats.nba.com/")
  Sys.sleep(3)
  searchbox <- renDr$findElement(using = "css selector", value = ".landing-search__input")
  searchbox$sendKeysToElement(list(player))
  searchbox$sendKeysToElement(list(key = "down_arrow"))
  searchbox$sendKeysToElement(list(key = "enter"))
  
  Sys.sleep(5)
  
  selectall <- renDr$findElement(using = "xpath", value = "/html/body/main/div[2]/div/div[2]/div/div/nba-stat-table/div[1]/div/div/select")
  
  selectall$sendKeysToElement(list("Andrew Wiggins"))
  
  Sys.sleep(1)
  
  gettable <- renDr$findElement(using = "css selector", value = ".nba-stat-table__overflow > table:nth-child(1)")
  
  Sys.sleep(6)
  
  webElem5txt <- gettable$getElementAttribute("outerHTML")[[1]]
  table <- readHTMLTable(webElem5txt, header=TRUE, as.data.frame=TRUE)[[1]]
  
  return(table)
}

# scraping shot chat data
tg_shot_chart <- shot_chart_web_scrape("Treveon Graham")
jm_shot_chart <- shot_chart_web_scrape("Jordan McLaughlin")
kbd_shot_chart <- shot_chart_web_scrape("Keita Bates-Diop")
jb_shot_chart <- shot_chart_web_scrape("Jordan Bell")
#jv_shot_chart <- shot_chart_web_scrape("Jarred Vanderbilt")
je_shot_chart <- shot_chart_web_scrape("Jacob Evans")
nr_shot_chart <- shot_chart_web_scrape("Naz Reid")
km_shot_chart <- shot_chart_web_scrape("Kelan Martin")
ac_shot_chart <- shot_chart_web_scrape("Allen Crabbe")
gd_shot_chart <- shot_chart_web_scrape("Gorgui Dieng")
nv_shot_chart <- shot_chart_web_scrape("Noah Vonleh")
jn_shot_chart <- shot_chart_web_scrape("Jaylen Nowell")
aw_shot_chart <- shot_chart_web_scrape("Andrew Wiggins")
kat_shot_chart <- shot_chart_web_scrape("Karl-Anthony Towns")
mb_shot_chart <- shot_chart_web_scrape("Malik Beasley")
dar_shot_chart <- shot_chart_web_scrape("D'Angelo Russell")
rc_shot_chart <- shot_chart_web_scrape("Robert Covington")
jh_shot_chart <- shot_chart_web_scrape("Juan Hernangomez")
jt_shot_chart <- shot_chart_web_scrape("Jeff Teague")
jo_shot_chart <- shot_chart_web_scrape("Josh Okogie")
jj_shot_chart <- shot_chart_web_scrape("James Johnson")
jc_shot_chart <- shot_chart_web_scrape("Jarrett Culver")
sn_shot_chart <- shot_chart_web_scrape("Shabazz Napier")
jl_shot_chart <- shot_chart_web_scrape("Jake Layman")

final_shot_df <- rbind(ac_shot_chart, 
                       gd_shot_chart, jb_shot_chart, 
                        jh_shot_chart,  
                        jm_shot_chart, jn_shot_chart, 
                       km_shot_chart, mb_shot_chart, 
                      rc_shot_chart, 
                       sn_shot_chart)
library(readr)
write_csv(final_shot_df, "Documents/gitrepos/nba_shot_chart_data_1.csv")


  
  
  
  
  