
# data access is not possible by an API but through an interactive dashboard (requiring js rendering) which limits the total amount of data exported in one time; the query was created and saved within the dashboard, and data was downloaded by chunks using browser automation using Selenium and its python API (see [code](https://github.com/JusteRaimbault/ABMCitiesFirms/tree/master/DataCollection)).


from selenium import webdriver

import time
import utils
import navigation

driver = navigation.initialize()

# try directly? -> does not work, need search
#navigation.wait_for_element(driver,"a","href","/version-2020423/fame/1/Companies/List")
#driver.get('https://fame4.bvdinfo.com/version-2020423/fame/1/Companies/List')

# go to saved search
navigation.wait_and_click(driver,"li","data-vs-value","load-search-section")
#popup
navigation.wait_and_click(driver,"div","class","wm-close-button walkme-x-button")
# retrieve first saved search
navigation.wait_for_element(driver,"ul","class","user-data-item-folder")
#navigation.click(driver.find_element_by_xpath("//ul[@class='user-data-item-folder']/li[1]//li[@data-ajax-submit='click:Search:LoadSearch']"))
#navigation.click(driver,driver.find_element_by_xpath("//ul[@class='user-data-item-folder']/li[1]//span[@class='name']")) # does not work
#navigation.click(driver,driver.find_element_by_xpath("//ul[@class='user-data-item-folder']/li[1]")) # does not work
navigation.click(driver,driver.find_element_by_xpath("//ul[@class='user-data-item-folder']/li[1]//span[@data-ajax-submit='click:Search:LoadSearch']"))

navigation.wait_for_element(driver,"td","class","search-step")

#navigation.click(driver,navigation.get_element(driver,"a","href","/version-2020423/fame/1/Companies/List"))
driver.get('https://fame4.bvdinfo.com/version-2020423/fame/1/Companies/List')

time.sleep(2)

# 12820 with Export view
for RANGE_FROM in [1,12001]:#[100,200,666]:

    # go to export
    navigation.wait_for_element(driver,"li","class","menuActions")
    navigation.click(driver,driver.find_element_by_xpath("//li[@class='menuActions']/a[1]"))
    navigation.wait_and_click(driver,"a","data-toolbar-action","records-export")

    time.sleep(2)
    # pop up, not external window
    #print(driver.window_handles)
    #driver.switch_to_window(driver.window_handles[1])
    #print("Switch to export window")

    # export parameters
    #RANGE_FROM="10000"
    #RANGE_TO="10010"
    #RANGE_TO = RANGE_FROM+9
    RANGE_TO = RANGE_FROM+11999
    print(str(RANGE_FROM)+"-"+str(RANGE_TO))

    # format
    navigation.select(driver,"select","id","component_FormatTypeSelectedId","Custom.list.txt")

    # range
    navigation.select(driver,"select","id","component_RangeOptionSelectedId","Range")
    navigation.wait_for_element(driver,"input","name","component.From")
    navigation.fill_field(driver,"input","name","component.From",RANGE_FROM)
    navigation.fill_field(driver,"input","name","component.To",RANGE_TO)
    navigation.fill_field(driver,"input","id","component_FileName","TEST2-"+str(RANGE_FROM)+"-"+str(RANGE_TO)+".tsv")

    navigation.wait_and_click(driver,"a","class","button submit ok")
    print("Waiting for file download")
    navigation.wait_for_element(driver,"a","class","button js-downloadLink")
    time.sleep(5)
    # close the popup
    navigation.wait_and_click(driver,"img","data-popup-close","cancel")
    time.sleep(10)

#    ## close the window: no need
#    #driver.close()
#    #driver.switch_to_window(driver.window_handles[0])
