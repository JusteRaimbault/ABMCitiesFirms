from selenium import webdriver

import time
import utils
import navigation

profile=webdriver.FirefoxProfile()
navigation.set_download_profile(profile)

driver = webdriver.Firefox(firefox_profile=profile)



# login
loginurl = utils.get_param('loginurl')
driver.get(loginurl)
navigation.fill_field(driver,"username",utils.get_param('username'))
navigation.fill_field(driver,"password",utils.get_param('password'))
navigation.click_validate(driver,"_eventId_proceed")
print("login successful")

# go to saved search
navigation.wait_and_click(driver,"ContentContainer1_ctl00_Content_QuickSearch1_ctl02_TabSavedSearches")

# retrieve first saved search
navigation.wait_and_click(driver,"ContentContainer1_ctl00_Content_QuickSearch1_ctl02_MySavedSearches1_DataGridResultViewer_ctl04_Linkbutton1")

for RANGE_FROM in [100,200]:

    # go to export
    navigation.wait_and_click(driver,"ContentContainer1_ctl00_Content_ListHeader_ListHeaderRightButtons_ExportButtons_ExportButton")
    time.sleep(5)
    #print(driver.window_handles)
    driver.switch_to_window(driver.window_handles[1])
    print("Switch to export window")

    # export parameters
    #RANGE_FROM="10000"
    #RANGE_TO="10010"
    navigation.fill_field(driver,"RANGEFROM",str(RANGE_FROM),False)
    navigation.fill_field(driver,"RANGETO",str(RANGE_FROM+9),False)
    navigation.fill_field(driver,"ctl00_ContentContainer1_ctl00_LowerContent_Formatexportoptions1_ExportDisplayName",str(RANGE_FROM+9))
    navigation.select(driver,"exportformat","UTF16Delimited")

    navigation.wait_and_click(driver,"imgBnOk")
    print("Waiting for file download")
    navigation.wait_for_id(driver,"DownloadPanel")
    time.sleep(10)

    # close the window
    driver.close()
    driver.switch_to_window(driver.window_handles[0])
