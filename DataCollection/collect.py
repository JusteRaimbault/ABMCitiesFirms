
import time
import utils
import navigation


#######
# Data collection strategy:
#  - generate range file
#  - set download folder in params

driver = navigation.initialize()

remaining=True

while remaining:
    RANGE_STR = utils.read_from_file(utils.get_param('rangefile'))
    if RANGE_STR is not None:
        try:
            RANGE_FROM = RANGE_STR.split("-")[0]
            RANGE_TO = RANGE_STR.split("-")[1]
            print("Getting range "+RANGE_STR)

            # go to export
            navigation.wait_and_click(driver,"ContentContainer1_ctl00_Content_ListHeader_ListHeaderRightButtons_ExportButtons_ExportButton")
            time.sleep(5)
            #print(driver.window_handles)
            driver.switch_to_window(driver.window_handles[1])
            print("Switch to export window")

            # export parameters
            navigation.wait_for_id(driver,"RANGEFROM",1000,False)
            navigation.fill_field(driver,"RANGEFROM",str(RANGE_FROM),False)
            navigation.fill_field(driver,"RANGETO",str(RANGE_TO),False)
            navigation.fill_field(driver,"ctl00_ContentContainer1_ctl00_LowerContent_Formatexportoptions1_ExportDisplayName",RANGE_STR+"_"+str(int(time.time())))
            navigation.select(driver,"exportformat","UTF16Delimited")

            navigation.wait_and_click(driver,"imgBnOk")
            print("Waiting for file download")
            navigation.wait_for_id(driver,"DownloadPanel")
            time.sleep(30) # is it necessary while downlload occur / will this be enough time for large files?

            # close the window
            driver.close()
            driver.switch_to_window(driver.window_handles[0])
        except Exception as e:
            print('Exception : '+str(e))
            print('Sleeping for a while')
            utils.add_to_file(RANGE_STR,utils.get_param('rangefile'))
            driver.quit()
            time.sleep(600) # 10 minutes
            driver = navigation.initialize()
    else:
        remaining = False
