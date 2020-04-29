
import time, utils, navigation, numpy


#######
# Data collection strategy:
#  - generate range file
#  - set download folder in params


def initialize():
    driver = navigation.initialize()
    # go to saved search
    navigation.wait_and_click(driver,"li","data-vs-value","load-search-section")
    navigation.wait_and_click(driver,"div","class","wm-close-button walkme-x-button")
    print("Saved searches")
    # retrieve first saved search
    navigation.wait_for_element(driver,"ul","class","user-data-item-folder")
    navigation.click(driver,driver.find_element_by_xpath("//ul[@class='user-data-item-folder']/li[1]//span[@data-ajax-submit='click:Search:LoadSearch']"))
    print("First saved search")
    navigation.wait_for_element(driver,"td","class","search-step")
    driver.get(utils.get_param('collecturl'))
    return(driver)

driver = initialize()

remaining=True

while remaining:
    RANGE_STR = utils.read_from_file(utils.get_param('rangefile'))
    if RANGE_STR is not None:
        try:
            RANGE_FROM = RANGE_STR.split("-")[0]
            RANGE_TO = RANGE_STR.split("-")[1]
            print("Getting range "+RANGE_STR)

            # go to export
            navigation.wait_for_element(driver,"li","class","menuActions")
            navigation.click(driver,driver.find_element_by_xpath("//li[@class='menuActions']/a[1]"))
            navigation.wait_and_click(driver,"a","data-toolbar-action","records-export")
            time.sleep(2+numpy.random.randint(8))
            print("Switch to export")

            navigation.wait_for_element(driver,"select","id","component_FormatTypeSelectedId")
            # format
            navigation.select(driver,"select","id","component_FormatTypeSelectedId","Custom.list.txt")
            navigation.select(driver,"select","id","component_RangeOptionSelectedId","Range")
            navigation.wait_for_element(driver,"input","name","component.From")
            navigation.fill_field(driver,"input","name","component.From",RANGE_FROM)
            navigation.fill_field(driver,"input","name","component.To",RANGE_TO)
            navigation.fill_field(driver,"input","id","component_FileName",RANGE_STR+"_"+str(int(time.time())))

            navigation.wait_and_click(driver,"a","class","button submit ok")
            print("Waiting for file download")
            navigation.wait_for_element(driver,"a","class","button js-downloadLink")
            time.sleep(10+numpy.random.randint(15))
            # close the popup
            navigation.wait_and_click(driver,"img","data-popup-close","cancel")
            time.sleep(5+numpy.random.randint(5))

        except Exception as e:
            print('Exception : '+str(e))
            print('Sleeping for a while')
            utils.add_to_file(RANGE_STR,utils.get_param('rangefile'))
            driver.quit()
            time.sleep(600) # 10 minutes
            driver = initialize()
    else:
        remaining = False
