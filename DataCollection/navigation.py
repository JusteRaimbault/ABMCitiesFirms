from selenium.webdriver.common import action_chains
from selenium.webdriver.common.keys import Keys
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import Select

import utils

def set_download_profile(profile):
    profile.set_preference('browser.download.folderList', 2)
    profile.set_preference('browser.download.manager.showWhenStarting', False)
    profile.set_preference('browser.download.dir', utils.get_param('downloaddir'))
    profile.set_preference('browser.helperApps.neverAsk.openFile', 'text/csv,text/plain')
    profile.set_preference('browser.helperApps.neverAsk.saveToDisk', 'text/csv,text/plain')
    profile.set_preference('browser.helperApps.alwaysAsk.force', False)
    profile.set_preference('browser.download.manager.alertOnEXEOpen', False)
    profile.set_preference('browser.download.manager.focusWhenStarting', False)
    profile.set_preference('browser.download.manager.useWindow', False)
    profile.set_preference('browser.download.manager.showAlertOnComplete', False)
    profile.set_preference('browser.download.manager.closeWhenDone', False)



def initialize():
    """
    Reset the driver and connection
    """
    profile=webdriver.FirefoxProfile()
    set_download_profile(profile)

    driver = webdriver.Firefox(firefox_profile=profile)

    # login
    loginurl = utils.get_param('loginurl')
    driver.get(loginurl)
    fill_field(driver,"username",utils.get_param('username'))
    fill_field(driver,"password",utils.get_param('password'))
    click_validate(driver,"_eventId_proceed")
    print("login successful")

    # go to saved search
    wait_and_click(driver,"ContentContainer1_ctl00_Content_QuickSearch1_ctl02_TabSavedSearches")
    print("Saved searches")

    # retrieve first saved search
    wait_and_click(driver,"ContentContainer1_ctl00_Content_QuickSearch1_ctl02_MySavedSearches1_DataGridResultViewer_ctl04_Linkbutton1")
    print("First saved search")
    # reload columns -> edit columns, saved lists ? OK default columns saved
    return(driver)



def fill_field(driver,id,value,by_id = True):
    if by_id:
        elem = driver.find_element_by_id(id)
        elem.clear()
        elem.send_keys(value)
    else: # else by name
        elem = driver.find_element_by_name(id)
        elem.clear()
        elem.send_keys(value)

def click_validate(driver,id):
    driver.find_element_by_name(id).send_keys(Keys.ENTER)


def wait_for_id(driver,id,timeout=1000):
    WebDriverWait(driver, timeout).until(expected_conditions.presence_of_element_located((By.ID, id)))


def wait_and_click(driver, id,timeout=1000):
    wait_for_id(driver,id,timeout)
    action_chains.ActionChains(driver).click(driver.find_element_by_id(id)).perform()

def select(driver,id,value):
    Select(driver.find_element_by_id(id)).select_by_value(value)
