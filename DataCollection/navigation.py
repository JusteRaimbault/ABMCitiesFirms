from selenium import webdriver
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
    fill_field(driver,"input","id","username",utils.get_param('username'))
    fill_field(driver,"input","id","password",utils.get_param('password'))
    click_validate(driver,"button","name","_eventId_proceed")
    print("login successful")

    return(driver)


def get_element(driver,element,attribute,attribute_value):
    return(driver.find_element_by_xpath("//"+element+"[@"+attribute+"='"+attribute_value+"']"))

def fill_field(driver,element,attribute,attribute_value,value):
    elem = get_element(driver,element,attribute,attribute_value)
    elem.clear()
    elem.send_keys(value)


def click_validate(driver,element,attribute,attribute_value):
    get_element(driver,element,attribute,attribute_value).send_keys(Keys.ENTER)

#'
#' by: By.ID, By.NAME
def wait_for_element(driver,element,attribute,attribute_value,timeout=1000):
    WebDriverWait(driver, timeout).until(expected_conditions.presence_of_element_located((By.XPATH, "//"+element+"[@"+attribute+"='"+attribute_value+"']")))

def wait_for_clickable(driver,element,attribute,attribute_value,timeout=1000):
    WebDriverWait(driver, timeout).until(expected_conditions.element_to_be_clickable((By.XPATH, "//"+element+"[@"+attribute+"='"+attribute_value+"']")))

def click(driver,element):
    action_chains.ActionChains(driver).click(element).perform()

def wait_and_click(driver, element,attribute,attribute_value,timeout=1000):
    wait_for_element(driver,element,attribute,attribute_value,timeout)
    click(driver,get_element(driver,element,attribute,attribute_value))

def select(driver,element,attribute,attribute_value,value):
    Select(get_element(driver,element,attribute,attribute_value)).select_by_value(value)
