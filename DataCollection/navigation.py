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


def fill_field(driver,id,value,by_id = True):
    if by_id:
        driver.find_element_by_id(id).send_keys(value)
    else: # else by name
        driver.find_element_by_name(id).send_keys(value)

def click_validate(driver,id):
    driver.find_element_by_name(id).send_keys(Keys.ENTER)


def wait_and_click(driver, id):
    WebDriverWait(driver, 10).until(expected_conditions.presence_of_element_located((By.ID, id)))
    action_chains.ActionChains(driver).click(driver.find_element_by_id(id)).perform()

def select(driver,id,value):
    Select(driver.find_element_by_id(id)).select_by_value(value)
