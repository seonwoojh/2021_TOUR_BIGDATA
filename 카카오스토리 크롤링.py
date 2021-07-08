# 필요한 라이브러리
from selenium.webdriver.common.action_chains import ActionChains
import requests
from bs4 import BeautifulSoup
import pymysql
from selenium import webdriver
from selenium.webdriver.common.keys import Keys
import time as time
import getpass
import urllib.request
import random
import xlsxwriter
from time import sleep
import datetime as dt
import pandas as pd
import numpy as np

# Driver 창 켜기

driver = webdriver.Chrome("C:/Users/Stat1316_02/Desktop/naverband/chromedriver.exe") # Chromedriver PATH
driver.get("https://story.kakao.com/")
driver.maximize_window()
time.sleep(3)

username = 'tjswl950@naver.com'
password = 'xhdrP123@'

element_id = driver.find_element_by_name("email")
element_id.send_keys(username)
element_password = driver.find_element_by_name("password")
element_password.send_keys(password)

time.sleep(1.5)

##로그인버튼 클릭
driver.find_element_by_css_selector('._loginBoxLoginBtn.btn_login').click()

time.sleep(3)

# --------------------- 로그인까지 완료 2021-07-08

# 해시태그 검색창 들어가기
keyword = input('검색할 단어를 입력하세요 : ')  # Search #

url = "https://story.kakao.com/hashtag/{}/".format(keyword)

driver.get(url)
time.sleep(5)


# 첫번째 게시글 클릭하기 탭 엔터 esc 활용
# driver.find_element_by_css_selector('.img_thumb._img').click()

actions = ActionChains(driver)

element = driver.find_element_by_css_selector('.img_thumb._img')
hover = actions.move_to_element(element).click()
hover.perform()
time.sleep(3)
# driver.find_element_by_css_selector('._btnClose').click()
# time.sleep(1.5)
# # hover.key_down(Keys.ENTER)
# hover.move_to_element_with_offset(element, 50, 2).click()
# ###
# # time.sleep(5)
# # hover.key_down(Keys.ESCAPE)

# 카카오스토리 크롤링(제발)

seq = 0
start = time.time()
##데이터를 저장할 Dictionary
insta_dict = {'id': [],'date': []}
if driver.find_element_by_css_selector('._btnClose'):
    if seq % 20 == 0:
        print('{}번째 수집 중'.format(seq), time.time() - start, sep='\t')
        driver.implicitly_wait(5)
        info_id = driver.find_element_by_css_selector('a.pf_name').text
        insta_dict['id'].append(info_id)
        driver.implicitly_wait(5)
        time_raw = driver.find_element_by_css_selector('span.time').text # 인스타 날짜형식 따오는거 그대로 쓰기
        insta_dict['date'].append(time_raw)
        driver.implicitly_wait(5)
        seq += 1
        if seq == 1:
            df = pd.DataFrame.from_dict(insta_dict)
            df.to_excel('C:/Users/Stat1316_02/Desktop/naverband/' + keyword + "_results.xlsx")



    #
    #         ## like 정보 수집
    #         try:
    #             driver.implicitly_wait(5)
    #             driver.find_element_by_css_selector('button.sqdOP.yWX7d._8A5w5')
    #             like = driver.find_element_by_css_selector('button.sqdOP.yWX7d._8A5w5').text
    #             insta_dict['like'].append(like)
    #             driver.implicitly_wait(5)
    #
    #         except:
    #             insta_dict['like'].append('영상')
    #
    #         ##text 정보수집
    #         driver.implicitly_wait(5)
    #         raw_info = driver.find_element_by_css_selector('div.C4VMK').text.split()
    #         text = []
    #         for i in range(len(raw_info)):
    #             ## 첫번째 text는 아이디니까 제외
    #             if i == 0:
    #                 pass
    #             ## 두번째부터 시작
    #             else:
    #                 if '#' in raw_info[i]:
    #                     pass
    #                 else:
    #                     text.append(raw_info[i])
    #         clean_text = ' '.join(text)
    #         insta_dict['text'].append(clean_text)
    #         driver.implicitly_wait(5)
    #
    #         ##hashtag 수집
    #         driver.implicitly_wait(5)
    #         raw_tags = driver.find_elements_by_css_selector('a.xil3i')
    #         hash_tag = []
    #         for i in range(len(raw_tags)):
    #             if raw_tags[i].text == '':
    #                 pass
    #             else:
    #                 hash_tag.append(raw_tags[i].text)
    #
    #         insta_dict['hashtag'].append(hash_tag)
    #