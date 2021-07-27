# 필요한 라이브러리

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
import matplotlib.pyplot as plt

# Driver창 켜기

driver = webdriver.Chrome("C:/Users/Stat1305_01/Desktop/인스타그램 크롤링/chromedriver.exe") # Chromedriver PATH
driver.get("https://www.instagram.com/accounts/login/")
driver.maximize_window()
time.sleep(3)


# 인스타그램 로그인

username = ''  # User ID
password = ''  # User PWD, 특히 getpass를 통해서 비밀번호 정보를 숨길 수도 있다. 잘 배워두자
keyword = input('검색할 단어를 입력하세요 : ')  # Search #
element_id = driver.find_element_by_name("username")
element_id.send_keys(username)
element_password = driver.find_element_by_name("password")
element_password.send_keys(password)

sleep(1.5)

##로그인버튼 클릭
driver.find_element_by_css_selector('.sqdOP.L3NKy.y3zKF').click()

time.sleep(3)

#로그인 무시

driver.find_element_by_css_selector('button.sqdOP.yWX7d.y3zKF').click()
time.sleep(3)


# 알림설정 무시
driver.find_element_by_css_selector('button.aOOlW.HoLwm').click()
time.sleep(3)

# --------------------- 로그인/ 알람무시까지 완료 --------------------

# 해시태그 검색창 들어가기
url = "https://www.instagram.com/explore/tags/{}/".format(keyword)
driver.get(url)
time.sleep(15)

# 맨 왼쪽 상단 첫 게시물 클릭
driver.find_element_by_css_selector('div.v1Nh3.kIKUG._bz0w').click()
time.sleep(3)







# 인스타그램 크롤링

seq = 0
start = time.time()
##데이터를 저장할 Dictionary
insta_dict = {'id': [],'date': [],'like': [],'text': [],'hashtag': []}
while True:
    try:
        if driver.find_element_by_css_selector('a._65Bje.coreSpriteRightPaginationArrow'):
            if seq % 20 == 0:
                print('{}번째 수집 중'.format(seq), time.time() - start, sep='\t')

            ## id 정보 수집
            try:
                driver.implicitly_wait(5)
                info_id = driver.find_element_by_css_selector('h2._6lAjh').text
                insta_dict['id'].append(info_id)
                driver.implicitly_wait(5)
            except:
                driver.implicitly_wait(5)
                info_id = driver.find_element_by_css_selector('div.C4VMK').text.split()[0]
                insta_dict['id'].append(info_id)
                driver.implicitly_wait(5)

            ## 시간정보 수집
            driver.implicitly_wait(5)
            time_raw = driver.find_element_by_css_selector('time.FH9sR.Nzb55')
            time_info = pd.to_datetime(time_raw.get_attribute('datetime')).normalize()
            insta_dict['date'].append(time_info)
            driver.implicitly_wait(5)

            ## like 정보 수집
            try:
                driver.implicitly_wait(5)
                driver.find_element_by_css_selector('button.sqdOP.yWX7d._8A5w5')
                like = driver.find_element_by_css_selector('button.sqdOP.yWX7d._8A5w5').text
                insta_dict['like'].append(like)
                driver.implicitly_wait(5)

            except:
                insta_dict['like'].append('')

            ##text 정보수집
            driver.implicitly_wait(5)
            raw_info = driver.find_element_by_css_selector('div.C4VMK').text.split()
            text = []
            for i in range(len(raw_info)):
                ## 첫번째 text는 아이디니까 제외
                if i == 0:
                    pass
                ## 두번째부터 시작
                else:
                    if '#' in raw_info[i]:
                        pass
                    else:
                        text.append(raw_info[i])
            clean_text = ' '.join(text)
            insta_dict['text'].append(clean_text)
            driver.implicitly_wait(5)

            ##hashtag 수집
            driver.implicitly_wait(5)
            raw_tags = driver.find_elements_by_css_selector('a.xil3i')
            hash_tag = []
            for i in range(len(raw_tags)):
                if raw_tags[i].text == '':
                    pass
                else:
                    hash_tag.append(raw_tags[i].text)

            insta_dict['hashtag'].append(hash_tag)

            seq += 1

            if seq == 1000:
                break

            time.sleep(15)
            driver.find_element_by_css_selector('a._65Bje.coreSpriteRightPaginationArrow').click()
            driver.implicitly_wait(5)


        else:
            break

    except:
        time.sleep(30)
        driver.find_element_by_css_selector('a._65Bje.coreSpriteRightPaginationArrow').click()
        time.sleep(2)




df = pd.DataFrame.from_dict(insta_dict)
df['date'] = df['date'].dt.tz_convert(None)
df.to_excel('C:/Users/Stat1305_01/Desktop/red'+ keyword + "_results.xlsx")
