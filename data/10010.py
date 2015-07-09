#! /usr/bin/env python   
# -*- coding: utf-8 -*-
import urllib2
import json
import sys
reload(sys)
sys.setdefaultencoding("utf-8")

provinces = open('province.json')
provinces = json.load(provinces)

cities = open('city.json')
cities = json.load(cities)

def getbroadbandBespeakList(province_code, city_code):
	cookie = 'mallcity=%s|%s; _n3fa_cid=481fef7aa4e4474881a546301a3120f6; _n3fa_ext=ft=1428377198; _n3fa_lvt_a9e72dfe4a54a20c3d6e671b3bad01d9=1435644503,1435646539; WT_FPC=id=26b0041330c26e057911428377201177:lv=1435644504470:ss=1435644504455; __utma=231252639.809609691.1428377202.1429605722.1435644506.4; __utmz=231252639.1428377202.1.1.utmcsr=(direct)|utmccn=(direct)|utmcmd=(none); __utmv=231252639.Guangdong; ang_sessionid=305727611893117752; ang_seqid=6; ang_catchyou=1; _n3fa_lpvt_a9e72dfe4a54a20c3d6e671b3bad01d9=1435646539; __utmc=231252639'
	opener = urllib2.build_opener()
	opener.addheaders.append(('Cookie', cookie % (province_code, city_code)))
	f = opener.open("http://www.10010.com/mall-web/MBroadbandBespeak/reload")
	content=f.read()
	content=json.loads(content)
	if content.has_key('broadbandBespeakList'):
		broadbandBespeakList = content['broadbandBespeakList']
	else:
		broadbandBespeakList = []
	return broadbandBespeakList

def printBroadband(province, city_name, broadbandBespeakList):
	for broadband in broadbandBespeakList:
		(TARIFF_TYPE, PRICE, SPEED, PRODUCT_NAME, BROADBAND_ID) = broadband.values()
		row = [province['name'], city_name, TARIFF_TYPE, PRICE, SPEED, PRODUCT_NAME, BROADBAND_ID]
		row = [str(column) for column in row]
		print "\t".join(row)


print "\t".join(['PROVINCE', 'CITY', 'TARIFF_TYPE', 'PRICE', 'SPEED', 'PRODUCT_NAME', 'BROADBAND_ID'])

for province in provinces[9:]:
	province_name = province['name']
	province_code = str(province['code'])
	for city in cities[province_code]:
		if city.has_key('name'):
			continue
		(city_code, city_name) = city.items()[0]
		broadbandBespeakList = getbroadbandBespeakList(province_code, city_code)
		printBroadband(province, city_name, broadbandBespeakList)
