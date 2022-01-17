#!/bin/bash

# 184 Lärare
# 195 Lokal
# 205 studentgrupp
# 212 Undervisningstype
# 219 Kurs
type=219

search=TDDD98

base=https://cloud.timeedit.net/liu/web/schema

identVirtual=$(curl "${base}/objects.json?l=sv_SE&search_text=${search}&types=${type}" \
	| jq -r '.records[0].identVirtual')

start=$(date +%Y%m%d -d 2022-01-01)
end=$(date +%Y%m%d -d 2022-12-31)

# data="sid=3&p=${start}.x,${end}.x&objects=${identVirtual}&pp=f&e=2201"
# "p=4" means 'rullande 4 veckor'
data="p=${start},${end}&objects=${identVirtual}"
scrambled=$(./te_scramble --scramble "$data")
curl "${base}/${scrambled}.ics"


# ri627QyQYo8Zy2Q5Q96n54Z1yQZZ696605
# sid=3&p=2&objects=677455.219&pp=f&e=2201



# 2022-01-17 - 2022-08-14
# TDDD20 - id 677455
# ri65Z636X82Z04Q6Z86g2YZ0yQ096Y59y06gQY6Q5571o5y6Qn0265
# unscrambled 'sid=3&p=20220117.x,20220814.x&objects=677455.219&pp=f&e=2201'
# sid: 3
# p: 20220117.x,20220814.x
# objects: 677455.219
# pp: f
# e: 2201
#
# sid: 3
# p: ${start}.x,${end}.x
# objects: ${record.id}.${record.type.id}
# objects: ${identVirtual}
# p: ???
# e: ???



# sid=3
# p=0.d,20220123.x			# idag - 2022-01-23
# objects=642215.219,642348 # lista av object-idn. Tydligen är inte typ obligatorisk?
# e=2108

