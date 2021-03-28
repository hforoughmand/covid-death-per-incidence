import sys, csv, re, datetime
import numpy as np

file_names = {"case": "data/time_series_covid19_confirmed_global.csv", "recover": "data/time_series_covid19_recovered_global.csv", "death": "data/time_series_covid19_deaths_global.csv"}
types = ["case", "recover", "death"]
ROW_CASE, ROW_RECOVER, ROW_DEATH = 0, 1, 2

data_raw = {}
for n, fn in file_names.items():
	with open(fn, 'r') as f:
		r = csv.DictReader(f, delimiter=',', quotechar='|')
		for row in r:
			if row["Country/Region"] == 'Iran':
				data_raw[n] = { datetime.datetime.strptime(d, "%m/%d/%y").date() : int(v) for d,v in row.items() if re.match('\d+/\d+/\d+', d) }

first_date = max([min(l.keys()) for n, l in data_raw.items()])
n = len(data_raw["case"])
data = np.zeros((3, n))
for i, t in enumerate(types):
	for j in range(n):
		data[i,j] = data_raw[t][first_date + datetime.timedelta(days=j)]

pass_days = 30
first_date = first_date + datetime.timedelta(days=pass_days)
n -= pass_days
data = data[:,pass_days:]

#for k, l in data.items():
	#print("{}: {}".format(k, sorted(l.keys())))
	#print("{}: {}".format(k, l))

#FIT THE FIRST MODEL:
def estimate_infection_to_death():
	fitness_min = 1e10
	l = n - 120
	for d in range(120):
		dd = data[ROW_DEATH, d:(d+l)] / data[ROW_CASE, 0:(l)]
		fatality_ratio = np.mean(dd)
		fitness = np.var(dd)
		print(d, fatality_ratio, fitness)
		if fitness < fitness_min:
			fatality_ratio_min, infection_to_death_min, fitness_min = fatality_ratio, d, fitness
	return infection_to_death_min, fatality_ratio_min
		

#days_infection_to_death = 23
#infection_fatality_ratio = 100



