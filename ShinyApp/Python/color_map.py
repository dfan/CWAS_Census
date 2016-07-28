import csv, os, sys, numpy as np
from bs4 import BeautifulSoup
from os.path import dirname

# dictionary with state names as key
population = {}
# dirname accesses parent directory
reader = csv.reader(open(dirname(dirname(os.getcwd())) + '/Data/County/' + sys.argv[1]), delimiter = ",")
# skip header
next(reader)
for row in reader:
    population[row[0]] = row[1]

svg = open('counties.svg', 'r').read()
# soup = BeautifulSoup(svg, selfClosingTags=['defs','sodipodi:namedview'])
soup = BeautifulSoup(svg, "xml")
# find all counties in the XML of the svg file
paths = soup.findAll('path')
colors = ["ffffff", "#fff5f0", "#fee0d2" ,"#fcbba1", "#fc9272", "#fb6a4a", "#ef3b2c", "#cb181d", "#a50f15", "#67000d"]

# replace style attribute for each path in SVG file
path_style = 'font-size:12px;fill-rule:nonzero;stroke:#FFFFFF;stroke-opacity:1; stroke-width:0.1;stroke-miterlimit:4;stroke-dasharray:none;stroke-linecap:butt; marker-start:none;stroke-linejoin:bevel;fill:'
# convert string list to floats
values = [float(i) for i in population.values()]

buckets = []
for i in range(1, 11):
    buckets.append(np.percentile(values, i * 10))

# loop through all paths and find population from the population dictionary, select color class
for p in paths:
    # don't want to change style of state lines or line that separates Hawaii and Alaska from continental US
    if p['id'] not in ["State_Lines", "separator"]:
        # pass
        try:
            pop = float(population[p['id']])
        except:
            continue
        if pop > buckets[9]:
            color_class = 9
        elif pop > buckets[8]:
            color_class = 8
        elif pop > buckets[7]:
            color_class = 7
        elif pop > buckets[6]:
            color_class = 6
        elif pop > buckets[5]:
            color_class = 5
        elif pop > buckets[4]:
            color_class = 4
        elif pop > buckets[3]:
            color_class = 3
        elif pop > buckets[2]:
            color_class = 2
        elif pop > buckets[1]:
            color_class = 1
        else:
            color_class = 0
        color = colors[color_class]
        p['style'] = path_style + color
print(soup.prettify())
