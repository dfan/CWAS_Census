    import os
import sys

script_dir = os.path.dirname(__file__) #<-- absolute dir the script is in
rel_path = 'Data/' + sys.argv[1]
abs_file_path = os.path.join(script_dir, rel_path)
file = open(abs_file_path, 'r+')
# very important
data = file.readlines()
file.close()

new = open(abs_file_path, 'w')

def sumSubList(params):
    sum = 0
    for i in range(1, len(params)):
        sum += float(params[i])
    return sum

for line in data:
    # Make sure last token doesn't have carriage return or else #DIV/0 values at the end won't be removed
    params = line.rstrip('\r\n').split(',')
    # get rid of N/A values (when county has 0 population)
    # skip first element since it's the county code
    for i in range(1, len(params)):
        if params[i] == '#DIV/0!':
            params[i] = 0

    # states end with 000. We don't want states; only counties.
    # Keep counties even if they have no data. But get rid of 30113, 51560 and 51780 since they became incorporated as towns
    if params[0] == 'STCOU' or (params[0][2:6] != '000' and params[0] != '30113' and params[0] != '51560' and params[0] != '51780' and params[0] != 'STCOU'):
        for i in range(0, len(params) - 1):
            new.write(str(params[i]) + ',')
        # no comma on last item
        new.write(str(params[len(params) - 1]) + '\n')
