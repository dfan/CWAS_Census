import os

script_dir = os.path.dirname(__file__) #<-- absolute dir the script is in
rel_path = 'Data/ACS_2010.csv'
abs_file_path = os.path.join(script_dir, rel_path)
file = open(abs_file_path, 'r+')
#rel_path = 'Data/test.csv' 
#abs_file_path = os.path.join(script_dir, rel_path)
# very important
data = file.readlines()
file.close()

new = open(abs_file_path, 'w')

for line in data:
    params = line.rstrip('\n').split(',')
    # get rid of n/a values (when county has 0 population)
    if params[2] == '#DIV/0!':
        params[2] = 0
    if params[3] == '#DIV/0!':
        params[3] = 0
    # states end with 000. We don't want states; only counties
    if params[0][2:6] != '000':
        for i in range(0, len(params) - 1):
            new.write(str(params[i]) + ',')
        # no comma on last item
        new.write(str(params[len(params) - 1]) + '\n')
