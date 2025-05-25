import csv
import random

with open('/home/kaio/MKS-labs-R/lab1/input.csv', 'r', newline='', encoding='UTF-8') as input_file:
    reader = csv.reader(input_file)
    header = next(reader)
    data = list(reader)

random_sampled_data = random.sample(data, 200)

with open('/home/kaio/MKS-labs-R/lab1/data.csv', 'w', newline='', encoding='UTF-8') as output_file:
    writer = csv.writer(output_file)
    writer.writerow(header)
    writer.writerows(random_sampled_data)