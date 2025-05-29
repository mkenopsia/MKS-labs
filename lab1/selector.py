import csv
import random

with open('/home/kaio/MKS-labs-R/lab1/input.csv', 'r', newline='', encoding='UTF-8') as input_file:
    reader = csv.reader(input_file)
    header = next(reader)
    data = list(reader)

study_level = header.index('УровеньПодготовки')
grade1 = header.index('Оценка1')
grade2 = header.index('Оценка2')
grade3 = header.index('Оценка3')

filtered_data = [
    row for row in data 
    if row[study_level] == 'Бакалавриат'
    and row[grade1] not in ('', ' ', None)
    and row[grade2] not in ('', ' ', None)
    and row[grade3] not in ('', ' ', None)
]

random_sampled_data = random.sample(filtered_data, 200)

with open('/home/kaio/MKS-labs-R/lab1/data.csv', 'w', newline='', encoding='UTF-8') as output_file:
    writer = csv.writer(output_file)
    writer.writerow(header)
    writer.writerows(random_sampled_data)