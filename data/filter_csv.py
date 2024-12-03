import csv
import re

input_file = 'accepted_words.csv'
output_file = 'filtered_accepted_words.csv'

with open(input_file, 'r') as csv_in:
    reader = csv.reader(csv_in)
    
    with open(output_file, 'w', newline='') as csv_out:
        writer = csv.writer(csv_out)
        
        # Iterate over rows in the CSV
        for row in reader:

            filtered_row = []
            
            for word in row:
                word = word.strip()
                
                # Checks if the word length is greater than 4 & starts with an alphabetical letter
                if len(word) > 4 and re.match(r"^[a-zA-Z]", word):
                    filtered_row.append(word)
            
            # Filters out any empty rows
            if filtered_row:
                writer.writerow(filtered_row)

print(f"Filtered data saved to: {output_file}.")