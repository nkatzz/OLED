import csv
import sys

def run(neg_ratio, read_from, write_to):
    with open(read_from, 'rb') as data, open(write_to, 'wb') as out_file: 
        rows = csv.reader(data, delimiter="|")
        writer = csv.writer(out_file, delimiter='|')
        pos_count = 1 # avoid division by zero
        neg_count = 0
        row_count = 0
        for row in rows:
            try: 
                row_count += 1
                if row[28] == '1':
                    writer.writerow(row)
                    pos_count += 1
                    print(row_count,'fraud')
                else:
                    if add_negative(neg_count, pos_count + neg_count, neg_ratio):
                        writer.writerow(row)
                        neg_count += 1
                        print(row_count,'no fraud')
            except IndexError:
                print(row)
                pass      
        out_file.close()
        print("positives: %s, negatives: %s"%(str(pos_count),str(neg_count)))

""" The total negatives must be at most ratio% of the whole """
def add_negative(neg_count, whole, ratio):
    return float(ratio) > 100 * float(neg_count)/float(whole)

""" a transaction is valid is none of its fields is empty """
""" 
def is_valid_row(row):
    def non_empty(x):
        return x.strip() != ''
    return all(non_empty(x) for x in row)
"""

"""
Run as

python pos-neg-split-2.py "/path/to/data.csv" "/out/path.csv" 70

The above will generate a summary of the dataset where the negatives are the 70% and the postives are 30%.
"""

if __name__ == "__main__":
    read = sys.argv[1]
    write = sys.argv[2]
    neg_ratio = sys.argv[3]
    print(read, write, neg_ratio)
    run(neg_ratio, read, write)
    
     

#['2145927781401', '0180419aa39608b59ccecf6b277308d3', '0', '12.53', '44366728', '204009', '5430', '7382', '341', '60', '2612', '2640', '2342769205', '3691143237', '29', '7', '407', '0510', '68', '3', '36', '16', '', '', '22', '13', '4', 'Food/Entertainment', '0']

    
    
 


