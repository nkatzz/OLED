"""
// < 1 euro
event tiny_amount(card_pan)

// < 50 euro
event very_small_amount(card_pan)

// < 100 euro
event small_amount(card_pan)

// < 200 euro
event medium_amount(card_pan)

// < 300 euro
event high_amount(card_pan)

// < 400 euro
event very_high_amount(card_pan)

// < 600 euro
event massive_amount(card_pan)

The fluent of interest is fraud(card_pan). 

The row structure in the csv is:

"""

import csv
import sys
from itertools import groupby

input_data = '/media/storage_/SPEEDD-FRAUD-TEST-DATA/speedd_patterns.csv'
out_data = '/media/storage_/SPEEDD-FRAUD-TEST-DATA/fraud-data.txt'


""" This is not used, the conversion takes place at ASP side. """
def classify_amounts(amount):
    x = float(amount)
    if x < 1:
        return 'tiny_amount'
    elif x < 50:
        return 'very_small_amount'
    elif x < 100:
        return 'small_amount'
    elif x < 200: 
        return 'medium_amount'
    elif x < 300:
        return 'high_amount'
    elif x < 400:
        return 'very_high_amount'  
    elif x < 600:
        return 'massive_amount'

#buffer = []

with open(input_data, 'rb') as csvfile, open(out_data, "wb") as outfile:
    transactions_reader = csv.reader(csvfile, delimiter=',')
    headers = next(transactions_reader, None)
    current_time = 0


    for row in transactions_reader:
        (timestamp,transaction_id,is_cnp,amount_eur,card_pan,card_exp_date,card_country,card_family,\
            card_type,card_tech,acquirer_country,merchant_mcc,terminal_brand,terminal_id,terminal_type,\
            terminal_emv,transaction_response,card_auth,terminal_auth,client_auth,card_band,cvv_validation,\
            tmp_card_pan,tmp_card_exp_date,transaction_type,auth_type,fraud,pattern) = \
            (row[0],row[1],row[2],row[3],row[4],row[5],row[6],row[7],row[8],row[9],row[10],row[11],row[12],row[13],row[14],\
            row[15],row[16],row[17],row[18],row[19],row[20],row[21],row[22],row[23],row[24],row[25],row[26],row[27])
         
        msg = 'transaction(%s,\"%s\",%s,\"%s\",\"%s\",%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,\"%s\",%s,%s,%s,%s,%s).\n'%(str(current_time),transaction_id,is_cnp,amount_eur,card_pan,card_exp_date,card_country,card_family,card_type,card_tech,acquirer_country,merchant_mcc,terminal_brand,terminal_id,terminal_type,terminal_emv,transaction_response,card_auth,terminal_auth,client_auth,card_band,cvv_validation,tmp_card_pan,tmp_card_exp_date,transaction_type,auth_type,fraud,pattern)
        # msg = 'row(\"%s\",\"%s\",%s).\n' % (transaction_id,amount_eur,str(current_time))
        outfile.write(msg)
        current_time += 1
        

    


