# Author: Suyang Lu
# Data: Feb-29-2016
# This program is for count the difference of dict_index between bundle_1 and bundle_n for n > 1
# Out put will be several diff_1_n.csv file for some n > 1

import csv
import os


# Set working directory
os.chdir("/Users/suyanglu/Dropbox/urap_programming/suyang/gen_deln_adjacent")
# Target file
# Put your file path here!
file_path = "input/new_lst_700D_Yi_Feb222016_giftindex_price_corrected_juexiao( 2016-02-23).csv"

# This part is for count

# Compare two array and return the difference
def comp_array(one, other):
	count = 0
	for i in other:
		if i not in one:
			count += 1

	for i in one:
		if i not in other:
			count += 1

	return count

# Find the unique item id.
# Open the csv file.
with open(file_path) as csvfile:
	csv_opened = csv.DictReader(csvfile)
	itemID_all = []
	for row in csv_opened:
		if row["ItemID(LIsting_id)"] not in itemID_all:
			itemID_all += [row["ItemID(LIsting_id)"]]
print("Finished count all item id")

# get each id's dict_yi_index when bundle_index = 1
with open(file_path) as csvfile:
	csv_opened = csv.DictReader(csvfile)
	dict_yi_index_1 = []
	for itemID in itemID_all:
		csvfile.seek(0)
		temp_dict_index = []
		for row in csv_opened:
			if row["ItemID(LIsting_id)"] == itemID and row["bundle_index"] == "1" and row["dict_yi_index"] not in temp_dict_index:
				temp_dict_index += [row["dict_yi_index"]]
		dict_yi_index_1 += [temp_dict_index]



# get each id's dict_yi_index when bundle_index = 2
with open(file_path) as csvfile:
	csv_opened = csv.DictReader(csvfile)
	dict_yi_index_2 = []
	for itemID in itemID_all:
		csvfile.seek(0)
		temp_dict_index = []
		for row in csv_opened:
			if row["ItemID(LIsting_id)"] == itemID and row["bundle_index"] == "2" and row["dict_yi_index"] not in temp_dict_index:
				temp_dict_index += [row["dict_yi_index"]]
		dict_yi_index_2 += [temp_dict_index]



# get each id's dict_yi_index when bundle_index = 3
with open(file_path) as csvfile:
	csv_opened = csv.DictReader(csvfile)
	dict_yi_index_3 = []
	for itemID in itemID_all:
		csvfile.seek(0)
		temp_dict_index = []
		for row in csv_opened:
			if row["ItemID(LIsting_id)"] == itemID and row["bundle_index"] == "3" and row["dict_yi_index"] not in temp_dict_index:
				temp_dict_index += [row["dict_yi_index"]]
		dict_yi_index_3 += [temp_dict_index]



# get each id's dict_yi_index when bundle_index = 4
with open(file_path) as csvfile:
	csv_opened = csv.DictReader(csvfile)
	dict_yi_index_4 = []
	for itemID in itemID_all:
		csvfile.seek(0)
		temp_dict_index = []
		for row in csv_opened:
			if row["ItemID(LIsting_id)"] == itemID and row["bundle_index"] == "4" and row["dict_yi_index"] not in temp_dict_index:
				temp_dict_index += [row["dict_yi_index"]]
		dict_yi_index_4 += [temp_dict_index]


# get each id's dict_yi_index when bundle_index = 5
with open(file_path) as csvfile:
	csv_opened = csv.DictReader(csvfile)
	dict_yi_index_5 = []
	for itemID in itemID_all:
		csvfile.seek(0)
		temp_dict_index = []
		for row in csv_opened:
			if row["ItemID(LIsting_id)"] == itemID and row["bundle_index"] == "5" and row["dict_yi_index"] not in temp_dict_index:
				temp_dict_index += [row["dict_yi_index"]]
		dict_yi_index_5 += [temp_dict_index]



# get each id's dict_yi_index when bundle_index = 6
with open(file_path) as csvfile:
	csv_opened = csv.DictReader(csvfile)
	dict_yi_index_6 = []
	for itemID in itemID_all:
		csvfile.seek(0)
		temp_dict_index = []
		for row in csv_opened:
			if row["ItemID(LIsting_id)"] == itemID and row["bundle_index"] == "6" and row["dict_yi_index"] not in temp_dict_index:
				temp_dict_index += [row["dict_yi_index"]]
		dict_yi_index_6 += [temp_dict_index]
#print(len(dict_yi_index_6))



# get each id's dict_yi_index when bundle_index = 7
with open(file_path) as csvfile:
	csv_opened = csv.DictReader(csvfile)
	dict_yi_index_7 = []
	for itemID in itemID_all:
		csvfile.seek(0)
		temp_dict_index = []
		for row in csv_opened:
			if row["ItemID(LIsting_id)"] == itemID and row["bundle_index"] == "7" and row["dict_yi_index"] not in temp_dict_index:
				temp_dict_index += [row["dict_yi_index"]]
		dict_yi_index_7 += [temp_dict_index]
#print(len(dict_yi_index_7))
print("Finished count dict_yi_index_7")


# get each id's dict_yi_index when bundle_index = 8
with open(file_path) as csvfile:
	csv_opened = csv.DictReader(csvfile)
	dict_yi_index_8 = []
	for itemID in itemID_all:
		csvfile.seek(0)
		temp_dict_index = []
		for row in csv_opened:
			if row["ItemID(LIsting_id)"] == itemID and row["bundle_index"] == "8" and row["dict_yi_index"] not in temp_dict_index:
				temp_dict_index += [row["dict_yi_index"]]
		dict_yi_index_8 += [temp_dict_index]



# This part is for Write file

# csv file filenames here
fn = ['Author', 'ItemID(LIsting_id)', 'bundle_index', 'diff']

# Author name
an = "suyang"
l = len(dict_yi_index_8)
# for each id compare their dict_yi_index with bundle_index = 1 and bundle_index = 2
with open('gen_deln_adjacent.csv', 'w') as csvfile:
		writer = csv.DictWrriter(csvfile, fieldnames = fn)
		writer.writeheader()
		for idx in range(0, l):
			one = dict_yi_index_1[idx]
			other2 = dict_yi_index_2[idx]
			other3 = dict_yi_index_3[idx]
			other4 = dict_yi_index_4[idx]
			other5 = dict_yi_index_5[idx]
			other6 = dict_yi_index_6[idx]
			other7 = dict_yi_index_7[idx]
			other8 = dict_yi_index_8[idx]
			
			

			if one == []:
				#print(itemID_all[idx], " bundle_index = 1 does not exist ")
				writer.writerow({'Author': an, 'ItemID(LIsting_id)': itemID_all[idx], 'bundle_index': " ", 'diff': "bundle_index = 1 does not exist"})
			elif other2 == []:
				#print(itemID_all[idx], " bundle_index = 2 does not exist ")
				writer.writerow({'Author': an, 'ItemID(LIsting_id)': itemID_all[idx], 'bundle_index': " ", 'diff': "bundle_index = 2 does not exist"})
			else:
				writer.writerow({'Author': an, 'ItemID(LIsting_id)': itemID_all[idx], 'bundle_index': "(1,3)", 'diff': comp_array(one, other3)})
			

			


		
			if one == []:
				#print(itemID_all[idx], " bundle_index = 1 does not exist ")
				writer.writerow({'Author': an, 'ItemID(LIsting_id)': itemID_all[idx], 'bundle_index': " ", 'diff': "bundle_index = 1 does not exist"})
			elif other3 == []:
				#print(itemID_all[idx], " bundle_index = 2 does not exist ")
				writer.writerow({'Author': an, 'ItemID(LIsting_id)': itemID_all[idx], 'bundle_index': " ", 'diff': "bundle_index = 3 does not exist"})
			else:
				writer.writerow({'Author': an, 'ItemID(LIsting_id)': itemID_all[idx], 'bundle_index': "(1,3)", 'diff': comp_array(one, other3)})
			

			if other2 == []:
				#print(itemID_all[idx], " bundle_index = 1 does not exist ")
				writer.writerow({'Author': an, 'ItemID(LIsting_id)': itemID_all[idx], 'bundle_index': " ", 'diff': "bundle_index = 2 does not exist"})
			elif other3 == []:
				#print(itemID_all[idx], " bundle_index = 2 does not exist ")
				writer.writerow({'Author': an, 'ItemID(LIsting_id)': itemID_all[idx], 'bundle_index': " ", 'diff': "bundle_index = 3 does not exist"})
			else:
				writer.writerow({'Author': an, 'ItemID(LIsting_id)': itemID_all[idx], 'bundle_index': "(2,3)", 'diff': comp_array(other2, other3)})
			

			

			




			if other1 == []:
				#print(itemID_all[idx], " bundle_index = 1 does not exist ")
				writer.writerow({'Author': an, 'ItemID(LIsting_id)': itemID_all[idx], 'bundle_index': " ", 'diff': "bundle_index = 1 does not exist"})
			elif other4== []:
				#print(itemID_all[idx], " bundle_index = 2 does not exist ")
				writer.writerow({'Author': an, 'ItemID(LIsting_id)': itemID_all[idx], 'bundle_index': " ", 'diff': "bundle_index = 4 does not exist"})
			else:
				writer.writerow({'Author': an, 'ItemID(LIsting_id)': itemID_all[idx], 'bundle_index': "(1,4)", 'diff': comp_array(one, other4)})
			
			if other2 == []:
				#print(itemID_all[idx], " bundle_index = 1 does not exist ")
				writer.writerow({'Author': an, 'ItemID(LIsting_id)': itemID_all[idx], 'bundle_index': " ", 'diff': "bundle_index = 2 does not exist"})
			elif other4== []:
				#print(itemID_all[idx], " bundle_index = 2 does not exist ")
				writer.writerow({'Author': an, 'ItemID(LIsting_id)': itemID_all[idx], 'bundle_index': " ", 'diff': "bundle_index = 4 does not exist"})
			else:
				writer.writerow({'Author': an, 'ItemID(LIsting_id)': itemID_all[idx], 'bundle_index': "(2,4)", 'diff': comp_array(other2, other4)})
			
			if other3 == []:
				#print(itemID_all[idx], " bundle_index = 1 does not exist ")
				writer.writerow({'Author': an, 'ItemID(LIsting_id)': itemID_all[idx], 'bundle_index': " ", 'diff': "bundle_index = 3 does not exist"})
			elif other4== []:
				#print(itemID_all[idx], " bundle_index = 2 does not exist ")
				writer.writerow({'Author': an, 'ItemID(LIsting_id)': itemID_all[idx], 'bundle_index': " ", 'diff': "bundle_index = 4 does not exist"})
			else:
				writer.writerow({'Author': an, 'ItemID(LIsting_id)': itemID_all[idx], 'bundle_index': "(3,4)", 'diff': comp_array(other3, other4)})
			
			
			
			




			if one == []:
				#print(itemID_all[idx], " bundle_index = 1 does not exist ")
				writer.writerow({'Author': an, 'ItemID(LIsting_id)': itemID_all[idx], 'bundle_index': " ", 'diff': "bundle_index = 1 does not exist"})
			elif other5 == []:
				#print(itemID_all[idx], " bundle_index = 2 does not exist ")
				writer.writerow({'Author': an, 'ItemID(LIsting_id)': itemID_all[idx], 'bundle_index': " ", 'diff': "bundle_index = 5 does not exist"})
			else:
				writer.writerow({'Author': an, 'ItemID(LIsting_id)': itemID_all[idx], 'bundle_index': "(1,5)", 'diff': comp_array(one, other5)})
			
			if other2 == []:
				#print(itemID_all[idx], " bundle_index = 1 does not exist ")
				writer.writerow({'Author': an, 'ItemID(LIsting_id)': itemID_all[idx], 'bundle_index': " ", 'diff': "bundle_index = 2 does not exist"})
			elif other5 == []:
				#print(itemID_all[idx], " bundle_index = 2 does not exist ")
				writer.writerow({'Author': an, 'ItemID(LIsting_id)': itemID_all[idx], 'bundle_index': " ", 'diff': "bundle_index = 5 does not exist"})
			else:
				writer.writerow({'Author': an, 'ItemID(LIsting_id)': itemID_all[idx], 'bundle_index': "(2,5)", 'diff': comp_array(other2, other5)})
			
			if other3 == []:
				#print(itemID_all[idx], " bundle_index = 1 does not exist ")
				writer.writerow({'Author': an, 'ItemID(LIsting_id)': itemID_all[idx], 'bundle_index': " ", 'diff': "bundle_index = 3 does not exist"})
			elif other5 == []:
				#print(itemID_all[idx], " bundle_index = 2 does not exist ")
				writer.writerow({'Author': an, 'ItemID(LIsting_id)': itemID_all[idx], 'bundle_index': " ", 'diff': "bundle_index = 5 does not exist"})
			else:
				writer.writerow({'Author': an, 'ItemID(LIsting_id)': itemID_all[idx], 'bundle_index': "(3,5)", 'diff': comp_array(other3, other5)})
			

			if other4 == []:
				#print(itemID_all[idx], " bundle_index = 1 does not exist ")
				writer.writerow({'Author': an, 'ItemID(LIsting_id)': itemID_all[idx], 'bundle_index': " ", 'diff': "bundle_index = 4 does not exist"})
			elif other5 == []:
				#print(itemID_all[idx], " bundle_index = 2 does not exist ")
				writer.writerow({'Author': an, 'ItemID(LIsting_id)': itemID_all[idx], 'bundle_index': " ", 'diff': "bundle_index = 5 does not exist"})
			else:
				writer.writerow({'Author': an, 'ItemID(LIsting_id)': itemID_all[idx], 'bundle_index': "(4,5)", 'diff': comp_array(other4, other5)})
			
			
			


			if one == []:
				#print(itemID_all[idx], " bundle_index = 1 does not exist ")
				writer.writerow({'Author': an, 'ItemID(LIsting_id)': itemID_all[idx], 'bundle_index': " ", 'diff': "bundle_index = 1 does not exist"})
			elif other6 == []:
				#print(itemID_all[idx], " bundle_index = 2 does not exist ")
				writer.writerow({'Author': an, 'ItemID(LIsting_id)': itemID_all[idx], 'bundle_index': " ", 'diff': "bundle_index = 6 does not exist"})
			else:
				writer.writerow({'Author': an, 'ItemID(LIsting_id)': itemID_all[idx], 'bundle_index': "(1,6)", 'diff': comp_array(one, other6)})
			
			if other2 == []:
				#print(itemID_all[idx], " bundle_index = 1 does not exist ")
				writer.writerow({'Author': an, 'ItemID(LIsting_id)': itemID_all[idx], 'bundle_index': " ", 'diff': "bundle_index = 2 does not exist"})
			elif other6 == []:
				#print(itemID_all[idx], " bundle_index = 2 does not exist ")
				writer.writerow({'Author': an, 'ItemID(LIsting_id)': itemID_all[idx], 'bundle_index': " ", 'diff': "bundle_index = 6 does not exist"})
			else:
				writer.writerow({'Author': an, 'ItemID(LIsting_id)': itemID_all[idx], 'bundle_index': "(2,6)", 'diff': comp_array(other2, other6)})
			
			if other3 == []:
				#print(itemID_all[idx], " bundle_index = 1 does not exist ")
				writer.writerow({'Author': an, 'ItemID(LIsting_id)': itemID_all[idx], 'bundle_index': " ", 'diff': "bundle_index = 3 does not exist"})
			elif other6 == []:
				#print(itemID_all[idx], " bundle_index = 2 does not exist ")
				writer.writerow({'Author': an, 'ItemID(LIsting_id)': itemID_all[idx], 'bundle_index': " ", 'diff': "bundle_index = 6 does not exist"})
			else:
				writer.writerow({'Author': an, 'ItemID(LIsting_id)': itemID_all[idx], 'bundle_index': "(3,6)", 'diff': comp_array(other3, other6)})
			
			if other4 == []:
				#print(itemID_all[idx], " bundle_index = 1 does not exist ")
				writer.writerow({'Author': an, 'ItemID(LIsting_id)': itemID_all[idx], 'bundle_index': " ", 'diff': "bundle_index = 4 does not exist"})
			elif other6 == []:
				#print(itemID_all[idx], " bundle_index = 2 does not exist ")
				writer.writerow({'Author': an, 'ItemID(LIsting_id)': itemID_all[idx], 'bundle_index': " ", 'diff': "bundle_index = 6 does not exist"})
			else:
				writer.writerow({'Author': an, 'ItemID(LIsting_id)': itemID_all[idx], 'bundle_index': "(4,6)", 'diff': comp_array(other4, other6)})
			

			if other5 == []:
				#print(itemID_all[idx], " bundle_index = 1 does not exist ")
				writer.writerow({'Author': an, 'ItemID(LIsting_id)': itemID_all[idx], 'bundle_index': " ", 'diff': "bundle_index = 5 does not exist"})
			elif other6 == []:
				#print(itemID_all[idx], " bundle_index = 2 does not exist ")
				writer.writerow({'Author': an, 'ItemID(LIsting_id)': itemID_all[idx], 'bundle_index': " ", 'diff': "bundle_index = 6 does not exist"})
			else:
				writer.writerow({'Author': an, 'ItemID(LIsting_id)': itemID_all[idx], 'bundle_index': "(5,6)", 'diff': comp_array(other5, other6)})
			
			
			






			




			if other6 == []:
				#print(itemID_all[idx], " bundle_index = 1 does not exist ")
				writer.writerow({'Author': an, 'ItemID(LIsting_id)': itemID_all[idx], 'bundle_index': " ", 'diff': "bundle_index = 6 does not exist"})
			elif other7 == []:
				#print(itemID_all[idx], " bundle_index = 2 does not exist ")
				writer.writerow({'Author': an, 'ItemID(LIsting_id)': itemID_all[idx], 'bundle_index': " ", 'diff': "bundle_index = 7 does not exist"})
			else:
				writer.writerow({'Author': an, 'ItemID(LIsting_id)': itemID_all[idx], 'bundle_index': "(6,7)", 'diff': comp_array(other6, other7)})

			if other5 == []:
				#print(itemID_all[idx], " bundle_index = 1 does not exist ")
				writer.writerow({'Author': an, 'ItemID(LIsting_id)': itemID_all[idx], 'bundle_index': " ", 'diff': "bundle_index = 5 does not exist"})
			elif other7 == []:
				#print(itemID_all[idx], " bundle_index = 2 does not exist ")
				writer.writerow({'Author': an, 'ItemID(LIsting_id)': itemID_all[idx], 'bundle_index': " ", 'diff': "bundle_index = 7 does not exist"})
			else:
				writer.writerow({'Author': an, 'ItemID(LIsting_id)': itemID_all[idx], 'bundle_index': "(5,7)", 'diff': comp_array(other5, other7)})

			if other4 == []:
				#print(itemID_all[idx], " bundle_index = 1 does not exist ")
				writer.writerow({'Author': an, 'ItemID(LIsting_id)': itemID_all[idx], 'bundle_index': " ", 'diff': "bundle_index = 4 does not exist"})
			elif other7 == []:
				#print(itemID_all[idx], " bundle_index = 2 does not exist ")
				writer.writerow({'Author': an, 'ItemID(LIsting_id)': itemID_all[idx], 'bundle_index': " ", 'diff': "bundle_index = 7 does not exist"})
			else:
				writer.writerow({'Author': an, 'ItemID(LIsting_id)': itemID_all[idx], 'bundle_index': "(4,7)", 'diff': comp_array(other4, other7)})

			if other3 == []:
				#print(itemID_all[idx], " bundle_index = 1 does not exist ")
				writer.writerow({'Author': an, 'ItemID(LIsting_id)': itemID_all[idx], 'bundle_index': " ", 'diff': "bundle_index = 3 does not exist"})
			elif other7 == []:
				#print(itemID_all[idx], " bundle_index = 2 does not exist ")
				writer.writerow({'Author': an, 'ItemID(LIsting_id)': itemID_all[idx], 'bundle_index': " ", 'diff': "bundle_index = 7 does not exist"})
			else:
				writer.writerow({'Author': an, 'ItemID(LIsting_id)': itemID_all[idx], 'bundle_index': "(3,7)", 'diff': comp_array(other3, other7)})

			if other2 == []:
				#print(itemID_all[idx], " bundle_index = 1 does not exist ")
				writer.writerow({'Author': an, 'ItemID(LIsting_id)': itemID_all[idx], 'bundle_index': " ", 'diff': "bundle_index = 2 does not exist"})
			elif other7 == []:
				#print(itemID_all[idx], " bundle_index = 2 does not exist ")
				writer.writerow({'Author': an, 'ItemID(LIsting_id)': itemID_all[idx], 'bundle_index': " ", 'diff': "bundle_index = 7 does not exist"})
			else:
				writer.writerow({'Author': an, 'ItemID(LIsting_id)': itemID_all[idx], 'bundle_index': "(2,7)", 'diff': comp_array(other2, other7)})

			if one == []:
				#print(itemID_all[idx], " bundle_index = 1 does not exist ")
				writer.writerow({'Author': an, 'ItemID(LIsting_id)': itemID_all[idx], 'bundle_index': " ", 'diff': "bundle_index = 1 does not exist"})
			elif other7 == []:
				#print(itemID_all[idx], " bundle_index = 2 does not exist ")
				writer.writerow({'Author': an, 'ItemID(LIsting_id)': itemID_all[idx], 'bundle_index': " ", 'diff': "bundle_index = 7 does not exist"})
			else:
				writer.writerow({'Author': an, 'ItemID(LIsting_id)': itemID_all[idx], 'bundle_index': "(1,7)", 'diff': comp_array(one, other7)})






			



			if other7 == []:
				#print(itemID_all[idx], " bundle_index = 1 does not exist ")
				writer.writerow({'Author': an, 'ItemID(LIsting_id)': itemID_all[idx], 'bundle_index': " ", 'diff': "bundle_index = 7 does not exist"})
			elif other8 == []:
				#print(itemID_all[idx], " bundle_index = 2 does not exist ")
				writer.writerow({'Author': an, 'ItemID(LIsting_id)': itemID_all[idx], 'bundle_index': " ", 'diff': "bundle_index = 8 does not exist"})
			else:
				writer.writerow({'Author': an, 'ItemID(LIsting_id)': itemID_all[idx], 'bundle_index': "(7,8)", 'diff': comp_array(other7, other8)})

			if other6 == []:
				#print(itemID_all[idx], " bundle_index = 1 does not exist ")
				writer.writerow({'Author': an, 'ItemID(LIsting_id)': itemID_all[idx], 'bundle_index': " ", 'diff': "bundle_index = 6 does not exist"})
			elif other8 == []:
				#print(itemID_all[idx], " bundle_index = 2 does not exist ")
				writer.writerow({'Author': an, 'ItemID(LIsting_id)': itemID_all[idx], 'bundle_index': " ", 'diff': "bundle_index = 8 does not exist"})
			else:
				writer.writerow({'Author': an, 'ItemID(LIsting_id)': itemID_all[idx], 'bundle_index': "(6,8)", 'diff': comp_array(other6, other8)})

			if other5 == []:
				#print(itemID_all[idx], " bundle_index = 1 does not exist ")
				writer.writerow({'Author': an, 'ItemID(LIsting_id)': itemID_all[idx], 'bundle_index': " ", 'diff': "bundle_index = 5 does not exist"})
			elif other8 == []:
				#print(itemID_all[idx], " bundle_index = 2 does not exist ")
				writer.writerow({'Author': an, 'ItemID(LIsting_id)': itemID_all[idx], 'bundle_index': " ", 'diff': "bundle_index = 8 does not exist"})
			else:
				writer.writerow({'Author': an, 'ItemID(LIsting_id)': itemID_all[idx], 'bundle_index': "(5,8)", 'diff': comp_array(other5, other8)})

			if other4 == []:
				#print(itemID_all[idx], " bundle_index = 1 does not exist ")
				writer.writerow({'Author': an, 'ItemID(LIsting_id)': itemID_all[idx], 'bundle_index': " ", 'diff': "bundle_index = 4 does not exist"})
			elif other8 == []:
				#print(itemID_all[idx], " bundle_index = 2 does not exist ")
				writer.writerow({'Author': an, 'ItemID(LIsting_id)': itemID_all[idx], 'bundle_index': " ", 'diff': "bundle_index = 8 does not exist"})
			else:
				writer.writerow({'Author': an, 'ItemID(LIsting_id)': itemID_all[idx], 'bundle_index': "(4,8)", 'diff': comp_array(other4, other8)})

			if other3 == []:
				#print(itemID_all[idx], " bundle_index = 1 does not exist ")
				writer.writerow({'Author': an, 'ItemID(LIsting_id)': itemID_all[idx], 'bundle_index': " ", 'diff': "bundle_index = 3 does not exist"})
			elif other8 == []:
				#print(itemID_all[idx], " bundle_index = 2 does not exist ")
				writer.writerow({'Author': an, 'ItemID(LIsting_id)': itemID_all[idx], 'bundle_index': " ", 'diff': "bundle_index = 8 does not exist"})
			else:
				writer.writerow({'Author': an, 'ItemID(LIsting_id)': itemID_all[idx], 'bundle_index': "(3,8)", 'diff': comp_array(other3, other8)})

			if other2 == []:
				#print(itemID_all[idx], " bundle_index = 1 does not exist ")
				writer.writerow({'Author': an, 'ItemID(LIsting_id)': itemID_all[idx], 'bundle_index': " ", 'diff': "bundle_index = 2 does not exist"})
			elif other8 == []:
				#print(itemID_all[idx], " bundle_index = 2 does not exist ")
				writer.writerow({'Author': an, 'ItemID(LIsting_id)': itemID_all[idx], 'bundle_index': " ", 'diff': "bundle_index = 8 does not exist"})
			else:
				writer.writerow({'Author': an, 'ItemID(LIsting_id)': itemID_all[idx], 'bundle_index': "(2,8)", 'diff': comp_array(other2, other8)})


			if one == []:
				#print(itemID_all[idx], " bundle_index = 1 does not exist ")
				writer.writerow({'Author': an, 'ItemID(LIsting_id)': itemID_all[idx], 'bundle_index': " ", 'diff': "bundle_index = 1 does not exist"})
			elif other8 == []:
				#print(itemID_all[idx], " bundle_index = 2 does not exist ")
				writer.writerow({'Author': an, 'ItemID(LIsting_id)': itemID_all[idx], 'bundle_index': " ", 'diff': "bundle_index = 8 does not exist"})
			else:
				writer.writerow({'Author': an, 'ItemID(LIsting_id)': itemID_all[idx], 'bundle_index': "(1,8)", 'diff': comp_array(one, other8)})




























































