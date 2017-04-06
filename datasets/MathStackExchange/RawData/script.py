
"""
Script to turn the data into Event Calculus format and add them to a mongo database
"""
from pymongo import Connection

in_file = './quantized_features_chains_5categories.txt'
out_file = './training_data.lp'

def process_line(row, i):
    line = row.split(",")
    time = line[0].split('w')[1]
    id = 'id'+str(i)
    size = line[2]
    density = line[3]
    cohesion = line[4]
    association = line[5]
    ratio_association = line[6]
    ratio_cut = line[7]
    edge_number = line[8]
    average_path_length = line[9]
    diameter = line[10]
    clustering_coefficient = line[11]
    closeness_centrality = line[12]
    betweenness_centrality = line[13]
    eigenvector_centrality = line[14]
    centrality = line[15]
    label = line[len(line) - 1].replace(" ", "").replace("\n", "")
    _size = "happensAt(size(%s,%s),%s)"%(id,size,time)
    _density = "happensAt(density(%s,%s),%s)"%(id,density,time)
    _cohesion = "happensAt(cohesion(%s,%s),%s)"%(id,cohesion,time)
    _association = "happensAt(association(%s,%s),%s)"%(id,association,time)
    _ratio_association = "happensAt(ratio_association(%s,%s),%s)"%(id,ratio_association,time)
    _ratio_cut = "happensAt(ratio_cut(%s,%s),%s)"%(id,ratio_cut,time)
    _edge_number = "happensAt(edge_number(%s,%s),%s)"%(id,edge_number,time)
    _average_path_length = "happensAt(average_path_length(%s,%s),%s)"%(id,average_path_length,time)
    _diameter = "happensAt(diameter(%s,%s),%s)"%(id,diameter,time)
    _clustering_coefficient = "happensAt(clustering_coefficient(%s,%s),%s)"%(id,clustering_coefficient,time)
    _closeness_centrality = "happensAt(closeness_centrality(%s,%s),%s)"%(id,closeness_centrality,time)
    _betweenness_centrality = "happensAt(betweenness_centrality(%s,%s),%s)"%(id,betweenness_centrality,time)
    _eigenvector_centrality = "happensAt(eigenvector_centrality(%s,%s),%s)"%(id,eigenvector_centrality,time)
    _centrality = "happensAt(centrality(%s,%s),%s)"%(id,centrality,time)
    _label = ("holdsAt(%s(%s),%s)"%(label,id,time)).replace(" ", "")
    return "%s %s %s %s %s %s %s %s %s %s %s %s %s %s %s"%(_size,_density,_cohesion,_association,_ratio_association,\
           _ratio_cut,_edge_number,_average_path_length,_diameter,_clustering_coefficient,_closeness_centrality,_betweenness_centrality,\
           _eigenvector_centrality,_centrality,_label)
    
def insert_to_mongo(database,example):
    pass

line_counter = 0
example_counter = 0
db_name = 'MathStackExchange'
connection = Connection()
connection.drop_database(db_name) # clear if exists
database = connection[db_name]

with open(in_file) as f1, open(out_file, "wb") as f2:
    for line in f1:
        splitted = line.split("|")
        episodes = map(lambda x: process_line(x, line_counter), splitted)
        # write to file, just for debugging
        s = '\n'.join(episodes)
        f2.write(s+"\n\n")   
        # add to database
        x = ' '.join(episodes).split(' ')
        narrative = filter(lambda x: "happensAt" in x, x)
        annotation = filter(lambda x: "holdsAt" in x, x) 
        exmpl_id = example_counter
        post = {'time':exmpl_id,'annotation':annotation,'narrative':narrative}
        database.examples.insert(post)     
        line_counter += 1
        example_counter += 1

connection.close()



