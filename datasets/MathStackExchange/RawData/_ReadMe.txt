Datasets
--------
Math Stack Exhange

Levels of quantization (in separate files)
------------------------------------------
3 levels
5 levels

Data outline
------------
Each row contains the evolution of a single community across time (time is split into 20 windows)
Different steps of the evolution of the same community are separated using the "|" char.
For each step in the evolution the following is available (including 14 structural features for which both quantized and real values are given):

w<window id (i.e. timestamp)> , c<community id> , <quantized size feature> , <quantized density feature> , <quantized cohesion feature> , <quantized normalized association feature> , <quantized ratio association feature> , <quantized ratio cut feature> , <quantized normalized edges number feature> , <quantized average path length feature> , <quantized diameter feature> , <quantized clustering coefficient feature> , <quantized closeness centrality feature> , <quantized betweenness centrality feature> , <quantized eigenvector centrality feature> , <quantized centrality feature> , <real-valued size feature> , <real-valued density feature> , <real-valued cohesion feature> , <real-valued normalized association feature> , <real-valued ratio association feature> , <real-valued ratio cut feature> , <real-valued normalized edges number feature> , <real-valued average path length feature> , <real-valued diameter feature> , <real-valued clustering coefficient feature> , <real-valued closeness centrality feature> , <real-valued betweenness centrality feature> , <real-valued eigenvector centrality feature> , <real-valued centrality feature> , <event (i.e. label)>

e.g.
w1,c0,3,1,1,3,3,1,3,3,3,2,1,1,1,3,0.24063464081092992,0.04773330644890278,6.214432591787556,0.49567280848687884,13.007326007326007,13.234432234432234,0.43509158855602525,2.3965722351043452,6.0,0.41573778206681306,0.42634670874291414,0.002567228373353579,0.0018315018315018172,21430.0,growing



Possible events are: {continuing, shrinking, growing, dissolving} 
A community grows or shrinks when there is a difference of 6 or more members between two consequtive windows.


Note: The real values of the features are not normalized. If used then some normalization is necessary duw to different scales.
