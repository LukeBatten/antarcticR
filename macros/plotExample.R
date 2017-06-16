### Quick example macro of how to run some antarcticR functions
require(antarcticR)

# produce a latitude-longitude dataframe froma .csv file: used to look at the actually long-lat positions
dataFrame <- csvToDF("../data/examplePoints.csv")
                                        #dataFrame

dataFrame

jub <- plotAntarctica(antarcticMap, dataFrame)
jub
