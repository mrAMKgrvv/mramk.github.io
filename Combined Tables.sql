#Importing/inspecting the csv files 
#my files kept saying itselfs as csv twice

January23 <-read_csv("cyclistic/January 23.csv.csv")
February23 <-read_csv("cyclistic/February 23.csv.csv")
March23 <-read_csv("cyclistic/March 23.csv.csv")
April23 <-read_csv("cyclistic/April 23.csv.csv")
May23 <-read_csv("cyclistic/May 23.csv.csv")
June23 <-read_csv("cyclistic/June 23.csv.csv")
July23 <-read_csv("cyclistic/July 23.csv.csv")
August23 <-read_csv("cyclistic/August 23.csv.csv")
September23 <-read_csv("cyclistic/September 23.csv.csv")
October23 <-read_csv("cyclistic/October 23.csv.csv")
November23 <-read_csv("cyclistic/November 23.csv.csv")
December23 <-read_csv("cyclistic/December 23.csv.csv")

#Binding Rows
trip_data <- bind_rows(February23, March23, April23, May23, June23, July23, August23, September23, October23, November23, December23, January23)

