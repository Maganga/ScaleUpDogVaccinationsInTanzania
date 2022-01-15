# function to sort date of records from 1990 to 2017
#read in vector of dates in standard formats eg 1 Aug 03 or Aug-03 etc
#returns a vector of numbers giving the month since jan 1990 (1) to Dec 05 (180)

sortdate = function(datebad) {	#ORIGINAL SORT DATE FUNCTION THAT GIVES THE DATE IN MONTHS SINCE JAN 1990
dategood=monthgood=yeargood=rep(NA, length(datebad))
monthgood[grep("jan",as.character(datebad),ignore.case=T)]=1
monthgood[grep("feb",as.character(datebad),ignore.case=T)]=2
monthgood[grep("mar",as.character(datebad),ignore.case=T)]=3
monthgood[grep("apr",as.character(datebad),ignore.case=T)]=4
monthgood[grep("may",as.character(datebad),ignore.case=T)]=5
monthgood[grep("jun",as.character(datebad),ignore.case=T)]=6
monthgood[grep("jul",as.character(datebad),ignore.case=T)]=7
monthgood[grep("aug",as.character(datebad),ignore.case=T)]=8
monthgood[grep("sep",as.character(datebad),ignore.case=T)]=9
monthgood[grep("oct",as.character(datebad),ignore.case=T)]=10
monthgood[grep("nov",as.character(datebad),ignore.case=T)]=11
monthgood[grep("dec",as.character(datebad),ignore.case=T)]=12

yeargood[grep("-90",as.character(datebad),ignore.case=T)]=0
yeargood[grep("-91",as.character(datebad),ignore.case=T)]=1
yeargood[grep("-92",as.character(datebad),ignore.case=T)]=2
yeargood[grep("-93",as.character(datebad),ignore.case=T)]=3
yeargood[grep("-94",as.character(datebad),ignore.case=T)]=4
yeargood[grep("-95",as.character(datebad),ignore.case=T)]=5
yeargood[grep("-96",as.character(datebad),ignore.case=T)]=6
yeargood[grep("-97",as.character(datebad),ignore.case=T)]=7
yeargood[grep("-98",as.character(datebad),ignore.case=T)]=8
yeargood[grep("-99",as.character(datebad),ignore.case=T)]=9
yeargood[grep("-00",as.character(datebad),ignore.case=T)]=10
yeargood[grep("-01",as.character(datebad),ignore.case=T)]=11
yeargood[grep("-02",as.character(datebad),ignore.case=T)]=12
yeargood[grep("-03",as.character(datebad),ignore.case=T)]=13
yeargood[grep("-04",as.character(datebad),ignore.case=T)]=14
yeargood[grep("-05",as.character(datebad),ignore.case=T)]=15
yeargood[grep("-06",as.character(datebad),ignore.case=T)]=16
yeargood[grep("-07",as.character(datebad),ignore.case=T)]=17
yeargood[grep("-08",as.character(datebad),ignore.case=T)]=18
yeargood[grep("-09",as.character(datebad),ignore.case=T)]=19
yeargood[grep("-10",as.character(datebad),ignore.case=T)]=20
yeargood[grep("-11",as.character(datebad),ignore.case=T)]=21
yeargood[grep("-12",as.character(datebad),ignore.case=T)]=22
yeargood[grep("-13",as.character(datebad),ignore.case=T)]=23
yeargood[grep("-14",as.character(datebad),ignore.case=T)]=24
yeargood[grep("-15",as.character(datebad),ignore.case=T)]=25
yeargood[grep("-16",as.character(datebad),ignore.case=T)]=26
yeargood[grep("-17",as.character(datebad),ignore.case=T)]=27

yeargood[grep(" 90",as.character(datebad),ignore.case=T)]=0
yeargood[grep(" 91",as.character(datebad),ignore.case=T)]=1
yeargood[grep(" 92",as.character(datebad),ignore.case=T)]=2
yeargood[grep(" 93",as.character(datebad),ignore.case=T)]=3
yeargood[grep(" 94",as.character(datebad),ignore.case=T)]=4
yeargood[grep(" 95",as.character(datebad),ignore.case=T)]=5
yeargood[grep(" 96",as.character(datebad),ignore.case=T)]=6
yeargood[grep(" 97",as.character(datebad),ignore.case=T)]=7
yeargood[grep(" 98",as.character(datebad),ignore.case=T)]=8
yeargood[grep(" 99",as.character(datebad),ignore.case=T)]=9
yeargood[grep(" 00",as.character(datebad),ignore.case=T)]=10
yeargood[grep(" 01",as.character(datebad),ignore.case=T)]=11
yeargood[grep(" 02",as.character(datebad),ignore.case=T)]=12
yeargood[grep(" 03",as.character(datebad),ignore.case=T)]=13
yeargood[grep(" 04",as.character(datebad),ignore.case=T)]=14
yeargood[grep(" 05",as.character(datebad),ignore.case=T)]=15
yeargood[grep(" 06",as.character(datebad),ignore.case=T)]=16
yeargood[grep(" 07",as.character(datebad),ignore.case=T)]=17
yeargood[grep(" 08",as.character(datebad),ignore.case=T)]=18
yeargood[grep(" 09",as.character(datebad),ignore.case=T)]=19
yeargood[grep(" 10",as.character(datebad),ignore.case=T)]=20
yeargood[grep(" 11",as.character(datebad),ignore.case=T)]=21
yeargood[grep(" 12",as.character(datebad),ignore.case=T)]=22
yeargood[grep(" 13",as.character(datebad),ignore.case=T)]=23
yeargood[grep(" 14",as.character(datebad),ignore.case=T)]=24
yeargood[grep(" 15",as.character(datebad),ignore.case=T)]=25
yeargood[grep(" 16",as.character(datebad),ignore.case=T)]=26
yeargood[grep(" 17",as.character(datebad),ignore.case=T)]=27
(12*yeargood)+monthgood
}

#___________________________________________________________________
sortday=function(datebad){ #Calculates the day from a formatted date string 
	day=format(strptime(datebad, "%d-%b-%y"), "%d")
	day
}

sortmonth=function(datebad){#Calculates the month from the date text 
	dategood=monthgood=yeargood=rep(NA, length(datebad))
	monthgood[grep("jan",as.character(datebad),ignore.case=T)]=1
	monthgood[grep("feb",as.character(datebad),ignore.case=T)]=2
	monthgood[grep("mar",as.character(datebad),ignore.case=T)]=3
	monthgood[grep("apr",as.character(datebad),ignore.case=T)]=4
	monthgood[grep("may",as.character(datebad),ignore.case=T)]=5
	monthgood[grep("jun",as.character(datebad),ignore.case=T)]=6
	monthgood[grep("jul",as.character(datebad),ignore.case=T)]=7
	monthgood[grep("aug",as.character(datebad),ignore.case=T)]=8	
	monthgood[grep("sep",as.character(datebad),ignore.case=T)]=9
	monthgood[grep("oct",as.character(datebad),ignore.case=T)]=10
	monthgood[grep("nov",as.character(datebad),ignore.case=T)]=11
	monthgood[grep("dec",as.character(datebad),ignore.case=T)]=12
	monthgood
}

sortyear=function(datebad){#Calculates the year from the date text
	yeargood=rep(NA, length(datebad))
	yeargood[grep("-90",as.character(datebad),ignore.case=T)]=0
	yeargood[grep("-91",as.character(datebad),ignore.case=T)]=1
	yeargood[grep("-92",as.character(datebad),ignore.case=T)]=2
	yeargood[grep("-93",as.character(datebad),ignore.case=T)]=3
	yeargood[grep("-94",as.character(datebad),ignore.case=T)]=4
	yeargood[grep("-95",as.character(datebad),ignore.case=T)]=5
	yeargood[grep("-96",as.character(datebad),ignore.case=T)]=6
	yeargood[grep("-97",as.character(datebad),ignore.case=T)]=7
	yeargood[grep("-98",as.character(datebad),ignore.case=T)]=8
	yeargood[grep("-99",as.character(datebad),ignore.case=T)]=9
	yeargood[grep("-00",as.character(datebad),ignore.case=T)]=10
	yeargood[grep("-01",as.character(datebad),ignore.case=T)]=11
	yeargood[grep("-02",as.character(datebad),ignore.case=T)]=12
	yeargood[grep("-03",as.character(datebad),ignore.case=T)]=13
	yeargood[grep("-04",as.character(datebad),ignore.case=T)]=14
	yeargood[grep("-05",as.character(datebad),ignore.case=T)]=15
	yeargood[grep("-06",as.character(datebad),ignore.case=T)]=16
	yeargood[grep("-07",as.character(datebad),ignore.case=T)]=17
	yeargood[grep("-08",as.character(datebad),ignore.case=T)]=18
	yeargood[grep("-09",as.character(datebad),ignore.case=T)]=19
	yeargood[grep("-10",as.character(datebad),ignore.case=T)]=20
	yeargood[grep("-11",as.character(datebad),ignore.case=T)]=21
	yeargood[grep("-12",as.character(datebad),ignore.case=T)]=22
	yeargood[grep("-13",as.character(datebad),ignore.case=T)]=23
	yeargood[grep("-14",as.character(datebad),ignore.case=T)]=24
	yeargood[grep("-15",as.character(datebad),ignore.case=T)]=25
	yeargood[grep("-16",as.character(datebad),ignore.case=T)]=26
	yeargood[grep("-17",as.character(datebad),ignore.case=T)]=27
	
	yeargood[grep(" 90",as.character(datebad),ignore.case=T)]=0
	yeargood[grep(" 91",as.character(datebad),ignore.case=T)]=1
	yeargood[grep(" 92",as.character(datebad),ignore.case=T)]=2
	yeargood[grep(" 93",as.character(datebad),ignore.case=T)]=3
	yeargood[grep(" 94",as.character(datebad),ignore.case=T)]=4
	yeargood[grep(" 95",as.character(datebad),ignore.case=T)]=5
	yeargood[grep(" 96",as.character(datebad),ignore.case=T)]=6
	yeargood[grep(" 97",as.character(datebad),ignore.case=T)]=7
	yeargood[grep(" 98",as.character(datebad),ignore.case=T)]=8
	yeargood[grep(" 99",as.character(datebad),ignore.case=T)]=9
	yeargood[grep(" 00",as.character(datebad),ignore.case=T)]=10
	yeargood[grep(" 01",as.character(datebad),ignore.case=T)]=11
	yeargood[grep(" 02",as.character(datebad),ignore.case=T)]=12
	yeargood[grep(" 03",as.character(datebad),ignore.case=T)]=13
	yeargood[grep(" 04",as.character(datebad),ignore.case=T)]=14
	yeargood[grep(" 05",as.character(datebad),ignore.case=T)]=15
	yeargood[grep(" 06",as.character(datebad),ignore.case=T)]=16
	yeargood[grep(" 07",as.character(datebad),ignore.case=T)]=17
	yeargood[grep(" 08",as.character(datebad),ignore.case=T)]=18
	yeargood[grep(" 09",as.character(datebad),ignore.case=T)]=19
	yeargood[grep(" 10",as.character(datebad),ignore.case=T)]=20
	yeargood[grep(" 11",as.character(datebad),ignore.case=T)]=21
	yeargood[grep(" 12",as.character(datebad),ignore.case=T)]=22
	yeargood[grep(" 13",as.character(datebad),ignore.case=T)]=23
	yeargood[grep(" 14",as.character(datebad),ignore.case=T)]=24
	yeargood[grep(" 15",as.character(datebad),ignore.case=T)]=25
	yeargood[grep(" 16",as.character(datebad),ignore.case=T)]=26
	yeargood[grep(" 17",as.character(datebad),ignore.case=T)]=27
	yeargood

}

sortmlength=function(datebad) {#Calculates monthlength in days from the date text
	mlength=rep(NA, length(datebad))
	mlength[grep("jan",as.character(datebad),ignore.case=T)]=31
	mlength[grep("feb",as.character(datebad),ignore.case=T)]=28
	mlength[grep("mar",as.character(datebad),ignore.case=T)]=31
	mlength[grep("apr",as.character(datebad),ignore.case=T)]=30
	mlength[grep("may",as.character(datebad),ignore.case=T)]=31
	mlength[grep("jun",as.character(datebad),ignore.case=T)]=30
	mlength[grep("jul",as.character(datebad),ignore.case=T)]=31
	mlength[grep("aug",as.character(datebad),ignore.case=T)]=31
	mlength[grep("sep",as.character(datebad),ignore.case=T)]=30
	mlength[grep("oct",as.character(datebad),ignore.case=T)]=31
	mlength[grep("nov",as.character(datebad),ignore.case=T)]=30
	mlength[grep("dec",as.character(datebad),ignore.case=T)]=31
	mlength
}

sorttext=function(datebad, days, mlength){	#calculates a random day for an event given "mid", "early", "late" and "in" text
	earlyI=grep("early",as.character(datebad),ignore.case=T)
	midI=grep("mid",as.character(datebad),ignore.case=T)
	lateI=grep("late",as.character(datebad),ignore.case=T)
	inI=grep("in",as.character(datebad),ignore.case=T)
	days[earlyI]=round(runif(length(earlyI), 1, 10))
	days[midI]=round(runif(length(midI), 10, 20))
	days[lateI]=round(runif(length(lateI), 20, mlength[lateI]))	
	days[inI]=round(runif(length(inI), 20, mlength[inI]))	
	days
}

mdaycalc=function(month){ #Calculate how many days since the start fo the year to reach the month given 
	monthlength=c(31,28,31,30,31,30,31,31,30,31,30,31)
	if (!is.na(month)==1) {mdays=sum(monthlength[0:(month-1)])}
	else (mdays=NA)
	mdays
}

getdays=function(baddate){ #COMBINES THE ABOVE, GIVES A DATE IN DAYS SINCE JAN'90 - randomly assigning the day if there is uncertainty
	year=sortyear(baddate)
	month=sortmonth(baddate)
	mlength=sortmlength(baddate)
	days=sorttext(baddate, as.integer(sortday(baddate)), mlength)
	Tdays=(365*year)+unlist(lapply(month, mdaycalc))+days
	Tdays
}

########################################################
#

newday=function(datebad){  #gives the median day for an event given "mid", "early", "late" and "in" text
  days=sortday(datebad)
  earlyI=grep("early",as.character(datebad),ignore.case=T)
  midI=grep("mid",as.character(datebad),ignore.case=T)
  lateI=grep("late",as.character(datebad),ignore.case=T)
  inI=grep("in",as.character(datebad),ignore.case=T)
  days[earlyI]=rep(5, length(earlyI))
  days[midI]=rep(15, length(midI))
  days[lateI]=rep(25, length(lateI))	
  days[inI]=rep(15, length(inI))
  days
}


accuracy=function(datebad){  #gives the +/- days for the date given "mid", "early", "late" and "in" text
  accuracy=rep(0, length(datebad))
  accuracy[grep("early",as.character(datebad),ignore.case=T)]=7
  accuracy[grep("mid",as.character(datebad),ignore.case=T)]=7
  accuracy[grep("late",as.character(datebad),ignore.case=T)]=7
  accuracy[grep("in",as.character(datebad),ignore.case=T)]=28
  accuracy
}

newformat=function(baddate){ #COMBINES THE ABOVE, GIVES A DATE and creates an accuracy column
  year=sortyear(baddate)
  month=sortmonth(baddate)
  day=newday(baddate)
  ifelse(is.na(day), NA, paste(day, "-", month, "-", year+1990))
}



