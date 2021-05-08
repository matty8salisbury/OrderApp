#SET REQUIRED PASSWORDS

#SET VENUE NAME AND VENUE DISPLAY NAME
venue <<- "Bananaman`s_Bar_PE27_6TN"
venueDisplayTitle <<- "Bananaman`s Bar"

#SQL database host, port, username and password
Sys.setenv(SQL_ENDPOINT = 'database-2.cnmaqhhd7kkj.eu-west-2.rds.amazonaws.com')
Sys.setenv(SQL_PORT = 3306)
Sys.setenv(MY_UID='sqlUsername')
Sys.setenv(MY_PWD='ReplaceThisPassword')

#SET VENUE LOGIN PASSWORD FOR VENUE END APP NB: NO APOSTROPHES IN THESE 
Sys.setenv(Venue_PsWd='BetterThanSuperTed001!')
