 
##SET REQUIRED PASSWORDS

##SET VENUE NAME AND VENUE DISPLAY NAME: 
##VENUE SHOULD BE THE VENUE NAME AND POSTCODE WITH ANY SPACES REPLACED BY _ AND ANY APOSTROPHES REPLACED BY 1
##VENUE DISPLAY TITLE SHOULD EXACTLY HOW THE CUSTOMER SHOULD SEE THE NAME

venue <<- "Bananaman1s_Bar_PE27_6TN"
venueDisplayTitle <<- "Bananaman's Bar"

#SQL database host, port, username and password

Sys.setenv(SQL_ENDPOINT = "shinymenudb.cl5kbzs1nxfd.eu-west-2.rds.amazonaws.com")
Sys.setenv(SQL_PORT = 3306)
Sys.setenv(MY_UID='replaceThisUsername')
Sys.setenv(MY_PWD='replaceThisPassword')

#VENUE LOGIN PASSWORD FOR PUBEND AND CHECKCLOSED APPS

Sys.setenv(VenuePsWd="mypassword")

#SET SECURITY TOKEN (ADDITIONAL INFO FOR PULLING NHS TRACK & TRACE INFO

Sys.setenv(securityToken="1359977526")
