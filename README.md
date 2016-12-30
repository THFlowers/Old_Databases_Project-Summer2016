# Old_Databases_Project-Summer2016
Racket and Sql code for main project of FSU "Introduction to Databases" class, taught by D. Gaitros in Summer 2016.
Placeholder images are modified assets from Cosmic Cat Comics (http://www.mailmemycomics.com/) used without permission for academic purposes.

##About

This project was a locally hosted (rough) immitation of an online pull-list maintainance system reffered to as "CosmonautKittens".  It was implemented in Racket, but mostly follows Scheme idioms while using some of Racket's more convenient libraries.  These libraries include it's quoted mysql queries, xml/html creation via sexp, clean custom formlet and built in web-server libraries.  Since this project is un-idiomatic, quick-and-dirty, and mixes html, Racket, MySQL, and some javascript, it can become difficult to read and have excessive levels of nesting.

I am particularily proud of the rather simple function generic-report-table, without it the program would be thousands of lines longer, and the contents of each cell can be easily customized as necessary (as demonstrated in most admin pages).

I regret not learning how to use Racket macros and modules during this project as this could have been seperated into many easy to read files, and much redundant boilerplate (and near boilerplate) could have been replaced with generating macros.

##Installation and Execution
###Pre-reqs
* Install a recent version of Racket
* Install a compatible version of MySQL

###Set-up MySQL requirements
* Create a new Database named ComicShop with full access user "Ron" and set the password as you like (or modify database-login to use whatever name you choose
* Run DB.sql with ComicShop as your database and using the previously created username and password

###Edit source as necessary
* Download this project into a directory you have read/write access to
* Edit line 19 (working-dir) and the final line (#:extra-files-paths) to refer to that location
* Optionally, set launch-browser to #t (true) if you want the website to auto-start when you run the program.

###Adjust network settings
* Set your computer to alias CosmonautKittens.com to localhost
* Set firewall to allow local communication over port 8080 if necessary

###Run within DrRacket
* Open file in DrRacket and click run
* Type the username/password set previously
* Access the site via CosmonautKittens.com:8080
