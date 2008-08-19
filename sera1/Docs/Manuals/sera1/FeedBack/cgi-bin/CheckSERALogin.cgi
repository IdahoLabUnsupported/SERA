#!/usr/local/bin/perl
###############################################################
#                                                             #
# password.cgi                                                #
#                                                             #
# This perl script runs when a user enters a username and     #
# password to enter the SERA Problem Reporting Form.          #
#                                                             #
# Matt Cohen                                                  #
# June 1, 1999                                                #
#                                                             #
###############################################################

push(@INC,"/cgi-bin");
require("cgi-lib.pl");

######### Begin password.cgi ###########

# Get the input from the password page
&ReadParse(*input);

# Declare and initialize flag
$correct = 0;

# Define the page they should go to if the password is correct:
$right_location = "http://www.cs.montana.edu/~bnct/manual/FeedBack/SERAProblemReport.html";

# Define the page they should go to if the password is wrong:
$wrong_location = "http://www.cs.montana.edu/~bnct/manual/FeedBack/SERALoginSorry.html";

# No need to change anything after this point, but feel free to mess around. 
# You might want to try replacing the 'print "Location: $right_or_wrong_location\n\n";' 
# with embeded HTML contents in the way that Perl Dynamic Anything does.

open(FILE,"<lists/SERA_1B0_login.list");

while ($line = <FILE>)
{
    chomp($line);
    ($username, $password) = split(" ", $line);

    if (($username eq $input{'username'}) && ($password eq $input{'password'})) 
    {
	$correct = 1;
    }
}

close(FILE);

if ( $correct )
{
    print "Location: $right_location?$input{'username'}\n\n";
}
else 
{
    print "Location: $wrong_location?$input{'username'}\n\n";
}

########## end password.cgi #############


