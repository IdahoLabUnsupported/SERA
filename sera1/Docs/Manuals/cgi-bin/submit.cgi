#!/usr/local/bin/perl
###############################################################
#                                                             #
# report.cgi                                                  #
#                                                             #
# This perl script runs when a user submits a SERA bug report #
#                                                             #
# Matt Cohen                                                  #
# June 1, 1999                                                #
#                                                             #
###############################################################

push(@INC,"/cgi-bin");
require("cgi-lib.pl");

######### Begin report.cgi ###########

# Define the recipient list
$recipient_file = "data/recipients.dat";

# Define which mail program to use
$mailprog = '/usr/sbin/sendmail';

# Define username flag
$valid_username = 0;

# Get the input from the form
&ReadParse(*input);

# Send email to the users listed in recipients.list
&send_mail;

# Send a copy of the report to the user
&send_a_copy($input{'email'});

# Print a thank you to the user for their submission
&user_thank_you;


#==============================================================
# Procedure:  user_thank_you
#
# Purpose:    Prints out a thank you web page to the user
#
# Parameters: None.
#
# Returned:   None.
#==============================================================
sub user_thank_you
{
    print "Content-type: text/html\n\n";
    print "<HTML>\n";
    print "<BODY bgcolor=\"#ccbbaa\" background = \"../graphics/mainbackground.gif\"><BR>\n";

    print "<H1><CENTER>Thank you $input{'firstName'} $input{'lastName'}</CENTER></H1><BR>\n";
    
    print "<CENTER><I>Your report has been received.<BR>\n";  
    print "If you entered your email address correctly,<BR>\n";
    print "you should receive a copy of the problem you reported.<BR><BR>\n";
    print "-- The SERA Development Team </I>\n";

    print "<BR><BR><H3><A HREF=\"http://www.cs.montana.edu/~bnct/right.html\">HOME</A></H3></CENTER>\n";

    print "</BODY></HTML>\n";
}


#==============================================================
# Procedure:  send_mail
#
# Purpose:    Opens "recipients.list" and sends a copy of the
#             users report to each person in the file.
#
# Parameters: None.
#
# Returned:   None.
#==============================================================
sub send_mail
{
    open(FILE,"<$recipient_file");

    while ( $line = <FILE>)
    {
	chomp;   # Remove the \n character

	if ( length($line) > 2 )
	{
	    send_a_copy($line);
	}
    }

    close(FILE);
}


#==============================================================
# Procedure:  send_a_copy
#
# Purpose:    Sends a copy of the report to one person.
#
# Parameters: Address of person to send mail to.
#
# Returned:   None.
#==============================================================
sub send_a_copy
{
    open(MAIL,"|$mailprog -t");

    print MAIL "To: @_\n";      # @_ is the address of the person
    print MAIL "From: $input{'email'}\n";
    print MAIL "Subject: SERA Problem Reporting Form\n\n";
    
    print MAIL "============================================================================\n";
    print MAIL "||                           USER INFORMATION                             ||\n";
    print MAIL "============================================================================\n";
    print MAIL "Date: $input{'date'}\n\n";

    print MAIL "Name: $input{'lastName'}, $input{'firstName'}\n";
    print MAIL "Organization: $input{'organization'}\n";
    print MAIL "Address: ";
    
    # Do a little formatting with the address1 and address2 fields
    if ( length("$input{'address1'}") )
    {
	print MAIL "$input{'address1'}\n";

	if ( length("$input{'address2'}") )
	{
	    print MAIL "         $input{'address2'}\n";
	}
    }
    else
    {
	if ( length("$input{'address2'}") )
	{
	    print MAIL "$input{'address2'}\n";
	}
    }

    print MAIL "City: $input{'city'}      State: $input{'state'}\n";
    print MAIL "Country: $input{'country'}      Zip: $input{'zip'}\n\n";

    print MAIL "Telephone: $input{'telephone'}      Fax: $input{'fax'}\n";
    print MAIL "E-mail: $input{'email'}\n\n";
    
    print MAIL "============================================================================\n";
    print MAIL "||                          COMPUTER INFORMATION                          ||\n";
    print MAIL "============================================================================\n";
    print MAIL "Computer: $input{'computer'}\n";
    print MAIL "System Memory: $input{'memory'}\n";
    print MAIL "Operating System: $input{'operatingSystem'}\n\n";

    print MAIL "============================================================================\n";
    print MAIL "||                          PROBLEM INFORMATION                           ||\n";
    print MAIL "============================================================================\n";
    print MAIL "Module: $input{'module'}      Version: $input{'version'}\n";
    print MAIL "Error Replication: $input{'errorRepeat'}\n\n";

    print MAIL "Location of Any Files Necessary to Duplicate Problem:\n";
    print MAIL "-----------------------------------------------------\n";
    print MAIL "$input{'files'}\n\n";

    print MAIL "Problem Description:\n";
    print MAIL "--------------------\n";
    print MAIL "$input{'problemDescription'}\n\n";

    print MAIL "User Impact Priority: $input{'priority'}\n";
    print MAIL "User Impact Description:\n";
    print MAIL "------------------------\n";
    print MAIL "$input{'userImpact'}\n\n";

    print MAIL "============================================================================\n";
    print MAIL "||                       TO BE COMPLETED BY GROUP                         ||\n";
    print MAIL "============================================================================\n\n";

    print MAIL "U.P.# __________________\n\n";

    print MAIL "Resolution: ________________________________________________________________\n\n";

    print MAIL "____________________________________________________________________________\n\n";

    print MAIL "____________________________________________________________________________\n\n";

    print MAIL "____________________________________________________________________________\n\n";

    print MAIL "Cognizant Engineer: ________________________________________________________\n\n";

    print MAIL "Code Version Correction/Upgrade Status: Date: ___________ Vers. # __________\n";


    
    close(MAIL);
}






