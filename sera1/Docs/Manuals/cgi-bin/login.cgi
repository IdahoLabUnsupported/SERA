#!/usr/bin/perl
###############################################################
#                                                             #
# login.cgi                                                   #
#                                                             #
# Matt Cohen                                                  #
# March, 2000                                                 #
#                                                             #
###############################################################
use File::Copy;

# Get the input from the password page
&read_input;

# List of links for a correct login
$login_correct_html = "
<CENTER><H3>Welcome to the BNCT secure pages</H3></CENTER>
<BR>
<LI><A HREF=\"pages/updates/UpdateSERA1C0.html\">SERA Update Page</A><BR>
<LI><A HREF=\"pages/feedback/SERAProblemReport.html\">SERA Feedback Form</A>";

my (
    %INPUT_FORM,
    $date,
    $time,
    $title,
    $html,
    $FORM_password,
    @username_list,
    @password_list,
    @exp_date_list
    );
$login_page = "../login.html";
$password_file = "data/pwd.dat";
$password_back = "data/pwd.dat.bak";
$temp_password_file = "data/password.temp.file";
$logfile = "data/login.log";
$NOT_VALID = 0;
$VALID = 1;
$PASSWORD_EXPIRED = 2;
$CHANGE_PASSWORD = 3;
$UPDATE_PASSWORD = 4;
$INITIAL_LOGIN = 5;
$result = $NOT_VALID;
$input_form = "<FORM action=\"login.cgi\" method=\"POST\">
<INPUT type=hidden name=mode value=\"update\">
<TABLE align=center border=5 cellpadding=10>
  <TR>
    <TD><I>
      Username: <INPUT type=text name=username value=\"$INPUT_FORM{'username'}\" maxlength=\"30\" size=\"20\">
    </I></TD>
  </TR>
  <TR>
    <TD><I>
      Old Password: <INPUT type=password name=password value=\"\" maxlength=\"30\" size=\"20\">
    </I></TD>
  </TR>
  <TR>
    <TD><I>
      New Password: <INPUT type=password name=newPassword value=\"\" maxlength=\"30\" size=\"20\">
    </I></TD>
  </TR>
  <TR>
    <TD><I>
      Retype New Password: <INPUT type=password name=validatePassword value=\"\" maxlength=\"30\" size=\"20\">
    </I></TD>
  </TR>
</TABLE>
<CENTER>
<H3>
<I>
<INPUT type=submit value=\" Submit \">
<INPUT type=reset value=\" Clear \">
</I>
</H3>
</CENTER>
</FORM>";
$password_file_header = "# This is the password file for the secure pages
# of the bnct website.
#
# The order is like so:
# username::password::exp_date
#
# More specifically, the password is encrypted with
# md5sum.  The process to encrypt a password is as follows:
# 1. Make a file that is the password with a new line character.
# 2. Encrypt the file: > md5sum <thefile>
#
# The password expiration date is: yyyymmdd\n\n";

# Get the current date and time
&get_time;

# Convert the given password using checksum
$FORM_password = check_sum($INPUT_FORM{'password'});

# Read in the password file
&read_password_file;

# Variable "result" is set in this procedure to 
&check_username_and_password;

# Check the outcome of the username and password check
if ( $result == $VALID )
{
    &login_user;
}
elsif ( $result == $NOT_VALID )
{
    &bad_login;
}
elsif ( $result == $CHANGE_PASSWORD )
{
    &change_password;
}
elsif ( $result == $PASSWORD_EXPIRED )
{
    &password_expired;
}
elsif ( $result == $UPDATE_PASSWORD )
{ 
    &update_password;
}

&write_to_log;


# > -----------------------------------------------------------------------
# Read from standard in.
# -------------------------------------------------------------------------
sub read_input { 
	my(
		$input, 
		@pairs, 
		$pair, 
		$name, 
		$value
	);
	
	if ($ENV{'REQUEST_METHOD'} eq "GET") { 
    	$input = $ENV{'QUERY_STRING'};
		$get = 1;
	} 
	elsif ($ENV{'REQUEST_METHOD'} eq "POST") {
		read (STDIN, $input, $ENV{'CONTENT_LENGTH'});
		$post = 1;
	}
	else {
		#* Offline mode goes here.
	}
	
	@pairs = split(/&/, $input);
	
	foreach $pair (@pairs) {
	
		($name, $value) = split(/=/, $pair);
		
		$name =~ tr/+/ /;
		$name =~ s/%([a-fA-F0-9][a-fA-F0-9])/pack("C", hex($1))/eg;
		
		$value =~ tr/+/ /;
		$value =~ s/%([a-fA-F0-9][a-fA-F0-9])/pack("C", hex($1))/eg;

		#* Erase everything exept [0-9 a-z A-Z/-]
		$value =~ s/[^0-9a-z\/\-_]//gi;
		$name =~ s/[^0-9a-z]//gi;
		
		$INPUT_FORM{$name} = $value;				
	}
}

sub check_sum
{
    # create a file containing the password
    open ( FILE, ">$temp_password_file" );
    truncate ( FILE, 0 );
    print FILE "$_[0]\n";
    close ( FILE );

    # find the check sum of the file using md5sum (128 encryption)
    $ret_val = `/usr/bin/md5sum $temp_password_file`;
    ($ret_val) = split ( ' ', $ret_val );
    chomp ( $ret_val );

    # Remove the password from the file
    open ( FILE, ">$temp_password_file" );
    truncate ( FILE, 0 );
    close ( FILE );    

    return $ret_val;
}

sub read_password_file
{
    # Open the password file for reading/writing
    open(FILE,"<$password_file");

    while ( <FILE> )
    {
	chomp;

	if ( /\#/ ) {
            # Skip lines starting with '#'
        } else {
	    ($username, $password, $exp_date) = split("::", $_);
	    if ( (length($username) > 0)&&(length($password) > 0)&&(length($exp_date) > 0) )
	    {
		push ( @username_list, $username );
		push ( @password_list, $password );
		push ( @exp_date_list, $exp_date );
	    }
	}
    }

    close(FILE);
}  


# =====================================================================
# check_username_and_password
#
# Checks the username and password and also the expiration date.
# Sets the variable "result" depending on username, password, and date.
# =====================================================================
sub check_username_and_password
{
    my (
	$username,
	$password,
	$exp_date
	);

    # First check if the user choose to change the password
    if ( $INPUT_FORM{'mode'} eq "change" ) {
	$result = $CHANGE_PASSWORD;
	return;
    }

    $i = 0;
    foreach $username ( @username_list )
    {
	if ( ($username_list[$i] eq $INPUT_FORM{'username'}) && 
	     ($password_list[$i] eq $FORM_password) ) 
	{
	    if ($exp_date_list[$i] < $date) {
		$result = $PASSWORD_EXPIRED;
	    } else {
		$result = $VALID;
	    }
	}
	$i++;
    }

    # Mode will be "update_password" when this script is called by this script.
    # So, first we'll do the check for password validity here first.
    if ( ( $INPUT_FORM{'mode'} eq "update" ) && 
	 ( ( $result == $VALID ) || ( $result == $PASSWORD_EXPIRED ) ) )
    {
	$result = $UPDATE_PASSWORD;
    }
}


sub change_password
{
    $title = "Change Password";
    $html = "<CENTER>
<H3><BR>Enter your new password.</H3><BR>
</CENTER> $input_form";
    &write_html;
}

sub password_expired
{
    $title = "Password Expired";
    $html = "<CENTER>
<H3><BR>Your password has expired!<BR>
Please enter a new one.</H3>
</CENTER> $input_form";
    &write_html;
}


sub update_password
{
    my (
	$username,
	$password,
	$exp_date,
	$new_date
	);

    $title = "Update Results";
    
    # Make sure that the user types the password correctly twice
    if ( $INPUT_FORM{'newPassword'} eq $INPUT_FORM{'validatePassword'} ) {
	# Make sure that password is new
	if ( $FORM_password eq check_sum($INPUT_FORM{'newPassword'}) ) {
	    $html = "<CENTER><H3><BR>Sorry, your new password must be different than your old password.</H3><BR></CENTER> $input_form";
	} 
	# Make sure the password is long enough
	elsif ( length ( $INPUT_FORM{'newPassword'} ) < 5 ) {
	    $html = "<CENTER><H3><BR>Sorry, your password must be at least 5 characters long.</H3><BR></CENTER> $input_form";
	} else {
	    # Make a copy of the password file first
	    copy ( $password_file, $password_back );

	    open ( PWFILE, ">$password_file" );
	    truncate ( PWFILE, 0 );
	    print PWFILE "$password_file_header";

	    $i = 0;
	    foreach $usename ( @username_list )
	    {
		if ( ($username_list[$i] eq $INPUT_FORM{'username'}) && 
		     ($password_list[$i] eq $FORM_password) ) 
		{   
		    # Add ten years to the current date for the expiration date
		    $new_date = $date+100000;
		    $line = join("::", $username_list[$i], check_sum($INPUT_FORM{'newPassword'}), $new_date );
		} else {
		    $line = join("::", $username_list[$i], $password_list[$i], $exp_date_list[$i] );
		}

		print PWFILE "$line\n";
		$i++;
	    }

	    close ( PWFILE );

	    $html = "<CENTER><H3><BR>Password updated successfully.<BR></H3></CENTER>$login_correct_html";	    
	}
    } else {
	$html = "<CENTER><H3><BR>Sorry, your new password didn't match.  Try again.</H3><BR></CENTER> $input_form";
    }
    
    &write_html;
}


sub login_user
{
    $title = "SERA Secure Pages";
    $html = $login_correct_html;
    &write_html;
}


sub bad_login
{
    $title = "Password Expired";
    $html = "<CENTER>
<H3><BR>Sorry, that username and password is incorrect</H3><BR>
</CENTER>
<A HREF=\"$login_page\">BACK</A>";
    &write_html;
}


#Must have an extra return after Content-type line for the browser
sub write_html
{
print <<END_of_html;
Content-type: text/html

<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 3.2 Final//EN">
<HTML>
<HEAD>
<TITLE>$title</TITLE>
</HEAD>
<BODY bgcolor = "#ccbbaa" background = "../graphics/mainbackground.gif">
$html
</BODY>
</HTML>
END_of_html
}

# > -----------------------------------------------------------------------
# Get local time and do some formatting.
# -------------------------------------------------------------------------
sub get_time {
	my(
		$year, 
		$mon, 
		$day, 
		$hour, 
		$min, 
		$sec
	);

	($daynyear,$year,$mon,$day,$hour,$min,$sec) 
	= (localtime(time))[7,5,4,3,2,1,0];
	
	$mon++;
	
	$year = $year + 1900;
	
	if ($mon > 12) { #* Ops we got an error here.
		$mon = 0;
	}
	
	$date = sprintf(qq/%04d%02d%02d/,$year,$mon,$day);
	$time = sprintf("%02d:%02d:%02d",$hour,$min,$sec);
}


sub write_to_log
{
    open ( LOGFILE, ">>$logfile" );
    print LOGFILE "Date:     $date\n";
    print LOGFILE "Time:     $time\n";
    print LOGFILE "Host:     $ENV{REMOTE_HOST} \n";
    print LOGFILE "IP:       $ENV{REMOTE_ADDR} \n";
    print LOGFILE "Name:     $INPUT_FORM{'username'}\n";
    print LOGFILE "Action:   ";
    if ( $result == $VALID ) {
	print LOGFILE "Logged in\n";
    } elsif ( $result == $NOT_VALID ) {
	print LOGFILE "Login failed\n";
    } elsif ( $result == $PASSWORD_EXPIRED ) {
	print LOGFILE "Password expired notification\n";
    } elsif ( $result == $CHANGE_PASSWORD ) {
	print LOGFILE "Requested password change\n";
    } elsif ( $result == $UPDATE_PASSWORD ) {
	print LOGFILE "Changed password\n";
    } else {
	print LOGFILE "Error\n";
    }
    print LOGFILE "-" x 25;
    print LOGFILE "\n";
    close(LOGFILE);    
}

