#!/usr/bin/perl
##############################################################################

##############################################################################
push(@INC,"/cgi-bin");
require("cgi-lib.pl");
use SearchResult;
##############################################################################

$STRING_SCORE = 100;
$ALL_SUBSTRINGS_SCORE = 50;
$SUBSTRING_SCORE = 25;
$MATCHES_FOUND = 0;

# Get the input from the form
&ReadParse(*input);

# Set up web page printing
print "Content-type: text/html\n\n";

&get_search_info;
&load_resources;
&get_files;
&search_files;
&scale_results;
&bubble_sort_results;
&print_web_page;
#&print_results;

sub load_resources
{
    my $temp;

    open ( FILE, "<data/search.rc" );

    while ( <FILE> )
    {
	# Remove the \n character
	chomp;

	if ( /.\#/ )
	{
	    # Skip lines starting with '#'
	    # print "Skipping line $_<BR>\n";
	}
	elsif ( /baseDirectory/ )
	{
	    ( $temp, $baseDirectory ) = split ( /\s*:=\s*/, $_ ); 
	}
	elsif ( /baseURL/ )
	{
	    ( $temp, $baseURL ) = split ( /\s*:=\s*/, $_ ); 
	}
	elsif ( /searchURL/ )
	{
	    ( $temp, $searchURL ) = split ( /\s*:=\s*/, $_ ); 
	}
	elsif ( /fileTypes/ )
	{
	    my $fileTypes;

	    ( $temp, $fileTypes ) = split ( /\s*:=\s*/, $_ ); 

	    # split the list of files up.  One or more space deliminates them.
	    @fileTypeList = split ( /\s+/, $fileTypes );
	}
	elsif ( /omitDirectories/ )
	{
	    my $fileTypes;

	    ( $temp, $omitDirs ) = split ( /\s*:=\s*/, $_ ); 

	    # split the list of directorys to omit up.  One or more space deliminates them.
	    @omitDirList = split ( /\s+/, $omitDirs );
	}
	else
	{
	    # skip blank lines
	    # print "Skipping blank line<BR>\n";
	}
    }

    close ( FILE );
}

sub scale_results
{
    my $score;
    my $max = 1;
    my $count = 0;
    my $tempResult;

    # Find the largest score
    foreach $tempResult ( @ALL_MATCHES )
    {
	if ( $tempResult->{score} > $max )
	{
	    $max = $tempResult->{score};
	}
    }

    # Scale all scores based on the largest score
    foreach $tempResult ( @ALL_MATCHES )
    {
	$tempResult->{score} = int ( $tempResult->{score}/$max*100 + 0.5 );
    }
}
    

sub print_web_page
{    
    my $tempResult;

    print "<HTML>\n<HEAD>\n";
    print "<TITLE>Search Results</TITLE>\n</HEAD>\n";
    print "<BODY bgcolor=\"#ccbbaa\" background=\"../graphics/mainbackground.gif\">\n";

    print "<H1>$MATCHES_FOUND matches found for <I>$SEARCH_STRING</I></H1>";

    if ( scalar ( @ALL_MATCHES ) == 0 )
    {
	print "<BR><BR><BR><BR>\n";
	print "<FONT SIZE=+1><I>Sorry, Your search produced no results.</I></FONT>\n";
    }
    else
    {
	foreach $tempResult ( @ALL_MATCHES )
	{
	    print "<FONT SIZE=+1><BR><FONT COLOR=RED>$tempResult->{score}% </FONT>\n";
	    print "<A HREF=\"$tempResult->{url}\">$tempResult->{title}</A></FONT>\n<BR>\n";
	    print "$tempResult->{description}<BR>\n";
	}
    }

    print "</BODY></HTML>\n";
}


# Debugging procedure
sub print_results
{
    my $tempResult;

    print "Base Directory: $baseDirectory\n";
    print "Base URL: $baseURL\n";
    print "File Types: $fileTypes\n\n";
    
    foreach $tempResult ( @ALL_MATCHES )
    {
	print "$tempResult->{score}  $tempResult->{filename}\n";
    }
}

sub get_files
{
    # define local variables
    my $list;
    my @LIST;
    my $tempFile;
    
    # Change the working directory
    chdir($baseDirectory);

    # Get list of files with the given file description
    $list = `ls -R`;
    
    # Make list into an array
    @LIST = split ( /\s+/, $list );
	
    # For each file in the LIST, add to list of filenames
    foreach $tempFile ( @LIST )
    {
	# add to the list of all files
	push ( @ALL_FILES, $tempFile );
    }
}

sub get_search_info
{
    $SEARCH_LOGIC = $input{"searchLogic"};
    $SEARCH_STRING = $input{"search"};
    
    # Remove any quotes from search string
    $SEARCH_STRING =~ s/\"//g;

    # $SEARCH_STRING = "bnct";
    @SUB_SEARCH_STRINGS = split ( /\s+/, $SEARCH_STRING );    
}


sub search_files
{
    # Local variable
    my $tempFile;
    # First directory will be the local directory
    my $subDirectory = "";

    # Look at each result of the `ls -R`
    foreach $tempFile ( @ALL_FILES )
    {
	# If the file is "./somedirectory:", then is it a subdirectory
	if ( $tempFile =~ /\.\/(.+):/ )
	{
	    $subDirectory = "$1/";
	    next;
	}
	# If the length is less than 1, continue
	elsif ( length ( $tempFile ) < 1 )
	{
	    next;
	}

	my $omitDir;
	my $blockedDir = 0;

	# Check if the current subdirectory is blocked from search
	foreach $omitDir ( @omitDirList )
	{
	    if ( $subDirectory =~ /^$omitDir/ )
	    {
		$blockedDir = 1;
		last;
	    }
	}
	if ( $blockedDir )
	{
	    next;
	}

	# Define local variables
	my $string;
	my $score = 0;
	my $fileOK = 0;
	my $substrings_matched = 0;
	my $tempFileType;
	my $complete_filename = "$baseDirectory$subDirectory$tempFile";

	# Is this a specified file type?
	foreach $tempFileType ( @fileTypeList )
	{
	    if ( $tempFile =~ /$tempFileType$/ )
	    {
		$fileOK = 1;
		last;
	    }
	}

	# Search this file?
	if ( $fileOK == 0 )
	{
	    next;
	}

	# Open and read file
	open ( FILE, "<$complete_filename" ) or warn "Could not open $complete_filename<BR>\n";
	@LINES = <FILE>;
	close ( FILE );

	$string = join ( ' ', @LINES );
	$string =~ s/\n//g;

	while ( $string =~ /$SEARCH_STRING/ig )
	{
	    $score += $STRING_SCORE;
	}

	# If not just looking for exact phrase
	if ( $SEARCH_LOGIC =~ /or/ )
	{
	    foreach  $subString ( @SUB_SEARCH_STRINGS )
	    {
		my $count = 0;

		while ( $string =~ /$subString/ig )
		{
		    $count++;
		    $score += $SUBSTRING_SCORE;
		}
	    
		if ( $count )
		{
		    $substrings_matched++;
		}
	    }
	}
	    
	if ( $substrings_matched == scalar ( @SUB_SEARCH_STRINGS ) )
	{
	    $score += $ALL_SUBSTRINGS_SCORE;
	}
	
	if ( $score )
	{
	    # Get a new SearchResult object
	    my $tempResult = SearchResult->new();

	    $MATCHES_FOUND++;

	    $tempResult->{filename} = $tempFile;
	    $tempResult->{score} = $score;
	    $tempResult->{url} = "$baseURL$subDirectory$tempFile";

	    # Get the title
	    if ( $string =~ /<title>(.+)<\/title>/is )
	    {
		$tempResult->{title} = $1;
	    }
	    else
	    {
		$tempResult->{title} = $tempFile;
	    }

	    # Get the description
	    # This looks for: "<meta", 0 or more spaces, "http-equiv" or "name", 
	    #                 0 or more spaces, "=", 0 or more spaces, 0 or 1 quote, 
	    #                 "description", 0 or 1 quote, 0 or more spaces, "=",
	    #                 0 or more spaces, a quote.
	    # Then copies everything until the closing quote into $2
	    if ( $string =~ /<meta\s*(http-equiv|name)\s*=\s*"?description"?\s*content\s*=\s*"([^"]+)"/isx ) # Closing quote "
	    {
		$tempResult->{description} = $2;
	    }
	    else
	    {
		$tempResult->{description} = "No description available.";
	    }

	    push ( @ALL_MATCHES, $tempResult );
	}
    }
}


sub bubble_sort_results
{
    my $count = scalar ( @ALL_MATCHES );
    my $i;
    my $j;

    for ( $i = 0; $i < $count; $i++ )
    { 
	for ( $j = $count - 1; $j > $i; $j-- )
	{
	    if ( $ALL_MATCHES[$j]->{score} > $ALL_MATCHES[$j - 1]->{score} )
	    {
		&swap_results ( $j, $j - 1 );
	    }
	}
    }
}


sub swap_results
{
    my $tempResult = SearchResult->new();
    my ($a, $b) = ($_[0], $_[1]);

    $tempResult->{filename} = $ALL_MATCHES[$a]->{filename};
    $tempResult->{url} = $ALL_MATCHES[$a]->{url};
    $tempResult->{title} = $ALL_MATCHES[$a]->{title};
    $tempResult->{description} = $ALL_MATCHES[$a]->{description};
    $tempResult->{score} = $ALL_MATCHES[$a]->{score};

    $ALL_MATCHES[$a]->{filename} = $ALL_MATCHES[$b]->{filename};
    $ALL_MATCHES[$a]->{url} = $ALL_MATCHES[$b]->{url};
    $ALL_MATCHES[$a]->{title} = $ALL_MATCHES[$b]->{title};
    $ALL_MATCHES[$a]->{description} = $ALL_MATCHES[$b]->{description};
    $ALL_MATCHES[$a]->{score} = $ALL_MATCHES[$b]->{score};

    $ALL_MATCHES[$b]->{filename} = $tempResult->{filename};
    $ALL_MATCHES[$b]->{url} = $tempResult->{url};
    $ALL_MATCHES[$b]->{title} = $tempResult->{title};
    $ALL_MATCHES[$b]->{description} = $tempResult->{description};
    $ALL_MATCHES[$b]->{score} = $tempResult->{score};
}
