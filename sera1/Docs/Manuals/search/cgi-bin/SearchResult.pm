#!/usr/bin/perl -w

# Define a module
 
package SearchResult;
use strict;
 
##################################################
## the object constructor (simplistic version)  ##
##################################################
sub new
{ 
    my $self = {};

    $self->{filename}    = undef;
    $self->{url}         = undef;
    $self->{title}       = undef;
    $self->{description} = undef;
    $self->{score}       = 0;

    bless ($self);
    return $self;
}

1;  # so the require or use succeeds



    
