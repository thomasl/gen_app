#!perl -W
## Poor man's beefing up of sed
## 
## Give substs as
##   -e SUBSTVAR FILE
## 
## Every occurrence of SUBSTVAR will be replaced by uninterpreted
## contents of FILE.
## 
## For instance:  bigsubst.pl -e '%OVID_MODS%' ../ebin/ovid-mods.tmp -file F
## 
## We use a hash for the -e pairs, but we could have done equally well
## by just traversing the array 2-wise. Oh well.
##
## LIMITATIONS
## - since files are read into memory, they shouldn't be huge
##   (but can be much larger than what sed handles)

use Getopt::Long;
use strict;
use warnings;

my @valargs;
my @fileargs;
my @inputfiles;

GetOptions(
    'e=s{2}' => \@valargs,
    'x=s{2}' => \@fileargs,
    'i=s' => \@inputfiles
);

my %h1 = @fileargs;
my %h2 = @valargs;
my %args;

my $file = $inputfiles[0];

print "Input file is '".$file."'\n";

unless (defined $file) {
    print STDERR "-i inputfile required\n";
    exit 1;
};

## Read each substfile into memory

foreach my $key (keys %h1) {
    my $substfile = $h1{$key};
    my $subst = do { local $/; local @ARGV = ($substfile); <> };
    $args{$key} = $subst;
}

## Copy raw values

foreach my $key (keys %h2) {
    $args{$key} = $h2{$key};
}

## Read the input file, perform substitutions

my $input = do { local $/; local @ARGV = ($file); <> };

foreach my $key (keys %args) {
    my $val = $args{$key};
    $input =~ s!$key!$val!g;
}

print $input;
