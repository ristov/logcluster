package LogClusterUtils;

# Sample LogCluster functions for preprocessing input data and creating
# word classes. The functions have been designed for use with LogCluster
# --lcfunc and --wcfunc options, for example:
# --wcfunc='require "LogClusterUtils.pm"; sub { LogClusterUtils::wordclass(@_) }'

use strict;
use warnings;

require Exporter;
our @ISA = qw(Exporter);

our $VERSION = 1.00;
our @EXPORT_OK = qw(bsdsyslog wordclass);

# Functions for converting input lines

sub bsdsyslog {
  my($line) = $_[0];
  my($timestamp, $host, $prog, $msg);

  if ($line =~ /^[[:upper:]][[:lower:]]{2} [ \d]\d (?:\d\d:){2}\d\d /g) {
    $timestamp = "BSDsyslog-timestamp";
  } elsif ($line =~ /^\d{4}(?:-\d\d){2}T(?:\d\d:){2}\d\d(?:\.\d+)?(?:Z|[+-]\d\d:\d\d) /g) {
    $timestamp = "ISO8601-timestamp";
  } else {
    return undef;
  }

  if ($line =~ /\G([\w.-]+) \S+ ([\w\/.-]+)(\[\d+\])?: (?:\[ID \S+ \S+\] )?(.+)/) { 
    $host = $1;
    $msg = $4;
    if (defined($3)) { 
      $prog = "$2\[PID\]";
    } else {
      $prog = $2;
    }
    return "$timestamp $host $prog: $msg";
  } else {
    return undef;
  } 
}

# Functions for creating word classes

sub convert_value {
  my($value) = $_[0];
  my($comp, $sep, $result);

  $result = "";

  while ($value =~ /\G(.*?)([[:punct:]]+|$)/g) {
    $comp = $1;
    $sep = $2;
    if ($comp =~ /^\d+$/) { $comp = "DIGITS"; }
    elsif ($comp =~ /^[[:alpha:]]+$/) { $comp = "LETTERS"; }
    elsif ($comp =~ /^[[:alnum:]]+$/) { $comp = "ALNUMS"; }
    $result .= $comp . $sep;
  }

  return $result;
}

sub wordclass {
  my($word) = $_[0];
  my($result, @result);

  if (/^([\w\/.-]+)\[\d+\]:$/) {
    push @result, $1 . "[PID]:";
  } else {
    $result = $word;
    $result =~ s/(?<!\.)\b(?:\d{1,3}\.){3}\d{1,3}\b(?!\.)/IP-address/g;
    $result =~ s/[\w.-]+@[\w.-]+/Email-address/g;
    if ($result ne $word) { push @result, $result; }
  }

  if ($word =~ /^(\w+)=(.+)/) {
    $result = $1 . "=" . convert_value($2);
    if ($result ne $word) { push @result, $result; }
    if ($2 ne "VALUE") { push @result, $1 . "=VALUE"; }
  } else {
    $result = convert_value($word);
    if ($result ne $word) { push @result, $result; }
  }

  return @result;
}

1;
