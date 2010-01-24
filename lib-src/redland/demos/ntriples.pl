#!/usr/bin/perl -Tw
#
# ntriples.pl - Redland N-Triples validator demo
#
# Copyright (C) 2002-2004, David Beckett http://www.dajobe.org/
# Copyright (C) 2000-2004, University of Bristol, UK http://www.bristol.ac.uk/
# 
# This package is Free Software and part of Redland http://librdf.org/
# 
# It is licensed under the following three licenses as alternatives:
#   1. GNU Lesser General Public License (LGPL) V2.1 or any newer version
#   2. GNU General Public License (GPL) V2 or any newer version
#   3. Apache License, V2.0 or any newer version
# 
# You may not use this file except in compliance with at least one of
# the above three licenses.
# 
# See LICENSE.html or LICENSE.txt at the top of this package for the
# complete terms and further detail along with the license texts for
# the licenses in COPYING.LIB, COPYING and LICENSE-2.0.txt respectively.
# 
# 
#

# CHANGE THIS FOR YOUR CONFIGURATION
$::ROOT_DIR='/somewhere';

use strict;

# Helps with broken web requests (missing headers)
$ENV{'Content-Length'}||=0;

# Tainting, dontcha know
$ENV{'PATH'}="/bin:/usr/bin:/usr/local/bin:$::ROOT_DIR/bin/";

delete $ENV{'BASH_ENV'};

# Standard perl modules
use CGI;
use LWP::Simple;
use URI::URL;


# Configuration

my $tmp_dir="$::ROOT_DIR/tmp";
my $log_file="$::ROOT_DIR/logs/ntriples.log";

my $max_stream_size=200;
my $max_error_size=100;

my(@parameters)=qw(uri);

# Redland perl modules

use RDF::Redland;
use RDF::Redland::RSS;



######################################################################
# Subroutines

sub log_action ($$;$) {
  my($host, $message, $now)=@_;
  $now ||= time;
  return unless open (LOG, ">>$log_file");
  my($sec,$min,$hour,$mday,$mon,$year)=gmtime $now;
  my $date=sprintf("%04d-%02d-%02dT%02d:%02d:%02dZ",1900+$year,$mon+1,$mday,$hour,$min,$sec);
  print LOG "$host $date $message\n";
  close(LOG);
}

sub end_page($) {
  my $q=shift;

print <<"EOT";

<h2>About the validator</h2>

<p>This was written using
<a href="http://librdf.org/">Redland</a>
and the
<a href="http://librdf.org/docs/pod/RDF/Redland/Parser.html">RDF::Redland::Parser</a> 
<a href="http://librdf.org/docs/perl.html">Perl</a> interface
to the <a href="http://librdf.org/raptor/">Raptor</a>
N-Triples parser.</p>

<p>The source code of this demonstration is available in the Redland
distribution as <tt>demos/ntriples.pl</tt> or from the
<a href="http://librdf.org/">Redland</a> website</p>

EOT


  print qq{<hr />\n\n<p class="copyright"><a href="http://www.dajobe.org/">Dave Beckett</a></p>\n\n</body></html>};
}


sub format_body($) {
  my $string=shift;
  # No need for HTML::Entities here for three things
  $string =~ s/\&/\&amp;/g;
  $string =~ s/</\&lt;/g;
  $string =~ s/>/\&gt;/g;
  $string;
}

sub format_attr($) {
  my $string=format_body(shift);
  $string =~ s/"/\&quot;/g; #"
  $string;
}

sub format_literal ($) {
  my($string)=@_;
  return 'UNDEFINED' if !defined $string;

  my $new_string='';
  for my $c (split(//, $string)) {
    if(ord($c) <0x20 || ord($c) == 0x7e || $c eq '\\' || $c eq '"') {
      $new_string.=sprintf("\\x%02X",ord($c));
    } else {
      $new_string.=$c;
    }
  }
  return format_body($new_string);
}

sub format_url($) {
  my $url=shift;
  my $a_url= format_attr($url);
  my $q_url= format_body($url);
  qq{<a href="$a_url">$q_url</a>};
}

sub format_node ($) {
  my $node=shift;
  my $type=$node->type;
  if($type == $RDF::Redland::Node::Type_Resource) {
    my $uri=$node->uri->as_string;
    return qq{URI <a href="$uri">$uri</a>};
  } elsif ($type == $RDF::Redland::Node::Type_Literal) {
    my $str=format_literal($node->literal_value).'"';
    my $is_xml=$node->literal_value_is_wf_xml;
    $str=$is_xml ? 'XML Literal: "'.$str : 'UTF-8 Literal: "'.$str;
    my $lang=$node->literal_value_language;
    $str.=" (Language: $lang)" if $lang;
    return $str;
  } elsif ($type == $RDF::Redland::Node::Type_Blank) {
    my $id=$node->blank_identifier;
    return qq{BNodeID $id};
  } else {
    return $node->as_string;
  }
}

######################################################################
my $q = new CGI;

# CGI parameter paranoia
my $val;

my $uri_string;
$val=$q->param('uri');
if(defined $val && $val =~ /^([ -~]+)$/) {
  $uri_string=$1;
} else {
  $uri_string='';
}

my $empty=(!$uri_string);

# Zap remaining parameters
$q->delete_all;

# End of parameter decoding


# Used in logging
my $host=$q->remote_host;


######################################################################
# Emit content

print $q->header(-type => 'text/html', -charset=>'utf-8');

# Always print header
print <<"EOT";
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" lang="en" xml:lang="en">
<head>
  <title>Redland N-Triples Validator</title>
  <!-- HTML STYLE -->
</head>
<body>

<!-- LOGO START -->
         <h1>Redland N-Triples Validator</h1>
<!-- LOGO END -->

<p>Enter the address of
<a href="http://www.w3.org/TR/rdf-testcases/#ntriples">N-Triples</a>
content into the following form and it will be validated
and formatted for display.

EOT


# Restore values
$q->delete_all;

$q->param('uri', $uri_string) if $uri_string;

# use q->url() to get URL of this script without any query parameters
# since we are using a POST here and don't want them added to the
# submission URL.
my $action_url="/".$q->url(-relative=>1);

print $q->start_form(-name => 'myform', -method=>'GET', -action => $action_url),"\n";

print "<p>N-Triples URI: ";
print $q->textfield(-name=>'uri',
		    -default=>'',
		    -size=>60,
		    -maxlength=>1024);
print '&nbsp;', $q->submit('Go');

print "</p>\n\n";

print $q->endform,"\n\n";

print <<"EOT";
<p><b>Data Privacy:</b> IP addresses and content URIs are logged
and may be used for testing.</p>

EOT

# Any parameters?
if($empty) {

  print <<"EOT";
<p>You might want to try this N-Triples test case from the
RDF Core test suite:
http://www.w3.org/2000/10/rdf-tests/rdfcore/ntriples/test.nt
</p>
EOT

  end_page($q);
  exit 0;
}


######################################################################

print "<h2>Results of N-Triples Validation</h2>\n";


# Validate me

my $uri;
eval "\$uri=new URI::URL(q{$uri_string});" if $uri_string;
if($@ || !$uri) {
  print qq{\n\n<p>URI <a href="$uri_string">$uri_string</a> is not a legal URI (according to perl)</p>\n};
  end_page($q);
  exit 0;
}

if(!$uri->scheme || $uri->scheme ne 'http') {
  print "\n\n<p>Cannot use URI $uri_string - must be a web http URI.</p>\n";
  end_page($q);
  exit 0;
}

my $source_uri=new URI::URL $uri;

# Must fetch and copy to temp file
my $temp_file;
$temp_file="$tmp_dir/rss-demo-$$.rss";
my $rc=getstore($uri_string, $temp_file);
if(!is_success($rc)) {
  print "\n\n<p>Failed to read URI $uri_string - HTTP error $rc</p>\n";
  end_page($q);
  exit 0;
}
if(open(IN, $temp_file)) {
  my $content=join('', <IN>);
  close(IN);
}
$source_uri=new URI::URL("file:$temp_file");


my(@errors)=();
RDF::Redland::set_error_handler(sub {
  my $msg=shift;
  push(@errors, $msg);
});



my $parser=new RDF::Redland::Parser("ntriples");
if(!$parser) {
  print "\n\n<p>Failed to create N-Triples parser.</p>\n";
  end_page($q);
  unlink $temp_file if $temp_file;
  exit 0;
}

my $redland_base_uri=new RDF::Redland::URI $uri;
my $redland_source_uri=new RDF::Redland::URI $source_uri;

log_action($host,"Parsing N-Triples URI $uri", time);
my $stream=$parser->parse_as_stream($redland_source_uri, $redland_base_uri);
if(!$stream || $stream->end) {
  print "\n\n<p>URI \"$uri\" failed to parse URI $uri as N-Triples.</p>\n";
}

my $count=0;
if($stream && !$stream->end) {

  print "<h2>Triples</h2>\n";

  print <<"EOT";
<center>
<table style="text-align:center" border="1">
<tr align="left">
<th>Count</th>
<th>Subject</th>
<th>Predicate</th>
<th>Object</th>
</tr>
EOT


  for(; $stream && !$stream->end; $stream->next) {
    my $statement=$stream->current;

    my $subject=format_node($statement->subject);
    my $predicate=format_node($statement->predicate);
    my $object=format_node($statement->object);

    my $id=$count+1;
    print << "EOT";
<tr align="left">
<td>$id</td>
<td>$subject</td>
<td>$predicate</td>
<td>$object</td>
</tr>
EOT

    $count++;

    if ($count == $max_stream_size) {
      my $cur=$count+1;
      while(1) {
	$stream->next;
	last if $stream->end;
	$count++;
      }
      print << "EOT";
<tr align="left">
<td>$cur...$count</td><td colspan="3">Truncated at $max_stream_size to limit table / page size</td>
</tr>
EOT
      last;
    }
  }

  print <<"EOT";
</table>
</center>

<p>Note: \\x<em>HH</em> where <em>HH</em> are hexadecimal digits
is used to indicate escaped characters such as \\, &quot; or
non-printable characters such as tabs and newlines.</p>
EOT

  $stream=undef;
}

if(@errors) {
  print "<h2>Errors</h2>\n\n<p>";

  my $error_count=1;
  for my $error (@errors) {
    $error =~ s/URI $uri_string:/line /;
    $error =~ s/- Raptor error//;
    print $error,"<br />\n";
    $error_count++;
    if ($error_count > $max_error_size) {
      print "</p>\n\n<p>Remaining errors $error_count..",scalar(@errors)," truncated to limit page size";
      last;
    }
  }
  print "</p>";
}


if(!$count) {
  end_page($q);
  #unlink $temp_file if $temp_file;
  exit 0;
}

my $error_count=scalar(@errors);
my $pl=($count != 1) ? 's' : '';
my $errorpl=($error_count != 1) ? 's' : '';
print "\n\n<p>URI \"$uri\" parsed as N-Triples giving $count triple$pl and $error_count error$errorpl</p>\n";

#unlink $temp_file if $temp_file;

end_page($q);
exit 0;
