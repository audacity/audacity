#!/usr/bin/perl -Tw
#
# parser-tests.pl - Redland OLD RDF Parser Tests Web Interface
#
# Copyright (C) 2001-2004, David Beckett http://www.dajobe.org/
# Copyright (C) 2001-2004, University of Bristol, UK http://www.bristol.ac.uk/
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
$ENV{'PATH'}="/bin:/usr/bin:/usr/local/bin";

# PT

# Standard perl modules
use CGI;
use LWP::Simple;
use URI::URL;
use HTML::Entities;
use Sys::Hostname;


# Configuration

my $RDF_NAMESPACE="http://www.w3.org/1999/02/22-rdf-syntax-ns#";
my $RDFS_NAMESPACE="http://www.w3.org/2000/01/rdf-schema#";
my $DC_NAMESPACE="http://purl.org/dc/elements/1.1/";
my $PT_NAMESPACE="http://www.ilrt.bristol.ac.uk/discovery/2001/03/parser-tests/schema#";

my $RDF_TESTS_ROOT_URI='http://www.w3.org/2000/10/rdf-tests/';

my $PT_TEST_URI_STRING=$PT_NAMESPACE."test";
my $PT_TESTRESULT_URI_STRING=$PT_NAMESPACE."testResult";

my $PT_PARSER_PREDICATE= RDF::Redland::Node->new_from_uri_string($PT_NAMESPACE."parser");
my $PT_TEST_RESOURCE= RDF::Redland::Node->new_from_uri_string($PT_TEST_URI_STRING);
my $PT_TESTRESULT_PREDICATE= RDF::Redland::Node->new_from_uri_string($PT_TESTRESULT_URI_STRING);
my $PT_STATEMENTS_PREDICATE= RDF::Redland::Node->new_from_uri_string($PT_NAMESPACE."statements");
my $PT_OUTPUT_PREDICATE= RDF::Redland::Node->new_from_uri_string($PT_NAMESPACE."output");
my $PT_ERROR_PREDICATE= RDF::Redland::Node->new_from_uri_string($PT_NAMESPACE."error");
my $PT_EXPECTEDRESULT_PREDICATE= RDF::Redland::Node->new_from_uri_string($PT_NAMESPACE."expectedResult");
my $RDF_TYPE_PREDICATE= RDF::Redland::Node->new_from_uri_string($RDF_NAMESPACE."type");
my $RDFS_LABEL_PREDICATE= RDF::Redland::Node->new_from_uri_string($RDFS_NAMESPACE."label");

my $DC_DESCRIPTION_PREDICATE=RDF::Redland::Node->new_from_uri_string($DC_NAMESPACE."description");
my $DC_TITLE_PREDICATE=RDF::Redland::Node->new_from_uri_string($DC_NAMESPACE."title");


my $GOOD_COLOUR='#00ff00'; # green;
my $WARN_COLOUR='#ffff00'; # yellow;
my $BAD_COLOUR='#ff0000'; # red


my(@commands)=qw(compare-all
		 list-tests
		 show-test-detail
		 list-parsers
		 show-parser-detail
		 );
# removed: compare

my(%command_labels)=('compare'            =>'Compare two parsers (NOT IMPL)',
		     'compare-all'        =>'Compare all parsers',
		     'list-tests'         =>'List all known tests',
		     'show-test-detail'   =>'Show test detail',
		     'list-parsers'       =>'List all known parsers',
		     'show-parser-detail' =>'Show parser detail',
		     'upload'             =>'Upload RDF/XML model');

my $working_parser='raptor';
my $default_command='compare-all';

my $tmp_dir="$::ROOT_DIR/tmp";
my $db_dir="$::ROOT_DIR/db";
my $db='pt';
my $log_file="$::ROOT_DIR/logs/parser-tests.log";


# Redland perl modules

use RDF::Redland;



######################################################################
# Subroutines

sub log_action ($$;$) {
  my($host, $message, $now)=@_;
  $host ||= '-';
  $now ||= time;
  return unless open (LOG, ">>$log_file");
  my($sec,$min,$hour,$mday,$mon,$year)=gmtime $now;
  my $date=sprintf("%04d-%02d-%02dT%02d:%02d:%02dZ",1900+$year,$mon+1,$mday,$hour,$min,$sec);
  print LOG "$host $date $message\n";
  close(LOG);
}

sub end_page($) {
  my $q=shift;

  print <<'EOT';
<p>The source code of this demonstration is available in the Redland
distribution as <tt>demos/parser-tests.pl</tt> or from the
<a href="http://librdf.org/">Redland</a> website</p>
EOT

  print qq{<hr />\n\n<p class="copyright"><a href="http://www.dajobe.org/">Dave Beckett</a></p>\n\n</body></html>\n};
}

sub format_literal ($) {
  my $string=shift;
  return 'UNDEFINED' if !$string;
  encode_entities($string, "&<>\200-\377");
  $string;
}

sub format_url($;$$) {
  my($url,$label,$title)=@_;
  $label ||=$url;
  $title=$title ? qq{ title="$title"} : '';
  qq{<a href="$url"$title>$label</a>};
}


sub upload_rdf($$$$$$) {
  my($host,$q,$model,$storage,$uri_string,$chatty)=@_;

  my $temp_file="$tmp_dir/pt-$$.rdf";

  my $rc=getstore($uri_string, $temp_file);
  if(!is_success($rc)) {
    print "\n\n<p>Failed to read URI $uri_string - HTTP error $rc</p>\n";
    end_page($q);
    exit 0;
  }
  my $source_uri=new URI::URL("file:$temp_file");

  my $parser=new RDF::Redland::Parser($working_parser);
  if(!$parser) {
    log_action($host, "Failed to create RDF/XML parser $working_parser");
    print "\n\n<p>Sorry - failed to create RDF/XML parser $working_parser.  This problem has been logged.</p>\n";
    end_page($q);
    #unlink $temp_file if $temp_file;
    exit 0;
  }

  my $redland_base_uri=new RDF::Redland::URI $uri_string;
  my $redland_source_uri=new RDF::Redland::URI $source_uri;

  my $uri_label='URI '.format_url($uri_string);

  log_action($host, "Parsing URI $uri_string with parser $working_parser");

  my $stream=$parser->parse_as_stream($redland_source_uri, $redland_base_uri);
  if(!$stream || $stream->end) {
    print "\n\n<p>$uri_label failed to parse as RDF/XML into model via stream with $working_parser parser</p>\n";
    end_page($q);
    #unlink $temp_file if $temp_file;
    exit 0;
  }

  my $count=0;
  my $new_tests_count=0;
  while(!$stream->end) {
    my $statement=$stream->current;
    $model->add_statement($statement);
    # Found ?--[pt:test]->[uri]
    if($statement->predicate->equals($PT_TEST_RESOURCE)) {
      my $statement2=RDF::Redland::Statement->new_from_nodes(RDF::Redland::Node->new_from_node($statement->object),
						    RDF::Redland::Node->new_from_node($RDF_TYPE_PREDICATE),
						    RDF::Redland::Node->new_from_node($PT_TEST_RESOURCE));
      if(!$model->contains_statement($statement2)) {
	$new_tests_count++;
	$model->add_statement($statement2);
      }
    }
    $count++;
    $stream->next;
  }
  $stream=undef;
  
  print "\n\n<p>$uri_label parsed as RDF/XML via stream with $working_parser parser OK creating $count statements.</p>\n" if $chatty;

  print "\n\n<p>Found $new_tests_count new tests.</p>\n" 
    if ($chatty && $new_tests_count);

  #unlink $temp_file if $temp_file;

  $count;
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

my $parser1_string;
$val=$q->param('parser1');
if(defined $val && $val =~ /^([-0-9a-z]+)$/) {
  $parser1_string=$1;
} else {
  $parser1_string=undef;
}

my $parser2_string;
$val=$q->param('parser2');
if(defined $val && $val =~ /^([-0-9a-z]+)$/) {
  $parser2_string=$1;
} else {
  $parser2_string=undef;
}

my $command;
$val=$q->param('command');
if(defined $val && $val =~ /^([-a-z]+)$/) {
  $command=$1;
} else {
  $command=undef;
}

my $empty=(!$uri_string && !$parser1_string && !$parser2_string &&
	   !$command);

# End of parameter decoding


# Used in logging
my $host=$q->remote_host;

if($host eq hostname) {
  push(@commands, 'upload', 'init');
}

if($command) {
  if($uri_string) {
    log_action($host, "Command $command URI $uri_string");
  } else {
    log_action($host, "Command $command");
  }
}

######################################################################
# Emit content

print $q->header(-type  =>  'text/html', -charset => 'iso-8859-1')
  unless ($command && $command eq 'init');


my $write='no';
my $new='';

if($command) {
  if($command eq 'init') {
    $write='yes'; $new=qq{new='yes' };
  } elsif($command eq 'upload') {
    $write='yes';
  }
}
my $storage=new RDF::Redland::Storage("hashes", $db,
			     "${new}write='$write',hash-type='bdb',dir='$db_dir'");
my $model;
if($storage) {
  $model=new RDF::Redland::Model($storage, "");
}
if(!$storage || !$model) {
  log_action($host, "Failed to open database $db");
  print "\n\n<p>Sorry - failed to open RDF database.  This problem has been recorded.</p>\n";
  end_page($q);
  exit 0;
}

if($command && $command eq 'init') {
  my(@init_content_uris)=(
    'http://www.ilrt.bristol.ac.uk/discovery/swsw/pt/inputs/expected.rdf',
    # Schemas:
    $PT_NAMESPACE,
    'http://www.ilrt.bristol.ac.uk/discovery/swsw/pt/inputs/dces.rdfs',
    $RDFS_NAMESPACE,
    # Parser Data:
    'http://www.ilrt.bristol.ac.uk/discovery/swsw/pt/inputs/parsers/libwww.rdf',
    'http://www.ilrt.bristol.ac.uk/discovery/swsw/pt/inputs/parsers/raptor.rdf',
    'http://www.ilrt.bristol.ac.uk/discovery/swsw/pt/inputs/parsers/sirpac-stanford.rdf',
    'http://www.ilrt.bristol.ac.uk/discovery/swsw/pt/inputs/parsers/sirpac-w3c.rdf',
    'http://zoe.mathematik.uni-osnabrueck.de/RDF/cara.rdf',
    'http://www.ilrt.bristol.ac.uk/discovery/swsw/pt/inputs/parsers/arp.rdf',
    'http://www.ilrt.bristol.ac.uk/discovery/swsw/pt/inputs/parsers/cwm.rdf',
    'http://www.ilrt.bristol.ac.uk/discovery/swsw/pt/inputs/parsers/rdfstore.rdf',
  );
  my $parser=new RDF::Redland::Parser($working_parser);
  if(!$parser) {
    print "Sorry - failed to create RDF/XML parser $working_parser.\n";
    exit 0;
  }

  my $temp_file="$tmp_dir/pt-$$.rdf";
  my $source_uri=new URI::URL("file:$temp_file");
  my $redland_source_uri=new RDF::Redland::URI $source_uri;

  for my $init_content_uri_string (@init_content_uris) {
    print "$init_content_uri_string: Adding content\n";
    my $count=upload_rdf($host, $q, $model, $storage,
			 $init_content_uri_string, 0);
    print "$init_content_uri_string: added $count statements\n";
  } # end for my $init_content_uri_string
  $model=undef;
  $storage=undef;

  print "Initialisation complete\n";
  exit 0;
} # end for command init


# Find parser nodes i.e.  ?--[rdf:node]-->[pt:parser]
my(@parser_nodes)=$model->sources($RDF_TYPE_PREDICATE, $PT_PARSER_PREDICATE);

my(@parsers);
my(%parser_descs);
{
  for my $node (@parser_nodes) {
    my $label=$model->target($node, $RDFS_LABEL_PREDICATE)->literal_value_as_latin1;
    my $title=$model->target($node, $DC_TITLE_PREDICATE)->literal_value_as_latin1;
    if(!$label || !$title) {
      log_action('', "Parser with node URI ".$node->uri->as_string." has no label and description - ignoring");
      next;
    }
    push(@parsers, $label);
    $parser_descs{$label}=$title;
  }
}


######################################################################

# Always print header
print <<"EOT";
<?xml version="1.0" encoding="iso-8859-1"?>
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" lang="en" xml:lang="en">
<head>
  <title>RDF Parser Tests</title>
  <!-- HTML STYLE -->
</head>
<body>

<!-- LOGO START -->
         <h1>RDF Parser Tests</h1>
<!-- LOGO END -->

<p>This is a web service allowing you to compare RDF/XML Parsers
against each other operating on test content and uses a
<a href="http://www.ilrt.bristol.ac.uk/discovery/2001/03/parser-tests/">simple schema for describing the tests and expected output</a>.
</p>



<p><strong>INSTRUCTIONS</strong>: Choose a command from the menu and
submit the form.  
The default command 'Compare
all parsers' is a good starting place - i.e. just submit the
form.</p>

<p>You can browse the individual
<a href="http://www.ilrt.bristol.ac.uk/discovery/swsw/pt/inputs/parsers/">parser results as RDF/XML files</a>
or the
<a href="http://www.ilrt.bristol.ac.uk/discovery/swsw/pt/inputs/expected.rdf">expected results</a> (<em>VERY tentative</em>).  The tests
are all from the
<a href="$RDF_TESTS_ROOT_URI">W3C RDF samples and miscellaneous tests page.</a></p>


<p>This is an all RDF system written using 
<a href="http://librdf.org/">Redland</a>
and the
<a href="http://librdf.org/docs/perl.html">Perl</a>
interface.  You can even look at the
<a href="http://librdf.org/demo?db=pt;command=print">raw statements</a> (read only)
if you want rather than the slightly cooked statements presented here.</p>

<hr />
EOT

# use q->url() to get URL of this script without any query parameters
# since we are using a POST here and don't want them added to the
# submission URL.
my $action_url=$q->url(-absolute=>1);

print $q->start_form(-method=>'GET', -action => $action_url),"\n";

@commands=qw(upload) if !@parsers;
print "\n\n<p><em>Command</em> \n";
print $q->popup_menu(-name=>'command', 
		     -values=>\@commands,
		     -default=>$default_command, 
		     -labels=>\%command_labels);

print "<p><em>URI (of test or parser)</em><br />\n";
print $q->textfield(-name=>'uri',
		    -default=>'',
		    -size=>80,
		    -maxlength=>1024);


#  if(@parsers) {
#    print "\n\n<p>RDF Parsers when comparing (not implemented yet): ";
#    print $q->popup_menu(-name=>'parser1', 
#  		     -values=>\@parsers,
#  		     -default=>$parsers[0], 
#  		     -labels=>\%parser_descs);

#    print "and";
#    print $q->popup_menu(-name=>'parser2', 
#  		       -values=>\@parsers,
#  		       -default=>$parsers[0], 
#  		       -labels=>\%parser_descs);
#  }

print "</p>\n\n<p>";
print $q->submit('Go');
print "</p>\n\n";

print $q->endform,"\n\n";

print <<"EOT";

<p>Note: the values in the <em>Expected</em> column of tests is a
combination of when the most of the parsers agreed and a visual
inspection.  None of these numbers have been properly validated.</p>

<p>Key: Test match versus expected result:</p>
<table>
<tr><td bgcolor="$BAD_COLOUR">Failed</td></tr>
<tr><td bgcolor="$GOOD_COLOUR">Succeeded</td></tr>
<tr><td bgcolor="$WARN_COLOUR">Succeeded but emitted warning</td></tr>
</table>

EOT

# Any parameters?
if($empty) {
  end_page($q);
  exit 0;
}


print "<hr />\n";

######################################################################

# Validate me

$q->delete('Go');

if($command && !grep($_ eq $command, @commands)) {
  print "\n\n<p>No such command '$command'</p>\n";
  $command=undef;
}

if(!$command) {
  end_page($q);
  exit 0;
}

if($command =~ /compare/) {
  if($parser1_string && !grep($_ eq $parser1_string, @parsers)) {
    print "\n\n<p>No such parser '$parser1_string' - valid list are @parsers</p>\n";
    $parser1_string=$parsers[0];
  }
  
  if($parser2_string && !grep($_ eq $parser2_string, @parsers)) {
    print "\n\n<p>No such parser '$parser2_string' - valid list are @parsers</p>\n";
    $parser2_string=$parsers[0];
  }
}

my $uri='';
if($uri_string) {
  eval "\$uri=new URI::URL(q{$uri_string});";
  if($@ || !$uri) {
    print "\n\n<p>URI ".format_url($uri)." is not a legal URI (according to perl)</p>\n";
    end_page($q);
    exit 0;
  }
  
  if(!$uri->scheme || $uri->scheme ne 'http') {
    print "\n\n<p>Cannot use URI $uri_string - must be a web http URI.</p>\n";
    end_page($q);
    exit 0;
  }
}


$parser1_string ||= $parsers[0];
$parser2_string ||= $parsers[0];



######################################################################

my $temp_file;

if($command eq 'upload') {
  upload_rdf($host, $q, $model, $storage, $uri_string,1);
  end_page($q);
  exit 0;
} # end if command upload


if($command eq 'list-tests') {
  
  $q->param('command', 'show-test-detail');

  # HACK - makes generated page / URIs large otherwise
  $q->delete('parser1');
  $q->delete('parser2');

  print <<"EOT";
<center>
  <table align="center" border="1">
    <tr><th>Source</th> <th>Details</th></tr>

EOT

  my $test_count=0;
  for my $test_node ($model->sources($RDF_TYPE_PREDICATE, $PT_TEST_RESOURCE)) {
    my $test_uri=$test_node->uri->as_string;
    $q->param('uri', $test_uri);
    my $details_uri=$q->self_url;

    my $label=$model->target($test_node, $RDFS_LABEL_PREDICATE);
    $label=$label ? $label->literal_value_as_latin1 : $test_uri;
    print qq{<tr><td>}.format_url($test_uri,$label).qq{</td> <td>}.format_url($details_uri,"Details").qq{</td></tr>\n};
    $test_count++;
  }

  print <<"EOT";
</table>
</center>

<p>Found $test_count tests.</p>

</body>
</html>
EOT

  end_page($q);
  exit 0;
} # end command list-tests


if($command eq 'show-test-detail') {

  print qq{<p>Properties of test }.format_url($uri_string).":<br />\n";

  print <<"EOT";
<center>
  <table align="center" border="1">
    <tr><th>Property</th> <th>Value</th></tr>

EOT

  my $test_node=RDF::Redland::Node->new_from_uri_string($uri_string);
  my $statement=RDF::Redland::Statement->new_from_nodes(RDF::Redland::Node->new_from_node($test_node), undef, undef);

  my $stream=$model->find_statements($statement);
  my $property_count=0;
  while(!$stream->end) {
    my $statement2=$stream->current;

    my $predicate_uri_string=$statement2->predicate->uri->as_string;
    my $value=$statement2->object;

    my $label=$model->target($statement2->predicate, $RDFS_LABEL_PREDICATE);
    $label=$label ? $label->literal_value_as_latin1 : undef;
    $label=format_url($predicate_uri_string, $label);

    print "<tr><td>$label</td> <td>";
    if($value->type == $RDF::Redland::Node::Type_Resource) {
      print format_url($value->uri->as_string);
    } else {
      print format_literal($value->literal_value_as_latin1); # probably literal
    }
    print "</td></tr>\n";
    $property_count++;
    $stream->next;
  }
  $stream=undef;

  print <<"EOT";
</table>
</center>

<p>Found $property_count properties.</p>
EOT

  print <<"EOT";
<p>Results per parser:</p>

<center>
  <table align="center" border="1">
    <tr><th>Parser</th> <th>Expected</th> <th>Statements</th> <th>Output</th> <th>Errors</th></tr>

EOT

  # Now find the results per-parser
  $q->param('command', 'show-parser-detail');

  # HACK - makes generated page / URIs large otherwise
  $q->delete('parser1');
  $q->delete('parser2');

  my $expected_node=$model->target($test_node, $PT_EXPECTEDRESULT_PREDICATE);
  my $expected_string=$expected_node ? $expected_node->literal_value_as_latin1 : '-';

  my $parsers_count=0;
  for my $testresult_node ($model->sources($PT_TEST_RESOURCE, $test_node)) {
    # Go up again to parser
    my $parser_node=$model->source($PT_TESTRESULT_PREDICATE, $testresult_node);

    my $statements_node=$model->target($testresult_node, $PT_STATEMENTS_PREDICATE);
    my $output_node=$model->target($testresult_node, $PT_OUTPUT_PREDICATE);
    my $error_node=$model->target($testresult_node, $PT_ERROR_PREDICATE);

    my $statements_string=$statements_node ? $statements_node->literal_value_as_latin1 : '-';
    my $output_string=$output_node ? $output_node->literal_value_as_latin1 : '-';
    my $error_string=$error_node ? $error_node->literal_value_as_latin1 : '-';


    $q->param('uri', $parser_node->uri->as_string);

    my $parser_title=$model->target($parser_node, $DC_TITLE_PREDICATE)->literal_value_as_latin1;

    my $parser_details=$q->self_url;

    my $colour_attrs='';
    if($error_string ne '-' || $expected_string ne '-') {
      my $cell_colour;
      if($error_string ne '-') {
	$cell_colour=$BAD_COLOUR;
      } elsif ($expected_string =~ /^\d+$/ && 
	 $statements_string eq $expected_string) {
	$cell_colour=($output_string eq '-') ? $GOOD_COLOUR: $WARN_COLOUR;
      } elsif ($expected_string eq 'Error' && $error_string ne '-') {
	$cell_colour=($output_string eq '-') ? $GOOD_COLOUR: $WARN_COLOUR;
      } else {
	$cell_colour=$BAD_COLOUR;
      }
      $colour_attrs=qq{ bgcolor="$cell_colour"};
    }

    print qq{<tr$colour_attrs><td>}.format_url($parser_details,$parser_title).qq{</td> <td>$expected_string</t> <td>$statements_string</td> <td>$output_string</td> <td>$error_string</td></tr>\n};
    $parsers_count++;
  }

  print <<"EOT";
</table>
</center>

<p>Found test results for $parsers_count RDF parsers.</p>
EOT

  

  print <<"EOT";
</body>
</html>
EOT
  end_page($q);
  exit 0;
} # end command show-test-detail


if($command eq 'list-parsers') {
  
  $q->param('command', 'show-parser-detail');

  # HACK - makes generated page / URIs large otherwise
  $q->delete('uri');
  $q->delete('parser2');

  print <<"EOT";
<center>
  <table align="center" border="1">
    <tr><th>Identifier</th> <th>Description</th> <th>Details</th></tr>

EOT

  my $parser_count=0;
  for my $parser_node (@parser_nodes) {
    my $parser_uri=$parser_node->uri->as_string;
    my $desc=$model->target($parser_node, $DC_DESCRIPTION_PREDICATE)->literal_value_as_latin1;

    $q->param('uri', $parser_uri);
    my $details_uri=$q->self_url;

    my $label=$model->target($parser_node, $RDFS_LABEL_PREDICATE);
    $label=$label ? $label->literal_value_as_latin1 : $parser_uri;
    print qq{<tr><td>}.format_url($parser_uri,$label).qq{</td> <td>$desc</td> <td>}.format_url($details_uri,"Details").qq{</td></tr>\n};
    $parser_count++;
  }

  print <<"EOT";
</table>
</center>

<p>Found $parser_count RDF parsers.</p>

</body>
</html>
EOT

  end_page($q);
  exit 0;
} # end command list-parsers


if($command eq 'show-parser-detail') {
  print qq{<p>Properties of RDF parser }.format_url($uri_string).qq{:<br />\n};

  print <<"EOT";
<center>
  <table align="center" border="1">
    <tr><th>Property</th> <th>Value</th></tr>

EOT

  my $parser_node=RDF::Redland::Node->new_from_uri_string($uri_string);
  my $statement=RDF::Redland::Statement->new_from_nodes($parser_node, undef, undef);

  my $stream=$model->find_statements($statement);
  my $property_count=0;
  my $test_results_count=0;
  my(@testresult_nodes);
  for(;!$stream->end; $stream->next) {
    my $statement2=$stream->current;

    if ($statement2->predicate->equals($PT_TESTRESULT_PREDICATE)) {
      $test_results_count++;
      push(@testresult_nodes, RDF::Redland::Node->new_from_node($statement2->object));
      next;
    }
    my $predicate_uri_string=$statement2->predicate->uri->as_string;
    my $value=$statement2->object;

    my $label=$model->target($statement2->predicate, $RDFS_LABEL_PREDICATE);
    $label=$label ? $label->literal_value_as_latin1 : undef;
    $label=format_url($predicate_uri_string, $label);

    print "<tr><td>$label</td> <td>";
    if($value->type == $RDF::Redland::Node::Type_Resource) {
      print format_url($value->uri->as_string);
    } else {
      print format_literal($value->literal_value_as_latin1); # probably literal
    }
    print "</td></tr>\n";
    $property_count++;
  }
  $stream=undef;

  print <<"EOT";
</table>
</center>

<p>Found $property_count properties.</p>

<center>
  <table align="center" border="1">
    <tr><th>Source</th> <th>Details</th> <th>Expected</th> <th>Statements</th> <th>Output</th> <th>Errors</th></tr>
EOT


  $q->param('command', 'show-test-detail');

  # HACK - makes generated page / URIs large otherwise
  $q->delete('parser1');
  $q->delete('parser2');


  my $test_count=0;
  for my $testresult_node (@testresult_nodes) {
    my $test_node=$model->target($testresult_node, $PT_TEST_RESOURCE);
    my $statements_node=$model->target($testresult_node, $PT_STATEMENTS_PREDICATE);
    my $output_node=$model->target($testresult_node, $PT_OUTPUT_PREDICATE);
    my $error_node=$model->target($testresult_node, $PT_ERROR_PREDICATE);
    my $expected_node=$model->target($test_node, $PT_EXPECTEDRESULT_PREDICATE);

    my $statements_string=$statements_node ? $statements_node->literal_value_as_latin1 : '-';
    my $output_string=$output_node ? $output_node->literal_value_as_latin1 : '-';
    my $error_string=$error_node ? $error_node->literal_value_as_latin1 : '-';
    my $expected_string=$expected_node ? ($expected_node->literal_value_as_latin1 || '-') : '-';

    my $test_uri=$test_node->uri->as_string;
    $q->param('uri', $test_uri);
    my $details_uri=$q->self_url;

    my $colour_attrs='';
    if($error_string ne '-' || $expected_string ne '-') {
      my $cell_colour;
      if($error_string ne '-') {
	$cell_colour=$BAD_COLOUR;
      } elsif ($expected_string =~ /^\d+$/ && 
	 $statements_string eq $expected_string) {
	$cell_colour=($output_string eq '-') ? $GOOD_COLOUR: $WARN_COLOUR;
      } elsif ($expected_string eq 'Error' && $error_string ne '-') {
	$cell_colour=($output_string eq '-') ? $GOOD_COLOUR: $WARN_COLOUR;
      } else {
	$cell_colour=$BAD_COLOUR;
      }
      $colour_attrs=qq{ bgcolor="$cell_colour"};
    }

    my $label=$model->target($test_node, $RDFS_LABEL_PREDICATE);
    $label=$label ? $label->literal_value_as_latin1 : $test_uri;
    print qq{<tr$colour_attrs><td>}.format_url($test_uri,$label).qq{</td> <td>}.format_url($details_uri,"Details").qq{</td> <td>$expected_string</t> <td>$statements_string</td> <td>$output_string</td> <td>$error_string</td></tr>\n};
    $test_count++;
  } # end for my testresult_node

  print <<"EOT";
</table>
</center>

<p>Found $test_count test results.</p>
EOT

  

  print <<"EOT";
</body>
</html>
EOT
  end_page($q);
  exit 0;
} # end command show-parser-detail


if($command eq 'compare-all') {

  print <<"EOT";
<center>
  <table align="center" border="1">
EOT

  print qq{    <tr><th>Test</th> <th>Details</th> <th>Expected</th> };

  $q->param('command', 'show-parser-detail');

  # HACK - makes generated page / URIs large otherwise
  $q->delete('parser1');
  $q->delete('parser2');

  my $parsers_count=0;
  for my $parser_node (@parser_nodes) {
    my $parser_uri=$parser_node->uri->as_string;
    my $title=$model->target($parser_node, $DC_TITLE_PREDICATE)->literal_value_as_latin1;
    $q->param('uri', $parser_uri);
    my $parser_details=$q->self_url;
    print qq{<th>}.format_url($parser_details, $title).qq{</th> };
    $parsers_count++;
  }
  print qq{</tr>\n};

  $q->param('command', 'show-test-detail');

  my $test_count=0;
  for my $test_node ($model->sources($RDF_TYPE_PREDICATE, $PT_TEST_RESOURCE)) {
    my $test_uri=$test_node->uri->as_string;

    my $expected_node=$model->target($test_node, $PT_EXPECTEDRESULT_PREDICATE);
    my $expected_string=$expected_node ? ($expected_node->literal_value_as_latin1 || '-') : '-';
    $q->param('uri', $test_uri);
    my $details_uri=$q->self_url;

    my $label=$model->target($test_node, $RDFS_LABEL_PREDICATE);
    $label=$label ? $label->literal_value_as_latin1 : ("<small>".format_url($test_uri,$label)."</small>");
    print qq{<tr><td>}.format_url($test_uri,$label).qq{</td> <td>}.format_url($details_uri,"Details").qq{</td> <td>$expected_string</td>};

    # Now find the results per-parser
    my(@testresult_nodes)=$model->sources($PT_TEST_RESOURCE, $test_node);

    # Sort them by the column order
    my(@testresult_nodes_sorted);
    for my $parser_node (@parser_nodes) {
      my $found=0;
      for my $testresult_node (@testresult_nodes) {
	my $testresult_parser_node=$model->source($PT_TESTRESULT_PREDICATE, $testresult_node);
	if($testresult_parser_node->equals($parser_node)) {
	  push(@testresult_nodes_sorted, $testresult_node);
	  $found=1;
	  last;
	}
      }
      push(@testresult_nodes_sorted, undef) if !$found;
    }

    @testresult_nodes=();
    for my $testresult_node (@testresult_nodes_sorted) {
      if(!$testresult_node) {
	print qq{<td>Missing</td>};
	next;
      }
      # Go up again to parser
      my $parser_node=$model->source($PT_TESTRESULT_PREDICATE, $testresult_node);
      my $statements_node=$model->target($testresult_node, $PT_STATEMENTS_PREDICATE);
      my $output_node=$model->target($testresult_node, $PT_OUTPUT_PREDICATE);
      my $error_node=$model->target($testresult_node, $PT_ERROR_PREDICATE);

      my $summary='';
      if($error_node) {
	$summary='Error';
      } elsif($statements_node) {
	$summary=$statements_node->literal_value_as_latin1;
      }
      if($error_node || $expected_string ne '-') {
	my $cell_colour;
	if($error_node && $expected_string eq 'Error') {
	  $cell_colour=$GOOD_COLOUR;
	} elsif ($summary ne $expected_string) {
	  $cell_colour=$BAD_COLOUR;
	} elsif ($output_node) {
	  $cell_colour=$WARN_COLOUR;
	} else {
	  $cell_colour=$GOOD_COLOUR;
	}
	print qq{<td bgcolor="$cell_colour">$summary</td>};
      } else {
	print qq{<td>$summary</td>};
      }
    } # end for my testresult_node (per parser)

    print qq{</tr>\n};

    $test_count++;
  } # end for my test_node

  print <<"EOT";
</table>
</center>

<p>Found $test_count test results for $parsers_count RDF parsers.</p>
EOT

  

  print <<"EOT";
</body>
</html>
EOT
  end_page($q);
  exit 0;
} # end command compare-all


