#!/usr/bin/perl -Tw
#
# rss-view.pl - Redland CGI RSS validator and viewer demo
#
# Copyright (C) 2000-2004, David Beckett http://www.dajobe.org/
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
my $log_file="$::ROOT_DIR/logs/rss.log";

my $max_error_size=100;
my $max_warning_size=100;

my(@parameters)=qw(uri box soup);

# Used for deleting databases
my @suffixes=qw(po2s so2p sp2o);

my(%namespaces)=(
# Built in modules
  'Dublin Core' => 'http://purl.org/dc/elements/1.1/',
  'Syndication' => 'http://purl.org/rss/1.0/modules/syndication/',

# Proposed modules from http://purl.org/rss/1.0/modules/proposed/
  'Changed Page' => 'http://my.theinfo.org/changed/1.0/rss/',
  'RSS 0.91'     => 'http://purl.org/rss/1.0/modules/rss091#',
  'Threading'    => 'http://purl.org/rss/1.0/modules/threading/',
  'Taxonomy'     => 'http://purl.org/rss/1.0/modules/taxonomy/',
  'Events'       => 'http://purl.org/rss/1.0/modules/event/',
  'Content'      => 'http://purl.org/rss/1.0/modules/content/',
  'Creative Commons' => 'http://web.resource.org/cc/',
  'Admin'        => 'http://webns.net/mvcb/',
  'Annotate'     => 'http://purl.org/rss/1.0/modules/annotate/',
# Other modules
  'Dublin Core Terms' => 'http://purl.org/dc/terms/',
  'Slash'        => 'http://slashcode.com/rss/1.0/modules/Slash/', 
);
# Specify order for consistency.  Note: must match keys %namespaces
my(@namespace_order)=(
  'Dublin Core', 'Dublin Core Terms', 'Syndication', 
  'Content', 'Creative Commons',
  'Changed Page', 'Admin',
  'RSS 0.91', 'Threading', 'Taxonomy', 'Events',
  'Slash'
);

my $rss_spec_url='http://purl.org/rss/1.0/spec';

my $content_encoded_uri=$namespaces{Content}."encoded";

my(%demo_rss_feeds)=(
  # Put your favourite RSS 1.0 feeds here
   'W3C Semantic Web News' => 'http://www.w3.org/2001/sw/Overview.rss',
);
my(@demo_rss_feeds_order)=sort {lc $a cmp lc $b} keys %demo_rss_feeds;

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
<a href="http://librdf.org/docs/pod/RDF/Redland/RSS.html">RDF::Redland::RSS</a> 
<a href="http://librdf.org/docs/perl.html">Perl</a> interface,
interpreting the RSS 1.0 content as RDF using the
<a href="http://librdf.org/raptor/">Raptor RDF parser</a>.</p>
<p>The source code of this demonstration is available in the Redland
distribution as <tt>demos/rss-show.pl</tt> or from the
<a href="http://librdf.org/">Redland</a> website</p>

EOT


  print qq{<hr />\n\n<p class="copyright"><a href="http://www.dajobe.org/">Dave Beckett</a></p>\n\n</body></html>};
}

sub format_term ($) {
  my($node)=@_;
  my $type=$node->type;
  if($type == $RDF::Redland::Node::Type_Resource) {
    my $uri=$node->uri->as_string;
    return qq{<a href="$uri">$uri</a>};
  } elsif ($type == $RDF::Redland::Node::Type_Blank) {
    my $id=$node->blank_identifier;
    return qq{BNodeID $id};
  }
  my $string=$node->literal_value;
  return "UNDEFINED" if !defined $string;
  # No need for HTML::Entities here for four things
  $string =~ s/\&/\&amp;/g;
  $string =~ s/</\&lt;/g;
  $string =~ s/>/\&gt;/g;
  $string =~ s/"/\&quot;/g; #"
  $string;
}

sub format_attribute ($) {
  my $string=shift;
  return 'UNDEFINED' if !$string;
  # No need for HTML::Entities here for four things
  $string =~ s/\&/\&amp;/g;
  $string =~ s/</\&lt;/g;
  $string =~ s/>/\&gt;/g;
  $string =~ s/"/\&quot;/g; #"
  $string;
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

my $box;
$val=$q->param('box');
if(defined $val && $val eq 'yes') {
  $box='yes';
} else {
  $box='no';
}

my $soup;
$val=$q->param('soup');
if(defined $val) {
  $soup=1 if $val eq 'yes';
}

my $empty=(!$uri_string);

# Zap remaining parameters
$q->delete_all;
#$q->delete(@parameters, 'Go');

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
  <title>Redland RSS 1.0 Validator and Viewer</title>
  <!-- HTML STYLE -->
</head>
<body>

<!-- LOGO START -->
         <h1>Redland RSS 1.0 Validator and Viewer</h1>
<!-- LOGO END -->

<p>Enter the address of an
<a href="http://groups.yahoo.com/group/rss-dev/">RSS 1.0</a>
feed into the following form and it will be validated
and formatted for display.

EOT


# Restore values
$q->delete_all;
#$q->delete(@parameters);

$q->param('uri', $uri_string) if $uri_string;
$q->param('box', $box);

# use q->url() to get URL of this script without any query parameters
# since we are using a POST here and don't want them added to the
# submission URL.
my $action_url="/".$q->url(-relative=>1);

print $q->start_form(-name => 'myform', -method=>'GET', -action => $action_url),"\n";

print "<p>RSS 1.0 content URI: ";
print $q->textfield(-name=>'uri',
		    -default=>'',
		    -size=>60,
		    -maxlength=>1024);
print '&nbsp;', $q->submit('Go');

print "</p>\n\n<p>Format results in a simple box? ";

my $y=$q->popup_menu(-name=>'box',
                     -values=>['yes','no'], -default=>'no');
# XHTML fixup
$y =~ s/selected /selected="selected" /;
print $y;


print "</p>\n\n";

print $q->endform,"\n\n";

print <<"EOT";
<p><b>Data Privacy:</b> IP addresses and RSS 1.0 content URIs are logged
and may be used for testing.</p>

EOT

if($empty) {
print <<"EOT";

<h2>Public RSS 1.0 feeds</h2>

<p>There are many public RSS 1.0 feeds, here are a sample of some
that can be used with this validator.</p>


EOT

my $height = int(@demo_rss_feeds_order/2+1);

print qq{<table><tr>\n<td>\n};
my $hcount=0;
for my $feed_label (@demo_rss_feeds_order) {

  my $feed_uri=$demo_rss_feeds{$feed_label};
  #$feed_uri =~ s/\&/\&amp;/g;

  $q->delete_all;
  #$q->delete(@parameters);

  $q->param('uri', $feed_uri);
  $q->param('box', 'no');
  my $feed_cgi_uri=$q->self_url;
  #$feed_cgi_uri =~ s/\&/\&amp;/g;

  $q->param('box', 'yes');
  my $box_feed_cgi_uri=$q->self_url;
  #$box_feed_cgi_uri =~ s/\&/\&amp;/g;

  print qq{<a href="}.format_attribute($feed_cgi_uri).qq{">$feed_label</a> (<a href="}.format_attribute($box_feed_cgi_uri).qq{">In a box</a>) <a href="}.format_attribute($feed_uri).qq{" title="RSS 1.0 feed for $feed_label in RDF/XML" type="application/rdf+xml"><img src="/rdf.png" alt="RDF" width="36" height="14" border="0"/></a><br />\n};  

  $hcount++;
  if($hcount >= $height) {
    $hcount=0;
    print qq{</td>\n<td>\n}
  }
}

while($hcount <= $height) {
  $hcount++;
  print qq{&nbsp;<br />\n}
}


print qq{</td>\n</tr></table>\n\n};

print <<"EOT";
<p><small>(Want to use the
<a href="http://www.dajobe.org/2001/04/rdf-icon/">RDF PNG Icon</a>?
Follow the previous link for terms of use.)
</small></p>

EOT

print <<"EOT";

<p>You can find other feeds in various places
such as the
<a href="http://www.oreillynet.com/meerkat/">Meerkat</a>,
<a href="http://www.newsisfree.com/">News Is Free</a> and
<a href="http://www.syndic8.com/">Syndic8</a>
RSS aggregrators.  You can make them yourself with
online tools such as <a href="http://rssxpress.ukoln.ac.uk/">RSS-xpress</a>
(which also has a list of RSS 1.0 feeds)
or <a href="http://www.tnl.net/how/asp/rss">RSSup</a>.
</p>

EOT
}

# Any parameters?
if($empty) {
  end_page($q);
  exit 0;
}


######################################################################

print "<h2>Results of RSS 1.0 Validation</h2>\n";


# Validate me

my $storage=new RDF::Redland::Storage("hashes", 'dummy',
			     "new='yes',write='yes',hash-type='memory',dir='.'");
my $model;
if($storage) {
  $model=new RDF::Redland::Model($storage, "");
}
if(!$storage && !$model) {
  print "\n\n<p>Failed to create RDF Database - sorry!</p>\n";
  end_page($q);
  exit 0;
}



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

my(@errors)=();
RDF::Redland::set_error_handler(sub {
  my $msg=shift;
  push(@errors, $msg);
});

my(@warnings)=();
RDF::Redland::set_warning_handler(sub {
  my $msg=shift;
  push(@warnings, $msg);
});


# Must fetch and copy to temp file
my $temp_file;
if(1) { # parser_just_file_uris
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
    if ($content =~ m%rss version.*(0.9\d+)%mi) {
      print "\n\n<p>The URI you submitted contained RSS $1 content not RSS 1.0.  This demonstration deals with RSS 1.0 only - see above.</p>\n";
      end_page($q);
      exit 0;
    }
  }
  $source_uri=new URI::URL("file:$temp_file");

}

my $parser_name=$soup ? 'rss-tag-soup' : 'raptor';
my $parser_label=$soup ? 'RSS Tag Soup' : 'RDF/XML';
my $parser=new RDF::Redland::Parser($parser_name);
if(!$parser) {
  log_action($host,"Failed to create $parser_name parser", time);
  print "\n\n<p>Failed to create Raptor $parser_label parser.</p>\n";
  end_page($q);
  unlink $temp_file if $temp_file;
  exit 0;
}

my $redland_base_uri=new RDF::Redland::URI $uri;
my $redland_source_uri=new RDF::Redland::URI $source_uri;

log_action($host,"Parsing RSS URI $uri with Raptor", time);
my $stream=$parser->parse_as_stream($redland_source_uri, $redland_base_uri);
if(!$stream || $stream->end) {
  print "\n\n<p>URI \"$uri\" failed to parse RSS 1.0 URI $uri as $parser_label with Raptor.</p>\n";
}

my $count=0;
if($stream && !$stream->end) {
  while(!$stream->end) {
    my $statement=$stream->current;
    $model->add_statement($statement);
    $statement=undef;
    $count++;
    $statement=$stream->next;
  }
}
$stream=undef;

if(@errors) {
  print "<h2>Errors</h2>\n\n<p>";

  my $error_count=1;
  for my $error (@errors) {
    $error =~ s/URI $uri_string:/line /;
    $error =~ s/- Raptor error//;
    print $error,"<br/>\n";
    $error_count++;
    if ($error_count > $max_error_size) {
      print "</p>\n\n<p>Remaining errors $error_count..",scalar(@errors)," truncated to limit page size";
      last;
    }
  }
  print "</p>";
}

if(@warnings) {
  print "<h2>Warnings</h2>\n\n<p>";

  my $warning_count=0;
  for my $warning (@warnings) {
    $warning =~ s/URI $uri_string:/line /;
    $warning =~ s/- Raptor warning//;
    print $warning,"<br/>\n";
    $warning_count++;
    if ($warning_count > $max_warning_size) {
      print "</p>\n\n<p>Remaining warnings $warning_count..",scalar(@warnings)," truncated to limit page size";
      last;
    }
  }
  print "</p>";
}

if(!$count) {
  end_page($q);
  unlink $temp_file if $temp_file;
  exit 0;
}

print "<h2>RSS 1.0 Content</h2>\n";

print "\n\n<p>URI \"$uri\" parsed RSS 1.0 as $parser_label OK (creating $count triples)</p>\n";

#unlink $temp_file if $temp_file;


#$RDF::Debug=1;

my $rss=RDF::Redland::RSS->new_from_model($model);
if(!$rss) {
  print "\n\n<p>Failed to create RDF::Redland::RSS object for URI $uri</p>\n";
  end_page($q);
  unlink $temp_file if $temp_file;
  exit 0;
}


if($box eq 'yes') {
  my $html=$rss->as_xhtml(align=>"right", width=>320, frameColor=>"#000000", titleBarTextColor=>"#000000", titleBarColor=>"#ccccff", boxFillColor=>"#eeeeee", hspace=>15, vspace=>0);
  if($html) {
    print $html,qq{<br clear="all"/></p>};
  } else {
    print "<p>Failed to format as XHTML\n</p>\n\n";
  }
  end_page($q);
  exit 0;
}


sub format_url($) {
  my $url=shift;
  return "UNDEFINED" if !defined $url;
  qq{<a href="$url">$url</a>};
}

my $content_encoded_property=RDF::Redland::Node->new_from_uri_string($content_encoded_uri);

my $missing;
for my $channel ($rss->channels) {
  $missing=qq{<b>Missing.</b>  This is a required element of &lt;rdf:RDF&gt; - see <a href="${rss_spec_url}#s5.3">RSS 1.0 section 5.3</a>};
  print "<p>Found channel with URI ",($channel->uri ? format_url($channel->uri->as_string) : $missing),"<br />\n";
  print "  <b>Title</b>: ",($channel->title ? format_term($channel->title) : $missing),"<br />\n";
  print "  <b>Link</b>: ",($channel->link ? format_url($channel->link->as_string) : $missing),"<br />\n";
  print "  <b>Description</b>: ",format_term($channel->description),"<br />\n" if $channel->description;
  print "</p>\n";

  for my $ns_label (@namespace_order) {
    my $ns_prefix=$namespaces{$ns_label};
    my(@props)=$channel->properties_with_ns_prefix($ns_prefix);
    if(@props) {
      print qq{<p><a href="$ns_prefix">$ns_label</a> properties:<br />\n};
      for my $property (@props) {
	my $value=$channel->property($property);
	my $puri=$property->uri->as_string;
	my $puri_label=$puri; $puri_label =~ s%^$ns_prefix%%;
	next if $puri_label =~ m%/%;
	print qq{<b><a href="$puri">$puri_label</a></b> : };
	print format_term($value); # probably literal
	print "<br />\n";
      }
      print "</p>\n\n";
    }
  }

  my(@items)=$channel->items;
  if(!@items) {
    print qq{<p>Channel items <b>Missing.</b>  &lt;items&gt; is a required element of &lt;channel&gt; - see <a href="${rss_spec_url}#s5.3.5">RSS 1.0 section 5.3.5</a></p>\n\n};
    end_page($q);
    exit 0;
  }


  print "<p>Found ",scalar(@items)," items in channel.</p>\n\n";

  print "<ul>\n\n";
  for my $item (@items) {
    $missing=qq{<b>Missing.</b>  This is a required element of &lt;item&gt; - see <a href="${rss_spec_url}#s5.5">RSS 1.0 section 5.5</a>};
    print "<li><p>Item with URI ",($item->uri ? format_url($item->uri->as_string) : $missing),"<br />\n";
    print "    <b>Title</b>: ",($item->title ? format_term($item->title) : $missing),"<br />\n";
    print "    <b>Link</b>: ",($item->link ? format_url($item->link->as_string) : $missing),"<br />\n";

    my $content_desc=$item->property($content_encoded_property);
    if($content_desc) {
      print "    <b>HTML Description (mod_content)</b>: [[",$content_desc->literal_value,"]]<br />\n";
      if(0) {
	# Now we have used that, remove it from the model
	my $cs=RDF::Redland::Statement->new_from_nodes(RDF::Redland::Node->new_from_node($item),
						       RDF::Redland::Node->new_from_node($content_encoded_property),
						       RDF::Redland::Node->new_from_node($content_desc));
	$model->remove_statement($cs);
      }
    }
    # RSS 1.0 section 5.5 <item> - description</b>:optional
    print "    <b>Description</b>: ",format_term($item->description),"<br />\n" if $item->description;
    print "</p>\n";

    for my $ns_label (@namespace_order) {
      my $ns_prefix=$namespaces{$ns_label};
      my(@props)=$item->properties_with_ns_prefix($ns_prefix);
      if(@props) {
	print qq{<p><a href="$ns_prefix">$ns_label</a> properties:<br />\n};
	for my $property (@props) {
	  my $value=$item->property($property);
	  my $puri=$property->uri->as_string;
	  my $puri_label=$puri; $puri_label =~ s%^$ns_prefix%%;
	  next if $puri_label =~ m%/%;

	  print qq{<b><a href="$puri">$puri_label</a></b> : };
	  print format_term($value);
	  print "<br />\n";
	}
	print "</p>\n\n";
      }
    }

    print "</li>\n";
  }
  print "</ul>\n\n";

  my $image=$channel->image;
  if($image) {
    $missing=qq{<b>Missing.</b>  This is a required element of &lt;image&gt; - see <a href="${rss_spec_url}#s5.4">RSS 1.0 section 5.4</a>};
    print "<p>Image with URI ",($image->uri ? format_url($image->uri->as_string) : $missing),"<br />\n";
    
    # RSS 1.0 section 5.4 <image> - If present, nothing optional
    print "    <b>Title</b>: ",($image->title ? format_term($image->title) : $missing),"<br />\n";
    print "    <b>Link</b>: ",($image->link ? format_url($image->link->as_string) : $missing),"<br />\n";
    print "    <b>URL</b>: ",($image->image_url ? format_url($image->image_url->as_string) : $missing),"<br />\n" if $image->image_url;
    print "</p>\n";
  }

  my $textinput=$channel->textinput;
  if($textinput) {
    $missing=qq{<b>Missing.</b>  This is a required element of &lt;textinput&gt; - see <a href="${rss_spec_url}#s5.6">RSS 1.0 section 5.6</a>};
    my $t_uri=$textinput->uri;
    my $t_title=$textinput->title;
    my $t_link=$textinput->link;
    my $t_desc=$textinput->description;
    my $t_name=$textinput->name;

    print "<p>Textinput with URI ",($t_uri ? format_url($t_uri->as_string) : $missing),"<br />\n";

    # RSS 1.0 section 5.6 <textinput> - If present, nothing optional
    print "    <b>Title</b>: ",($t_title ? format_term($t_title) : $missing),"<br />\n";
    print "    <b>Link</b>: ",($t_link ? format_url($t_link->as_string) : $missing),"<br />\n";
    print "    <b>Description</b>: ",($t_desc ? format_term($t_desc) : $missing),"<br />\n";
    print "    <b>Name</b>: ",($t_name ? format_term($t_name) : $missing),"<br />\n";
    print "</p>\n";

    if($t_uri && $t_title && $t_link && $t_desc && $t_name) {
      my $t_uri_string=$t_uri->as_string;
      my $t_name_string=$t_name->literal_value;
      my $t_desc_string=$t_desc->literal_value;
      my $t_title_string=$t_title->literal_value;

      print <<"EOT";
<p>Formatted search form:</p>

<blockquote>
<form method="GET" action="$t_uri_string">
  <b>$t_title_string</b><br />
  $t_desc_string
  <input type="text" name="$t_name_string" />
  <input type="submit" name="Go" value="Go" />
</form>
</blockquote>
EOT
    } else {
      print <<"EOT";
<p>Not formatting search form since there are missing elements.</p>
EOT
    }
  }

}



end_page($q);
exit 0;
