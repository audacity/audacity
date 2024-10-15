#!/usr/bin/perl

use strict 'vars';
use File::Spec;

my $traceLevel = 3;

# whether to box the clusters by sub-folder, but always color nodes regardless
my @clusterlist = qw(
   /xml
   /export
   /menus
   /effects/VST
   /effects/ladspa
   /effects/lv2
   /effects/nyquist
   /effects/vamp
);
my %clusters;
@clusters{@clusterlist} = ();
sub clustering
{
   return exists( $clusters{ $_[0] } );
}

# whether to prune redundant arcs implied in transitive closure
my $pruning = 1;

# whether to insert hyperlinks
my $links = 1;

# Step 1: collect short names and paths to .cpp files
# We assume that final path components uniquely identify the files!
my $dir = "../src";

my %names; # string to string
{
   foreach my $file (`find $dir -name '*.cpp' -o -name '*.h' -o -name '*.mm'`) {
      my $short = $file;
      chop $short;
      $short =~ s|\.cpp$||;
      $short =~ s|\.h$||;
      $short =~ s|\.mm$||;
      my $shorter = ($short =~ s|^.*/||r);
      $names{$shorter} = $short;
   }
}

#my $linkroot = "https://github.com/audacity/audacity/tree/master/src";
my $linkroot = "file://" . File::Spec->rel2abs( $dir );


print STDERR "Found ", scalar( keys %names ), " filename(s)\n" if $traceLevel >= 1;

# Step 2: collect inclusions in each .cpp/.h pair, and folder information,
# and build a graph
my $arcs = 0;
my %graph; # hash from names to sets of names
my $grepcmd = "grep '^ *# *include[^\"]*\"[^\"]*\\.h\"'"; # find include directives with quotes
my $sedcmd = "sed -E 's|^[^\"]*\"([^\"]*)\\.h\".*\$|\\1|'"; # extract quoted path
my %folders; # build our own tree like the directories
my $nFolders = 1;
while( my ($shorter, $short) = each(%names) ) {
   # find relevant files (.cpp and .h, and sometimes .mm too)
   my $pat = "${short}.*";
   my @files = glob $pat;

   # store path information, for subgraph clustering later
   $short = substr $short, length( $dir ) + 1;
   my @ownComponents = split '/', $short;
   my $last = pop @ownComponents;
   my $folder = \%folders;
   # this improves the graph in some ways:
   # files that we just put directly under src should be treated as if in
   # a separate subfolder.
   @ownComponents = ("UNCLASSIFIED") if not @ownComponents;
   # store paths in a hash from strings to references to hashes from strings to references to ...
   # (ensuring a nonempty set at key "" for each node of this tree)
   while (@ownComponents) {
      my $component = shift @ownComponents;
      if (not exists $$folder{ $component }) {
         my %empty = ("",());
         $$folder{ $component } = \%empty;
         ++$nFolders;
      }
      $folder = $$folder{ $component };
   }
   # at the last folder, hash empty string specially, to the set of files
   if (not exists $$folder{ "" }) {
      my %empty = ("",());
      $$folder{ "" } = \%empty;
   }
   $$folder{""}{$last} = ();

   my %empty;
   $graph{$shorter} = \%empty; # be sure leaf nodes are not omitted from hash
   foreach (`cat @files | $grepcmd | $sedcmd`) {
      chop;
      my @components = split '/';
      my $include = $components[-1];
      # omit self-arcs and arcs to .h files external to the project
      if (($shorter ne $include) && (exists $names{$include})) {
         $graph{$shorter}{$include} = (), ++$arcs;
      }
   }
}

print STDERR "Found ", scalar( keys %graph ), " node(s) and ${arcs} arc(s)\n" if $traceLevel >= 1;

# Step 3: compute an acyclic quotient graph

my %quotientMap; # from node name to reference to array of node names

sub SCCID {
   # given reference to an array of names
   # use the first name in the array as an ID
   my $scc = shift;
   return $$scc[0];
}

sub SCCLabel {
   # given reference to an array of names
   # use concatenation of names as the displayed label
   my $scc = shift;
   return join "\n", @$scc;
}

my %quotientGraph; # to be populated, from SCC ID to array of:
# [ array of immediately reachable SCC IDs,
#   array of transitively reachable SCC ids,
#   rank number ]
# The first member may be pruned to only those nodes reachable by a longest
# path of length one

# find strongly connected components with Tarjan's algorithm, which discovers
# the nodes of the quotient graph in a bottom-up topologically sorted order
my %temp; # assigns numbers to node names
my $count = 1;
my @stack; # names
my $traceDepth = 0;
$arcs = 0;
my $prunedArcs = 0;
my $maxRank = -1;
my $largest = 0;
# three utility procedures for discovery of one s.c.c.
sub merge {
   my ($a, $b) = @_;
   my $na = @$a;
   my $nb = @$b;
   my @result;
   while ($na && $nb) {
      if ($$a[-$na] lt $$b[-$nb]) {
         push @result, $$a[-($na--)];
      }
      elsif ($$b[-$nb] lt $$a[-$na]) {
         push @result, $$b[-($nb--)];
      }
      else {
         push @result, $$a[-($na--)]; $nb--;
      }
   }
   push @result, $$a[-($na--)] while $na;
   push @result, $$b[-($nb--)] while $nb;
   @result;
}
sub diff {
   my ($a, $b) = @_;
   my $na = @$a;
   my $nb = @$b;
   my @result;
   while ($na && $nb) {
      if ($$a[-$na] lt $$b[-$nb]) {
         push @result, $$a[-($na--)];
      }
      elsif ($$b[-$nb] lt $$a[-$na]) {
         $nb--;
      }
      else {
         $na--; $nb--;
      }
   }
   push @result, $$a[-($na--)] while $na;
   @result;
}
sub discoverOneComponent {
   my ($sorted, $traceIndent) = @_; # reference to sorted array of names
   # first populate the quotient map
   foreach my $node (@$sorted) {
      $quotientMap{ $node } = $sorted;
   }
   # now add arcs to the quotient graph
   my $qhead = $$sorted[0]; # identifier of quotient node, agreeing with sub SCCID
   $#{$quotientGraph{ $qhead }} = 2; # reserve results
   my $data = $quotientGraph{ $qhead }; # reference to results
   my $rank = -1;
   my @reachable;
   my %direct;
   my @merged;
   foreach my $node (@$sorted) {
      my $tails = $graph{ $node };
      foreach my $tail ( keys %$tails ) {
         # it is guaranteed that all destination nodes are already in quotientMap,
         # because of the bottom-up discovery sequence, so this works:
         my $qtail = SCCID( $quotientMap{ $tail } );
         $direct{ $qtail } = () if ( $qhead ne $qtail );
         my $tailData = $quotientGraph{ $qtail };
         my $tailRank = $$tailData[2];
         $rank = $tailRank if $tailRank > $rank;
         @reachable = merge( $$tailData[1], \@reachable );
      }
   }
   ++$rank;
   my @direct = sort ( keys %direct ); # all direct arcs
   my @pruned = diff( \@direct, \@reachable ); # all nonredundant direct arcs
   $prunedArcs += @pruned; # count for trace information
   $arcs += @direct; # count for trace information
   @reachable = merge( \@pruned, \@reachable ); # all nodes reachable (excluding self)
   $$data[0] = $pruning ? \@pruned : \@direct;
   $$data[1] = \@reachable;
   $$data[2] = $rank;
   if ($traceLevel >= 3) {
      print STDERR "${traceIndent}${qhead}";
      print STDERR " and ", (scalar(@$sorted) - 1), " other(s)" if scalar(@$sorted) > 1;
      print STDERR " discovered at rank ${rank}\n";
   }
   $maxRank = $rank if $rank > $maxRank;
   $largest = @$sorted if @$sorted > $largest;
}
#recursive procedure
sub tarjan {
   my ($name, $num) = @_;
   my $traceIndent = " " x $traceDepth;
   if ( exists( $temp{$name} ) ) {
      # have visited
      my $number = $temp{$name};
      if ($number > 0) {
         #scc not fully known
         print STDERR "${traceIndent}${name} ${number} revisited\n" if $traceLevel >= 3;
         return $number;
      }
      else {
         #scc known
         return $num; # unchanged
      }
   }
   else {
      # first visit
      push @stack, $name;
      $temp{$name} = my $number = $count++;
      print STDERR "${traceIndent}${name} ${number} discovering\n" if $traceLevel >= 3;

      # recur on directly reachable nodes
      my $least = $number;
      my $tails = $graph{$name};
      ++$traceDepth;
      foreach my $name2 ( keys %$tails ) {
         my $result = tarjan( $name2, $number );
         $least = $result if $result < $least;
      }
      --$traceDepth;

      if ($least == $number) {
         # finished a component (this was the first discovered node in it)
         my $node;
         my @scc;
         do {
           $node = pop @stack;
           $temp{ $node } = 0;
           push @scc, $node;
         } while( $node ne $name );
         my @sorted = sort @scc;
         discoverOneComponent( \@sorted, $traceIndent );
         return $num; # unchanged
      }
      else {
         # not finished
         print STDERR "${traceIndent}${name} deferred to ${least}\n" if $traceLevel >= 3;
         return $least;
      }
   }
}
# top invocation of recursive procedure discovers all
foreach my $node ( keys %graph ) {
   tarjan( $node, 0 );
}
#give trace information
if ($traceLevel >= 1) {
   print STDERR "Found ", scalar(keys(%quotientGraph)), " strongly connected component(s) in ", (1 + $maxRank), " rank(s)\n";
   print STDERR "Largest component size is ${largest}\n";
   print STDERR "${arcs} arc(s) found (${prunedArcs} after pruning)\n";
}

# Step 4: output the graph in dot language
print STDERR "Generating .dot file\n" if $traceLevel >= 1;

# temporary redirection
*OLD_STDOUT = *STDOUT;
my $fname = "graph.dot";
open my $fh, ">", $fname or die "Can't open file";
*STDOUT = $fh;

# header
my $graphAttr =
   # $clustering ?
   "labeljust=l labelloc=b"
   # : ""
   ;
print "strict digraph{ graph [";
print $graphAttr;
print " newrank=true";
#print " mclimit=0.01";
#print " nslimit=1";
#print " rank=max";
#print " rankdir=LR";
print "]\n";
print "node [style=filled]";

# nodes and their clusters
# group the nodes into subgraphs corresponding to directories
print "\n";
print "// Nodes\n";

my $hue = 0;
my $saturation = 1.0;
my $huestep = 1.0 / $nFolders;
sub subgraph{
   my ($foldername, $hashref) = @_;
   my $clustered = clustering( $foldername );
   my $cluster = $clustered ? "cluster" : "";
   my $clusterAttr = $clustered ? "style=bold color=\"blue\"" : "";
   print STDERR "subgraph \"$foldername\"\n" if $traceLevel >= 3;
   my $color = "${hue},${saturation},1.0";
   $hue += $huestep;
   $saturation = 1.5 - $saturation; # alternate bold and pale
   my $attrs = $clusterAttr . "label=\"$foldername\"";
   print "\nsubgraph \"${cluster}${foldername}\" { $attrs node [fillcolor=\"${color}\"]\n";
   # describe the nodes at this level, stored as a set (i.e. a hash to
   # don't-care values) at key ""
   foreach my $name (sort (keys %{$$hashref{""}})) {
      next unless $name; # ignore dummy element
      my $scc = $quotientMap{ $name };
      my $id = SCCID( $scc );
      # only want the name that is the representative of its s.c.c.
      # equivalence class
      next unless $name eq $id;
      my $label = SCCLabel( $scc );
      print "   \"${id}\" [label=\"$label\"";
      # insert other node attributes here as key=value pairs,
      print " URL=\"${linkroot}${foldername}/${id}.cpp\"" if $links;
      # separated by spaces
      print"]\n";
   }
   # now recur, to describe nested clusters
   foreach my $name ( sort( keys %$hashref ) ) {
     next unless $name; # we just did the special entry at key "" above,
     # which is a set of leaves at this level, not a subtree
     subgraph( "${foldername}/${name}", $$hashref{ $name } );
   }
   print "}\n";
}
subgraph( "", \%folders );

# now describe the arcs
print "\n";
print "// Arcs\n";

while( my ($head, $data) = each( %quotientGraph ) ) {
   foreach my $tail ( @{$$data[0]} ) {
      print "   \"$head\" -> \"$tail\" [";
      # insert arc attributes here as key=value pairs,
      print "penwidth=2.0";
      # separated by spaces
      print"]\n";
   }
}

#footer
print "}\n";

# restore
*STDOUT = *OLD_STDOUT;

# Step 5: generate image
print STDERR "Generating image...\n" if $traceLevel >= 1;
my $verbosity = ($traceLevel >= 2) ? "-v" : "";
`dot $verbosity -O -Tsvg $fname`;
print STDERR "done\n" if $traceLevel >= 1;
