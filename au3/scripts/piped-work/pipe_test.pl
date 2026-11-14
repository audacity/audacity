#!/usr/bin/env perl

# Test script for communicating with audacity via mod-script-pipe
# Audacity should be running first, with the scripting plugin loaded.
#
# Note that currently, some menu commands require the project to be focused for
# them to work.  Further information and a list of known problems is available
# on the 'Scripting' page of the Audacity wiki.

use strict;
use warnings;
use Time::HiRes qw( gettimeofday tv_interval );
use List::Util qw( max );

# Where should exported tracks be saved?
our $home = $ENV{HOME};
our $effectTestDir = $home.'/pipetest/';

# Variables for pipe names
our $Name;
our $UID;
our $ToSrvName;
our $FromSrvName;

# For timing
our $t0;

# TODO: Maybe get the pipe names from audacity?
if ($^O eq 'MSWin32') {
   $Name = 'Srv';
   $ToSrvName = '\\\\.\\pipe\\To'.$Name.'Pipe';
   $FromSrvName = '\\\\.\\pipe\\From'.$Name.'Pipe';
} elsif ($^O eq 'linux') {
   $UID = $<;
   $ToSrvName = '/tmp/audacity_script_pipe.to.'.$UID;
   $FromSrvName = '/tmp/audacity_script_pipe.from.'.$UID;
} elsif ($^O eq 'darwin') {
   $UID = $<;
   $ToSrvName = '/tmp/audacity_script_pipe.to.'.$UID;
   $FromSrvName = '/tmp/audacity_script_pipe.from.'.$UID;
}

# Open pipes
sub startUp{
   open( TO_SRV, "+<$ToSrvName" )
      or die "Could not open $ToSrvName";
   open( FROM_SRV, "+<$FromSrvName" )
      or die "Could not open $FromSrvName";

   # The next 'magic incantation' causes TO_SRV to be flushed every time we
   # write something to it.
   select((select(TO_SRV),$|=1)[0]);
}

# Close pipes
sub finish{
   print "Done. Press return to end.";
   <>;
   close TO_SRV;
   close FROM_SRV;
}

# Subroutines for measuring how long a command takes to complete
sub startTiming{
   $t0 = [gettimeofday];
}
sub stopTiming{
   my $elapsed = tv_interval ( $t0, [gettimeofday] );
   print "[Total time for command: $elapsed seconds.]\n";
}

# Write a command to the pipe
sub sendCommand{
   my $command = shift;
   if ($^O eq 'MSWin32') {
      print TO_SRV "$command

\r\n\0";
   } else {
      # Don't explicitly send \0 on Linux or reads after the first one fail...
      print TO_SRV "$command\n";
   }
   print "[$command]\n";
}

# Send (and time) a command, and print responses
sub doCommand{
   startTiming();
   sendCommand(shift);

   my @resps = getResponses();
   map { print "$_\n"; } @resps;

   stopTiming();
   print "\n";
   return @resps;
}

# Return an array of all responses
sub getResponses{
   my $resp;
   my @responses;

   while($resp = <FROM_SRV>) {
      chomp($resp);
      last if ($resp eq '');
      push(@responses, $resp);
   }

   return @responses;
}

# Get the value of a preference
sub getPref{
   my $name = shift;
   sendCommand("GetPreference: PrefName=$name");
   my @resps = getResponses();
   return shift(@resps);
}

# Set the value of a preference
sub setPref{
   my $name = shift;
   my $val = shift;
   doCommand("SetPreference: PrefName=$name PrefValue=$val");
}

# Send a menu command
sub menuCommand{
   my $commandName = shift;
   doCommand("MenuCommand: CommandName=$commandName");
}

# Send a command which requests a list of all available menu commands
sub getMenuCommands{
   doCommand("GetMenus: ShowStatus=0");
}

sub showMenuStatus{
   sendCommand("GetMenus: ShowStatus=1");
   my @resps = getResponses();
   map { print "$_\n"; } @resps;
}

# Send a string that should be a syntax error
sub syntaxError{
   doCommand("CommandWithNoColon foo bar");
}

# Send a command that doesn't exist
sub noSuchCommand{
   doCommand("NoSuchCommand: myParam=3");
}

sub parameterTest{
   # Set a non-existent parameter
   doCommand("GetMenus: blah=2");
   # Parameter with no '='
   doCommand("MenuCommand: CommandName");
}

# See what happens when commands have extra spaces in various places
sub extraSpaces{
   doCommand("Help: CommandName=Help");
   doCommand("Help : CommandName=Help");
   doCommand("Help: CommandName =Help");
   doCommand("Help: CommandName= Help");
   doCommand("Help: CommandName=Help ");
}

# Test whether we can fall back to batch mode
sub batchFallback{
   doCommand( "Echo: Delay=1.0 Decay=0.5" );
}

# Send lots of commands quickly
sub stressTest{
   my $n = 0;
   while($n < 600){
      getMenuCommands();
      ++$n;
   }
}

# Get help on a command
sub help{
   my $cmdName = shift;
   doCommand("Help: CommandName=$cmdName");
}

# Get help on all of the listed commands
sub fullHelp{
   my @cmds = qw(BatchCommand CompareAudio MenuCommand GetMenus GetTrackInfo Help Message Select SetTrackInfo);
   foreach my $cmd (@cmds){
      help($cmd);
   }
}

# From script, this works like an 'echo'
sub message{
   my $msg = shift;
   doCommand("Message: MessageString=$msg");
}

# Send a CompareAudio command with a given threshold
sub compareAudio{
   my $threshold = shift;
   my @resps = doCommand("CompareAudio: Threshold=$threshold");
   shift(@resps);
   return @resps;
}

# Delete all tracks
sub deleteAll{
   doCommand("Select: Mode=All");
   menuCommand("RemoveTracks");
}

# A test of the CompareAudio command
sub compareTest{
   deleteAll();

   menuCommand("NewMonoTrack");
   doCommand("Chirp:");
   menuCommand("NewMonoTrack");
   doCommand("Chirp:");

   my $j = 0;
   while($j < 3)
   {
      my $i = 0;
      while($i < 6){
         doCommand("Select: Mode=Range StartTime=5.0 EndTime=8.0 FirstTrack=0 LastTrack=0");

         doCommand("Amplify: Ratio=0.95");
         doCommand("Select: Mode=All");
         compareAudio(0.4 - 0.1*$j);
         ++$i;
      }
      ++$j;
   }
}

# Print some info returned by the GetTrackInfo command
sub getTrackInfo{
   my $trackID = shift;
   sendCommand("GetTrackInfo: Type=Name TrackIndex=0");
   my @resps = getResponses();
   my $name = shift(@resps);
   sendCommand("GetTrackInfo: Type=StartTime TrackIndex=0");
   @resps = getResponses();
   my $startTime = shift(@resps);
   sendCommand("GetTrackInfo: Type=EndTime TrackIndex=0");
   @resps = getResponses();
   my $endTime = shift(@resps);
   sendCommand("GetTrackInfo: Type=Pan TrackIndex=0");
   @resps = getResponses();
   my $pan = shift(@resps);
   sendCommand("GetTrackInfo: Type=Gain TrackIndex=0");
   @resps = getResponses();
   my $gain = shift(@resps);
   sendCommand("GetTrackInfo: Type=Mute TrackIndex=0");
   @resps = getResponses();
   my $mute = shift(@resps);
   sendCommand("GetTrackInfo: Type=Solo TrackIndex=0");
   @resps = getResponses();
   my $solo = shift(@resps);
   sendCommand("GetTrackInfo: Type=Selected TrackIndex=0");
   @resps = getResponses();
   my $selected = shift(@resps);
   sendCommand("GetTrackInfo: Type=Focused TrackIndex=0");
   @resps = getResponses();
   my $focused = shift(@resps);
   sendCommand("GetTrackInfo: Type=Linked TrackIndex=0");
   @resps = getResponses();
   my $linked = shift(@resps);

   print "     Name: $name\n";
   print "StartTime: $startTime\n";
   print "  EndTime: $endTime\n";
   print "      Pan: $pan\n";
   print "     Gain: $gain\n";
   print "     Mute: $mute\n";
   print "     Solo: $solo\n";
   print " Selected: $selected\n";
   print "  Focused: $focused\n";
   print "   Linked: $linked\n";
}

# Assortment of different tests
sub fullTest{
   syntaxError();
   extraSpaces();
   menuCommand("NewStereoTrack");
   doCommand("Select: Mode=All");
   getMenuCommands();
   menuCommand("NewMonoTrack");
   batchFallback();
   message("Hello!");
   getTrackInfo(0);
   deleteAll();
}

# Play for three seconds, then stop
sub playAndStop{
   menuCommand("Play");
   sleep(3.0);
   menuCommand("Stop");
}

# Select part of a stereo track
sub selectRegion{
   my $track = shift;
   my $start = shift;
   my $end = shift;
   my $t1 = $track + 1;
   doCommand("Select: Mode=Range FirstTrack=$track LastTrack=$t1 StartTime=$start EndTime=$end");
}

# Run testing on the effects that use the ClearAndPaste method
# Allows the user to check whether effects transform time correctly
sub testClearAndPasters{

   # Which effects to test, and with what parameters
   my @clearAndPasters = (
      "Unchanged:", # control: nonexistent command, so does nothing
                    # (so 'batch command not recognised' isn't an error)
      "Noise:",    # generate
      "NoiseRemoval:",                 # misc clear&paste
      "ChangeSpeed: Percentage=-10.0", # misc clear&paste
      "ChangeSpeed: Percentage=40.0", # misc clear&paste
      "ChangeTempo: Percentage=-20.0", # soundtouch
      "ChangeTempo: Percentage=80.0", # soundtouch
      "ChangePitch: Percentage=25.0", # soundtouch
      "ChangePitch: Percentage=-80.0", # soundtouch
      "TimeScale: RateStart=-80.0 RateEnd=150.0 HalfStepsStart=-5.0 HalfStepsEnd=8.0 PreAnalyze=no",                      # SBSMS
   ); # nyquist can't be called currently

   # Allow time for user to give the project window focus (workaround for menu
   # command problem)
   sleep(1.0);
   deleteAll();
   my $len = 20.0;

   # Since there aren't proper generator commands yet, we use the preferences
   # to control the duration.
   # This preferences is not read in Audacity 2.2.x where duration
   # is read from pluginsettings.cfg
   my $origDuration = getPref("/CsPresets/NoiseGen_Duration");
   setPref("/CsPresets/NoiseGen_Duration", $len);

   # For each effect to test:
   # * Create some stereo noise, and chop two pieces out of it
   # * Add some labels, then apply the effect
   # @splits determines where the splits are
   my @splits = map {$_ * $len} (0.999, 0.2, 0.5, 0.6, 0.8, 0.1, 0.9);
   my $trackNum = 0;
   foreach my $effect (@clearAndPasters) {
      menuCommand("NewStereoTrack");
      selectRegion($trackNum, 0.0, $splits[0]);
      doCommand("Noise:");
      selectRegion($trackNum, $splits[1], $splits[2]);
      menuCommand("SplitDelete");
      menuCommand("AddLabel");
      selectRegion($trackNum, $splits[3], $splits[4]);
      menuCommand("SplitDelete");
      menuCommand("AddLabel");

      # Apply the effect
      selectRegion($trackNum, $splits[5], $splits[6]);
      doCommand($effect);

      # Make and set the track name
      my @splat = split(':', $effect);
      my $name = $splat[0];
      doCommand("SetTrackInfo: TrackIndex=$trackNum Type=Name Name=$name");
      doCommand("Select: Mode=None");

      $trackNum = $trackNum + 3;
   }

   # Set duration back to what it was before
   setPref("/CsPresets/NoiseGen_Duration", $origDuration);
}


###############################################################################
#  Effect testing                                                             #
###############################################################################

# A list of effects to test (could be got from Audacity in future)
sub getEffects{

   # (These ones will need special handling)
   # AutoDuck
   # Bass and Treble
   # Repair
   # NoiseRemoval
   # ClickRemoval
   # Paulstretch

   # TimeScale (disabled because it's so slow)

   my @effects = qw(
      Amplify
      ChangePitch
      ChangeSpeed
      ChangeTempo
      Compressor
      Echo
      Equalization
      FadeIn
      FadeOut
      Invert
      Normalize
      Phaser
      Repeat
      Reverse
      TruncateSilence
      Wahwah
   );
   return @effects;
}

# Create a chirp for an effect to be applied to.
# The duration setting does not work in Audacity 2.2.x where duration
# is read from pluginsettings.cfg
sub generateBase{
   my $genCmd = "Chirp";
   my $duration = 30.0;
   menuCommand("NewMonoTrack");
   doCommand("$genCmd:");
   my $desc = $genCmd . "-" . $duration . "s";
   return $desc;
}

# Apply an effect and save the results (for use as reference output)
sub saveEffectResults{
   my $dirname = shift;
   my $effect = shift;
   deleteAll();

   my $filename = $dirname . "/" . generateBase() . "-" . $effect . ".wav";
   doCommand($effect);

   printHeading("Exporting to $filename\n");
   doCommand("Export: Mode=All Filename=$filename Channels=1");
}

# Apply an effect and compare the result to reference output
sub doEffectTest{
   my $dirname = shift;
   my $effect = shift;

   deleteAll();
   my $filename = $dirname . "/" . generateBase() . "-" . $effect . ".wav";
   doCommand("SetTrackInfo: TrackIndex=0 Type=Name Name=$effect");
   doCommand($effect);
   doCommand("Import: Filename=$filename");
   doCommand("Select: Mode=All");
   my @result = compareAudio(0.001);
   return @result;
}

# Export reference copies of the effects in the list 
sub exportEffects{
   my $exportDir = shift;
   my @effects = getEffects();
   foreach my $effect (@effects) {
      saveEffectResults($exportDir, $effect);
   }
}

# Test each of the effects in the list
sub testEffects{
   my $referenceDir = shift;
   my %results = ();

   my @effects = getEffects();
   foreach my $effect (@effects) {
      printHeading("Testing effect: $effect");
      my @res = doEffectTest($referenceDir, $effect);
      $results{ $effect }[0] = $res[0];
      $results{ $effect }[1] = $res[1];
   }

   # Print out table of results
   my $padLength = max(map { length($_) } @effects);

   printHeading("Test results");
   print "Effect name\tSamples\tSeconds\n\n";
   for my $effect (keys %results) {
      my $padded = sprintf("%-${padLength}s", $effect);
      my $badSamples = $results{ $effect }[0];
      my $badSeconds = $results{ $effect }[1];
      print "$padded\t$badSamples\t$badSeconds\n";
   }
}

# Print text with ascii lines above and below
sub printHeading{
   my $msg = shift;
   my $line = "-" x length($msg);
   print "$line\n$msg\n$line\n\n";
}

###############################################################################

startUp();

# Send some test commands

exportEffects($effectTestDir);
testEffects($effectTestDir);

finish();
