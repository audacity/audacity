#! /usr/bin/python3
# -*- coding: utf-8 -*-

"""Test Import / Export and recording.

recording-test.py loads a WAV file, plays it, recording at the same time until
the end of the track, and then exports the recording as a WAV with "-out"
appended to the file name.

To run the test without input prompts, set valid values for
PATH and INFILE.

User supplied variables
-------
    PATH: Path to the folder containing the input test file. Also used for exporting the result.
    INFILE: Name of the input WAV file.

With a little modification, can be suitable for rinse and repeat with different
input files.

Make sure Audacity is running and that mod-script-pipe is enabled
before running this script.
"""

import os
import sys
import time
import json


# Platform specific file name and file path.
# PATH is the location of files to be imported / exported.

#PATH = './'
PATH = ""
while not os.path.isdir(PATH):
    PATH = os.path.realpath(input('Path to test folder: '))
    if not os.path.isdir(PATH):
        print('Invalid path. Try again.')
print('Test folder: ' + PATH)


#INFILE = "testfile.wav"
INFILE = ""
while not os.path.isfile(os.path.join(PATH, INFILE)):
    INFILE = input('Name of input WAV file: ')
    # Ensure we have the .wav extension.
    INFILE = os.path.splitext(INFILE)[0] + '.wav'
    if not os.path.isfile(os.path.join(PATH, INFILE)):
        print(f"{os.path.join(PATH, INFILE)} not found. Try again.")
    else:
        print(f"Input file: {os.path.join(PATH, INFILE)}")
# Remove file extension.
INFILE = os.path.splitext(INFILE)[0]


# Platform specific constants
if sys.platform == 'win32':
    print("recording-test.py, running on windows")
    PIPE_TO_AUDACITY = '\\\\.\\pipe\\ToSrvPipe'
    PIPE_FROM_AUDACITY = '\\\\.\\pipe\\FromSrvPipe'
    EOL = '\r\n\0'
else:
    print("recording-test.py, running on linux or mac")
    PIPE_TO_AUDACITY = '/tmp/audacity_script_pipe.to.' + str(os.getuid())
    PIPE_FROM_AUDACITY = '/tmp/audacity_script_pipe.from.' + str(os.getuid())
    EOL = '\n'


print("Write to  \"" + PIPE_TO_AUDACITY +"\"")
if not os.path.exists(PIPE_TO_AUDACITY):
    print(""" ..does not exist.
    Ensure Audacity is running with mod-script-pipe.""")
    sys.exit()

print("Read from \"" + PIPE_FROM_AUDACITY +"\"")
if not os.path.exists(PIPE_FROM_AUDACITY):
    print(""" ..does not exist.
    Ensure Audacity is running with mod-script-pipe.""")
    sys.exit()

print("-- Both pipes exist.  Good.")

TOPIPE = open(PIPE_TO_AUDACITY, 'w')
print("-- File to write to has been opened")
FROMPIPE = open(PIPE_FROM_AUDACITY, 'r')
print("-- File to read from has now been opened too\r\n")


def send_command(command):
    """Send a command to Audacity."""
    print("Send: >>> "+command)
    TOPIPE.write(command + EOL)
    TOPIPE.flush()


def get_response():
    """Get response from Audacity."""
    line = FROMPIPE.readline()
    result = ""
    while True:
        result += line
        line = FROMPIPE.readline()
        # print(f"Line read: [{line}]")
        if line == '\n':
            return result


def do_command(command):
    """Do the command. Return the response."""
    send_command(command)
    # time.sleep(0.1) # may be required on slow machines
    response = get_response()
    print("Rcvd: <<< " + response)
    return response


def play_record(filename):
    """Import track and record to new track.
    Note that a stop command is not required as playback will stop at end of selection.
    """
    do_command(f"Import2: Filename={os.path.join(PATH, filename + '.wav')}")
    do_command("Select: Track=0")
    do_command("SelTrackStartToEnd")
    # Our imported file has one clip. Find the length of it.
    clipsinfo = do_command("GetInfo: Type=Clips")
    clipsinfo = clipsinfo[:clipsinfo.rfind('BatchCommand finished: OK')]
    clips = json.loads(clipsinfo)
    duration = clips[0]['end'] - clips[0]['start']
    # Now we can start recording.
    do_command("Record2ndChoice")
    print('Sleeping until recording is complete...')
    time.sleep(duration + 0.1)


def export(filename):
    """Export the new track, and deleted both tracks."""
    do_command("Select: Track=1 mode=Set")
    do_command("SelTrackStartToEnd")
    do_command(f"Export2: Filename={os.path.join(PATH, filename)} NumChannels=1.0")
    do_command("SelectAll")
    do_command("RemoveTracks")


def do_one_file(name):
    """Run test with one input file only."""
    play_record(name)
    export(name + "-out.wav")


def quick_test():
    """Quick test to ensure pipe is working."""
    do_command('Help: CommandName=Help')


quick_test()
# Run the test with "testfile.wav" in the specified PATH.
do_one_file(INFILE)
