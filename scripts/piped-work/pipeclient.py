#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""Automate Audacity via mod-script-pipe.

Pipe Client may be used as a command-line script to send commands to
Audacity via the mod-script-pipe interface, or loaded as a module.
Requires Python 2.7 or later. Python 3 strongly recommended.

======================
Command Line Interface
======================

    usage: pipeclient.py [-h] [-t] [-s ] [-d]

Arguments
---------
    -h,--help: optional
        show short help and exit
    -t, --timeout: float, optional
        timeout for reply in seconds (default: 10)
    -s, --show-time: bool, optional
        show command execution time (default: True)
    -d, --docs: optional
        show this documentation and exit

Example
-------
    $ python3 pipeclient.py -t 20 -s False

    Launches command line interface with 20 second time-out for
    returned message, and don't show the execution time.

    When prompted, enter the command to send (not quoted), or 'Q' to quit.

    $ Enter command or 'Q' to quit: GetInfo: Type=Tracks Format=LISP

============
Module Usage
============

Note that on a critical error (such as broken pipe), the module just exits.
If a more graceful shutdown is required, replace the sys.exit()'s with
exceptions.

Example
-------

    # Import the module:
    >>> import pipeclient

    # Create a client instance:
    >>> client = pipeclient.PipeClient()

    # Send a command:
    >>> client.write("Command", timer=True)

    # Read the last reply:
    >>> print(client.read())

See Also
--------
PipeClient.write : Write a command to _write_pipe.
PipeClient.read : Read Audacity's reply from pipe.

Copyright Steve Daulton 2018
Released under terms of the GNU General Public License version 2:
<http://www.gnu.org/licenses/old-licenses/gpl-2.0.html />

"""

import os
import sys
import threading
import time
import errno
import argparse
import json


if sys.version_info[0] < 3 and sys.version_info[1] < 7:
    sys.exit('PipeClient Error: Python 2.7 or later required')

# Platform specific constants
if sys.platform == 'win32':
    WRITE_NAME = '\\\\.\\pipe\\ToSrvPipe'
    READ_NAME = '\\\\.\\pipe\\FromSrvPipe'
    EOL = '\r\n\0'
else:
    # Linux or Mac
    PIPE_BASE = '/tmp/audacity_script_pipe.'
    WRITE_NAME = PIPE_BASE + 'to.' + str(os.getuid())
    READ_NAME = PIPE_BASE + 'from.' + str(os.getuid())
    EOL = '\n'


class PipeClient():
    """Write / read client access to Audacity via named pipes.

    Normally there should be just one instance of this class. If
    more instances are created, they all share the same state.

    __init__ calls _write_thread_start() and _read_thread_start() on
    first instantiation.

    Parameters
    ----------
        None

    Attributes
    ----------
        reader_pipe_broken : event object
            Set if pipe reader fails. Audacity may have crashed
        reply_ready : event object
            flag cleared when command sent and set when response received
        timer : bool
            When true, time the command execution (default False)
        reply : string
            message received when Audacity completes the command

    See Also
    --------
    write : Write a command to _write_pipe.
    read : Read Audacity's reply from pipe.

    """

    reader_pipe_broken = threading.Event()
    reply_ready = threading.Event()

    _shared_state = {}

    def __new__(cls, *p, **k):
        self = object.__new__(cls, *p, **k)
        self.__dict__ = cls._shared_state
        return self

    def __init__(self):
        self.timer = False
        self._start_time = 0
        self._write_pipe = None
        self.reply = ''
        if not self._write_pipe:
            self._write_thread_start()
        self._read_thread_start()

    def _write_thread_start(self):
        """Start _write_pipe thread"""
        # Pipe is opened in a new thread so that we don't
        # freeze if Audacity is not running.
        write_thread = threading.Thread(target=self._write_pipe_open)
        write_thread.daemon = True
        write_thread.start()
        # Allow a little time for connection to be made.
        time.sleep(0.1)
        if not self._write_pipe:
            sys.exit('PipeClientError: Write pipe cannot be opened.')

    def _write_pipe_open(self):
        """Open _write_pipe."""
        self._write_pipe = open(WRITE_NAME, 'w')

    def _read_thread_start(self):
        """Start read_pipe thread."""
        read_thread = threading.Thread(target=self._reader)
        read_thread.daemon = True
        read_thread.start()

    def write(self, command, timer=False):
        """Write a command to _write_pipe.

        Parameters
        ----------
            command : string
                The command to send to Audacity
            timer : bool, optional
                If true, time the execution of the command

        Example
        -------
            write("GetInfo: Type=Labels", timer=True):

        """
        self.timer = timer
        print('Sending command:', command)
        self._write_pipe.write(command + EOL)
        # Check that read pipe is alive
        if PipeClient.reader_pipe_broken.isSet():
            sys.exit('PipeClient: Read-pipe error.')
        try:
            self._write_pipe.flush()
            if self.timer:
                self._start_time = time.time()
            self.reply = ''
            PipeClient.reply_ready.clear()
        except IOError as err:
            if err.errno == errno.EPIPE:
                sys.exit('PipeClient: Write-pipe error.')
            else:
                raise

    def _reader(self):
        """Read FIFO in worker thread."""
        # Thread will wait at this read until it connects.
        # Connection should occur as soon as _write_pipe has connected.
        read_pipe = open(READ_NAME, 'r')
        message = ''
        pipe_ok = True
        while pipe_ok:
            line = read_pipe.readline()
            # Stop timer as soon as we get first line of response.
            stop_time = time.time()
            while pipe_ok and line != '\n':
                message += line
                line = read_pipe.readline()
                if line == '':
                    # No data in read_pipe indicates that the pipe is broken
                    # (Audacity may have crashed).
                    PipeClient.reader_pipe_broken.set()
                    pipe_ok = False
            if self.timer:
                xtime = (stop_time - self._start_time) * 1000
                message += 'Execution time: {0:.2f}ms'.format(xtime)
            self.reply = message
            PipeClient.reply_ready.set()
            message = ''
        read_pipe.close()

    def read(self):
        """Read Audacity's reply from pipe.

        Returns
        -------
        string
            The reply from the last command sent to Audacity, or null string
            if reply not received. Null string usually indicates that Audacity
            is still processing the last command.

        """
        if not PipeClient.reply_ready.isSet():
            return ''
        return self.reply


def bool_from_string(strval):
    """Return boolean value from string"""
    if strval.lower() in ('true', 't', '1', 'yes', 'y'):
        return True
    if strval.lower() in ('false', 'f', '0', 'no', 'n'):
        return False
    raise argparse.ArgumentTypeError('Boolean value expected.')


def main():
    """Interactive command-line for PipeClient"""

    parser = argparse.ArgumentParser()
    parser.add_argument('-t', '--timeout', type=float, metavar='', default=10,
                        help="timeout for reply in seconds (default: 10")
    parser.add_argument('-s', '--show-time', metavar='True/False',
                        nargs='?', type=bool_from_string,
                        const='t', default='t', dest='show',
                        help='show command execution time (default: True)')
    parser.add_argument('-d', '--docs', action='store_true',
                        help='show documentation and exit')
    args = parser.parse_args()

    if args.docs:
        print(__doc__)
        sys.exit(0)

    client = PipeClient()
    view_commands_options = {"C": "Commands", "M": "Menus"}
    while True:
        reply = ''
        if sys.version_info[0] < 3:
            message = input("\nEnter command, 'H' for usage help, or 'Q' to quit:\n> ")
        else:
            message = input(
                "\nEnter command, 'H' for usage help, or 'Q' to quit:\n> ")
        start = time.time()
        if message.upper() == 'Q':
            sys.exit(0)
        elif message.upper() == 'H':
            usage_helper(reply)
        elif message == '':
            pass
        else:
            command_list = False
            if message.upper() in view_commands_options:
                category = view_commands_options[message.upper()]
                command_list = True
                message = 'GetInfo: Type="{type}" Format="JSON"'.format(type=category)
            client.write(message, timer=args.show)
            while reply == '':
                time.sleep(0.1)  # allow time for reply
                if time.time() - start > args.timeout:
                    reply = 'PipeClient: Reply timed-out.'
                else:
                    reply = client.read()
            if command_list:
                command_helper(reply, message)
            else:
                print(reply)


def usage_helper(reply):
    """Returns instructions for command-line usage"""
    reply ="""
    ===============================
    Interactive command-line
    ===============================
    Usability assistance:
    The following contains shortcut commands for easier access to essential
    information needed to use this terminal with greater confidence.  
    
    View options are intended for readability for new users exploring the
    commands available through the mod-script-pipe.

    Terminal shortcut commands:
    'H' : view this usage help message
    'C' : view 'Commands' list (option to view individual usage requirements)
    'M' : view 'Menus' list (option to view individual usage requirements)
    'Q' : exit program
    
    A full detailed list of scripting commands and parameters can be found at:

    https://manual.audacityteam.org/man/scripting_reference.html

    
    ==================
    Command-line Usage
    ==================
    To send a command, provide the scripting id {command name}.
    Commands sent without parameters use default values set in Audacity.

    To include custom parameters, the scripting id must be followed by a colon ':',
    then spacing each parameter key/value in the following form {key="Value"}.

    Note: Quotes around values must be double.
    Note: Key values are case-sensitive, commands and keys are not.

    Command General Form:
    {Command Name}: {param1key}="{Value1}" {param2key}="{Value2}" ... 

    Example
    -------
        > Help

        This example sends the 'Help' scripting command using
        defaults (Command="Help" Format="JSON"). 
        

        > Help: Command="Amplify" Format="Brief"
        
        This example sends the 'Help' scipting command for the 
        command 'Amplify', returning it in "Brief" format.
    """
    print(reply)


def command_helper(reply, message):
    """Returns available commands in readable format, prompts for additional command info"""
    # check if message beginning indicates JSON
    firstchar = reply[0]
    if reply[0] != "[" and reply[0] != "{":
        print(reply)
        return
    # clean string for JSON conversion 
    reply = reply.replace('\n', '')
    reply = reply.replace('\\', '/')
    reply = reply.replace('/"', "'")
    runtime = ''
    # save run time if requested
    if reply.find('BatchCommand finished:') != -1:
        index = reply.find('BatchCommand finished:')
        runtime = reply[index:]
        reply = reply.replace(runtime, '')
    if reply.find('Execution time:') != -1:
        print(reply)
        return
    # convert to json for processing information
    commands = json.loads(reply)
    reply = runtime + '\n\n'
    # arrange commands to be read in column form
    command_ids = []
    for command in commands:
        if "id" in command:
            command_ids.append(command["id"])
    command_ids.sort()
    row = [] 
    for command in command_ids:  
            row.append(command)     
            if len(row) == 2:
                reply += str('{:<40s}{:<40s}\n'.format(row[0], row[1])) 
                row = []
    print(reply)
    # prompt user for lookup info for command usage, a not match performs no lookup 
    command_id = input("Lookup a particular command: ")
    command_found = False
    for command in commands:
        if "id" in command:
            if command["id"].upper() == command_id.upper():
                print(command_info(command))
                command_found = True
    if not command_found:
        print("No command lookup performed")


def command_info(command):
    """Prints lookup info for particular command"""
    reply = ''
    reply += '==================================\n'
    reply += 'Command {title} \n'.format(title='Name: ' + command["name"] if "name" in command else 'Label: ' + command["label"])
    reply += '{info}'.format(info=command["tip"]+'\n' if "tip" in command else '')
    reply += '{accel}'.format(accel='Audacity shortkey: ' + command["accel"] + '\n' if "accel" in command else '')
    reply += '{depth}'.format(depth='depth: ' + str(command["depth"]) + ' ' if "depth" in command else '')
    reply += '{flags}'.format(flags='flags: ' + str(command["flags"]) + ' ' if "flags" in command else '')
    
    reply += '{id}'.format(id='\n\nTo Use:\n> ' + command["id"] if "id" in command else '')
    if "params" not in command:
        params = "\n\nParameters: None"
    else:
        params = ':'
        for param in command["params"]:
            params += ' {key}="{type}"'.format(key=param["key"],type=param["type"])    
        params += "\n\nParameters:\n"
        for param in command["params"]:
            params += '\t{key} - Default({default})'.format(key=param["key"], default=str(param["default"]))
            params += '{enum}\n'.format(enum=str(param["enum"] if param["type"] == 'enum' else ''))
    reply += params
    return reply


if __name__ == '__main__':
    main()
