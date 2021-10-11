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
from functools import partial
from string import Template


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


class PipeTerminal():
    """
    ===============================
    PipeClient Interactive Terminal
    ===============================

    Command General Form:
    {Command Name}: {param1key}="{Value1}" {param2key}="{Value2}" ...

    Terminal built-in commands:
        'H' : view usage help message
        'C' : view 'Commands' list
        'M' : view 'Menus' list
        'Q' : exit program

    (Built-in view commands also provide the option to view individual
    usage requirements)

    A full detailed list of scripting commands and parameters can be found at:

    https://manual.audacityteam.org/man/scripting_reference.html
    """

    def __init__(self, timeout=10, show=True):
        """Initialize PipeClient Terminal"""
        self.timeout = timeout
        self.show = show
        self.client = PipeClient()
        self._builtIn = {"C": partial(self.view_commands, "Commands"),
                         "M": partial(self.view_commands, "Menus"),
                         "H": self.help,
                         "Q": self.quit}

    def start(self):
        """
        ==========================
        Using PipeClient Terminal:
        ==========================
        Prompts user for Audacity Scripting commands

        To send a command, provide the scripting id {command name}.
        Commands sent without parameters use default values set in Audacity.

        To include custom parameters, follow scripting id with a colon ':',
        then spacing each parameter key/value in the following form
        {key="Value"} with quotes around the value, not key.

        Command General Form:
        {Command Name}: {param1key}="{Value1}" {param2key}="{Value2}" ...

        Note: Quotes around values must be double.
        Note: Key values are case-sensitive, commands and keys are not.

        Example
        -------
        > Help

        This example sends the 'Help' scripting command using
        defaults (Command="Help" Format="JSON").


        > Help: Command="Amplify" Format="Brief"

        This example sends the 'Help' scipting command for the
        command 'Amplify', returning it in "Brief" format.
        """
        print(self.__doc__)
        prompt = ("\nEnter Command "
                  "('H': Help, "
                  "'C': Command List, "
                  "'M': Menu List, "
                  "'Q': Quit):\n> ")
        while True:
            # check if python version
            if sys.version_info[0] < 3:
                message = input(prompt)
            else:
                message = input(prompt)
            # check if user input is a built-in command
            if message.upper() in self._builtIn:
                reply = self._builtIn[message.upper()]()
            else:
                reply = self.get_reply(message)
            # if reply returned, print reply
            if reply is not None:
                print(reply)

    def view_commands(self, category):
        """
        =====================
        Usability assistance:
        =====================
        In Addition to Audacity scripting commands, the terminal includes
        built-in commands to view list of possible commands by category.

        The built-in view commands aim to provide easier access to essential
        information needed to use this terminal with greater confidence.

        View options are intended for readability for new users exploring the
        commands available through the mod-script-pipe.

        Example:
        --------
        'Menus': returns information of menu script commands
        (Terminal shortcut-command 'M')

        'Commands': returns information of command script commands
        (Terminal shortcut-command 'C')


        Returning a list of scripting commands by category
        Prompts user to optionally enter a command for additional
        usage information.
        """
        message = 'GetInfo: Type="{type}" Format="JSON"'.format(type=category)
        reply = self.get_reply(message)
        return self.command_helper(reply, message)

    def get_reply(self, message):
        """return reply string from PipeClient"""
        reply = ''
        start = time.time()
        self.client.write(message, timer=self.show)
        while reply == '':
            time.sleep(0.1)  # allow time for reply
            if time.time() - start > self.timeout:
                reply = 'PipeClient: Reply timed-out.'
            else:
                reply = self.client.read()
        return reply

    def command_helper(self, reply, message):
        """
        Formats the pipeclient reply of available scripting commands,
        printing them in readable format.

        Prompts user with the option to view additional usability
        information for provided command.
        """
        # check if message beginning indicates JSON
        if reply[0] != "[" and reply[0] != "{":
            return reply
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
        # prompt user for lookup info for command usage,
        # Empty string or a no match performs no lookup
        command_id = input("Lookup a particular command: ")
        for command in commands:
            if "id" in command:
                if command["id"].upper() == command_id.upper():
                    return self.command_info(command)
        # no command was found in command list
        return "No command lookup performed"

    def command_info(self, command):
        """Returns lookup info for particular command"""
        # Parse together command info depending on type of command
        if "name" in command:
            name = 'Name: ' + command["name"]
        else:
            name = 'Label: ' + command["label"]
        info = command["tip"] if 'tip' in command else ''
        accel = "depth" + command["accel"] if 'accel' in command else ''
        depth = str(command["depth"]) if 'depth' in command else ''
        flags = str(command["flags"]) if 'flags' in command else ''
        id = command["id"] if 'id' in command else ''
        if "params" in command:
            usage = f'{id}:'
            params = "\n\nParameters:\n"
            for param in command["params"]:
                key = param["key"]
                type = param["type"]
                default = str(param["default"])
                enum = str(param["enum"] if param["type"] == 'enum' else '')
                usage += f' {key}="{type}"'
                params += f'{key} - Default({default}){enum}\n'
        else:
            usage = '{id}'
            params = "\n\nParameters: None"
        # set up format of command information
        reply = f'==================================\n' \
                f'Command {name}\n' \
                f'{info}\n' \
                f'Audacity shortkey: {accel}\n' \
                f'{depth} {flags}\n\n' \
                f'To Use:\n> {usage}' \
                f'{params}'
        return reply

    def help(self):
        """
        Prints docstrings from methods to explain usability
        """
        print("PipeClient Terminal 'Help' Command:")
        print(self.__doc__)
        print(self.start.__doc__)
        print(self.view_commands.__doc__)

    def quit(self):
        """Quit pipeclient terminal"""
        sys.exit(0)


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

    terminal = PipeTerminal(args.timeout, args.show)
    terminal.start()


if __name__ == '__main__':
    main()
