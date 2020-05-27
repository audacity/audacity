#!/usr/bin/env python

# Copyright (C) 2008-2016 Erik de Castro Lopo <erikd@mega-nerd.com>
#
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are
# met:
#
#     * Redistributions of source code must retain the above copyright
#       notice, this list of conditions and the following disclaimer.
#     * Redistributions in binary form must reproduce the above copyright
#       notice, this list of conditions and the following disclaimer in
#       the documentation and/or other materials provided with the
#       distribution.
#     * Neither the author nor the names of any contributors may be used
#       to endorse or promote products derived from this software without
#       specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
# "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
# TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
# PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
# CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
# EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
# PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS;
# OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
# WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
# OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
# ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

# Simple test script for the sndfile-metadata-set program.

from __future__ import print_function

try:
    # py2
    import commands
except ImportError:
    # py3
    import subprocess as commands

import os, sys
import time, datetime

class Programs:
    def __init__ (self, needs_exe):
        if needs_exe:
            extension = ".exe"
        else:
            extension = ""
        self.meta_set_prog = "./sndfile-metadata-set" + extension
        self.meta_get_prog = "./sndfile-metadata-get" + extension
        self.make_sine_prog = "../examples/make_sine" + extension

    def _run_command (self, should_fail, cmd):
        status, output = commands.getstatusoutput (cmd)
        if should_fail and not status:
            print("\n\nError : command '%s' should have failed." % cmd)
            print(output)
            print()
            sys.exit (1)
        if not should_fail and status:
            print("\n\nError : command '%s' should not have failed." % cmd)
            print(output)
            print()
            sys.exit (1)
        return output

    def meta_set (self, should_fail, args):
        return self._run_command (should_fail, self.meta_set_prog + " " + args)

    def meta_get (self, should_fail, args):
        return self._run_command (should_fail, self.meta_get_prog + " " + args)

    def make_sine (self):
        return os.system (self.make_sine_prog)

    def check_executables (self):
        for name in [ self.meta_set_prog, self.meta_get_prog, self.make_sine_prog ]:
            if not (os.path.isfile (name)):
                print("\n\nError : Can't find executable '%s'. Have you run make?" % name)
                sys.exit (1)


def print_test_name (name):
    print("    %-30s :" % name, end="")

def assert_info (programs, filename, arg, value):
    output = programs.meta_get (False, "%s %s" % (arg, filename))
    if output.find (value) < 0:
        print("\n\nError : not able to find '%s'." % value)
        print(output)
        sys.exit (1)
    return


def test_empty_fail (programs):
    print_test_name ("Empty fail test")
    output = programs.meta_set (True, "--bext-description Alpha sine.wav")
    print("ok")

def test_copy (programs):
    print_test_name ("Copy test")
    output = programs.meta_set (False, "--bext-description \"First Try\" sine.wav output.wav")
    assert_info (programs, "output.wav", "--bext-description", "First Try")
    print("ok")

def test_update (programs, tests):
    print_test_name ("Update test")
    for arg, value in tests:
        output = programs.meta_set (False, "%s \"%s\" output.wav" % (arg, value))
        assert_info (programs, "output.wav", arg, value)
    print("ok")

def test_post_mod (programs, tests):
    print_test_name ("Post mod test")
    for arg, value in tests:
        assert_info (programs, "output.wav", arg, value)
    print("ok")

def test_auto_date (programs):
    print_test_name ("Auto date test")
    output = programs.meta_set (False, "--bext-auto-time-date sine.wav date-time.wav")
    target = datetime.date.today ().__str__ ()
    assert_info (programs, "date-time.wav", "--bext-orig-date", target)
    print("ok")


#-------------------------------------------------------------------------------

def test_coding_history (programs):
    print_test_name ("Coding history test")
    output = programs.meta_set (False, "--bext-coding-hist \"alpha beta\" output.wav")
    output = programs.meta_get (False, "--bext-coding-hist output.wav")
    print("ok")

#-------------------------------------------------------------------------------

def test_rewrite (programs):
    print_test_name ("Rewrite test")
    output = programs.meta_set (False, "--bext-originator \"Really, really long string\" output.wav")
    output = programs.meta_set (False, "--bext-originator \"Short\" output.wav")
    output = programs.meta_get (False, "--bext-originator output.wav")
    if output.find ("really long") > 0:
        print("\n\nError : output '%s' should not contain 'really long'." % output)
        sys.exit (1)
    print("ok")

#===============================================================================

test_dir = "programs"

print("\nTesting WAV metadata manipulation:")

if os.path.isdir (test_dir):
    os.chdir (test_dir)

if len (sys.argv) >= 1 and sys.argv [1].endswith ("mingw32"):
    needs_exe = True
else:
    needs_exe = False

programs = Programs (needs_exe)

programs.check_executables ()

programs.make_sine ()
if not os.path.isfile ("sine.wav"):
    print("\n\nError : Can't file file 'sine.wav'.")
    sys.exit (1)

test_empty_fail (programs)
test_copy (programs)

tests = [
    ("--bext-description", "Alpha"), ("--bext-originator", "Beta"), ("--bext-orig-ref", "Charlie"),
    ("--bext-umid", "Delta"), ("--bext-orig-date", "2001-10-01"),  ("--bext-orig-time", "01:02:03"),
    ("--str-title", "Echo"), ("--str-artist", "Fox trot")
    ]

test_auto_date (programs)
test_update (programs, tests)
test_post_mod (programs, tests)

test_update (programs, [ ("--str-artist", "Fox") ])

# This never worked.
# test_coding_history ()

test_rewrite (programs)


print()

sys.exit (0)

