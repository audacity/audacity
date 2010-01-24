#!/usr/bin/python

# Copyright (C) 2008 Erik de Castro Lopo <erikd@mega-nerd.com>
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

import commands, os, sys
import time, datetime

def print_test_name (name):
	print "    %-30s :" % name,

def assert_info (filename, arg, value):
	cmd = "./sndfile-metadata-get %s %s" % (arg, filename)
	status, output = commands.getstatusoutput (cmd)
	if status:
		print "\n\nError : command '%s' should not have failed." % cmd
		sys.exit (1)
	if output.find (value) < 0:
		print "\n\nError : not able to find '%s'." % value
		print output
		sys.exit (1)
	return


def check_executable (name):
	if not (os.path.isfile (name)):
		print "\n\nError : Can't find executable '%s'. Have you run make?" % name
		sys.exit (1)

def test_empty_fail ():
	print_test_name ("Empty fail test")
	cmd = "./sndfile-metadata-set --bext-description Alpha sine.wav"
	status, output = commands.getstatusoutput (cmd)
	if not status:
		print "\n\nError : command '%s' should have failed." % cmd
		sys.exit (1)
	print "ok"

def test_copy ():
	print_test_name ("Copy test")
	cmd = "./sndfile-metadata-set --bext-description \"First Try\" sine.wav output.wav"
	status, output = commands.getstatusoutput (cmd)
	if status:
		print "\n\nError : command '%s' should not have failed." % cmd
		sys.exit (1)
	assert_info ("output.wav", "--bext-description", "First Try")
	print "ok"

def test_update (tests):
	print_test_name ("Update test")
	for arg, value in tests:
		cmd = "./sndfile-metadata-set %s \"%s\" output.wav" % (arg, value)
		status, output = commands.getstatusoutput (cmd)
		if status:
			print "\n\nError : command '%s' should not have failed." % cmd
			sys.exit (1)
		assert_info ("output.wav", arg, value)
	print "ok"

def test_post_mod (tests):
	print_test_name ("Post mod test")
	for arg, value in tests:
		assert_info ("output.wav", arg, value)
	print "ok"

def test_auto_date ():
	print_test_name ("Auto date test")
	cmd = "./sndfile-metadata-set --bext-auto-time-date sine.wav date-time.wav"
	status, output = commands.getstatusoutput (cmd)
	if status:
		print "\n\nError : command '%s' should not have failed." % cmd
		sys.exit (1)
	target = datetime.date.today ().__str__ ()
	assert_info ("date-time.wav", "--bext-orig-date", target)
	print "ok"


#-------------------------------------------------------------------------------

def test_coding_history ():
	print_test_name ("Coding history test")
	cmd = "./sndfile-metadata-set --bext-coding-hist \"alpha beta\" output.wav"
	status, output = commands.getstatusoutput (cmd)
	if status:
		print "\n\nError : command '%s' should not have failed." % cmd
		sys.exit (1)
	cmd = "./sndfile-metadata-get --bext-coding-hist output.wav"
	status, output = commands.getstatusoutput (cmd)
	if status:
		print "\n\nError : command '%s' should not have failed." % cmd
		sys.exit (1)
	print "ok"

#-------------------------------------------------------------------------------

def test_rewrite ():
	print_test_name ("Rewrite test")
	cmd = "./sndfile-metadata-set --bext-originator \"Really, really long string\" output.wav"
	status, output = commands.getstatusoutput (cmd)
	if status:
		print "\n\nError : command '%s' should not have failed." % cmd
		sys.exit (1)
	cmd = "./sndfile-metadata-set --bext-originator \"Short\" output.wav"
	status, output = commands.getstatusoutput (cmd)
	if status:
		print "\n\nError : command '%s' should not have failed." % cmd
		sys.exit (1)
	cmd = "./sndfile-metadata-get --bext-originator output.wav"
	status, output = commands.getstatusoutput (cmd)
	if status:
		print "\n\nError : command '%s' should not have failed." % cmd
		sys.exit (1)
	if output.find ("really long") > 0:
		print "\n\nError : output '%s' should not contain 'really long'." % output
		sys.exit (1)
	print "ok"

#===============================================================================

test_dir = "programs"

if os.path.isdir (test_dir):
	os.chdir (test_dir)

for f in [ "sndfile-metadata-set", "sndfile-metadata-get", "../examples/make_sine" ]:
	check_executable (f)

os.system ("../examples/make_sine")
if not os.path.isfile ("sine.wav"):
	print "\n\nError : Can't file file 'sine.wav'."
	sys.exit (1)

print ""

test_empty_fail ()
test_copy ()

tests = [
	("--bext-description", "Alpha"), ("--bext-originator", "Beta"), ("--bext-orig-ref", "Charlie"),
	("--bext-umid", "Delta"), ("--bext-orig-date", "2001-10-01"),  ("--bext-orig-time", "01:02:03"),
	("--str-title", "Echo"), ("--str-artist", "Fox trot")
	]

test_auto_date ()
test_update (tests)
test_post_mod (tests)

test_update ([ ("--str-artist", "Fox") ])

test_coding_history ()

test_rewrite ()


print ""

sys.exit (0)

