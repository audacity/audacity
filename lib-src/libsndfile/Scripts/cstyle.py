#!/usr/bin/python -tt
#
# Copyright (C) 2005-2017 Erik de Castro Lopo <erikd@mega-nerd.com>
#
# Released under the 2 clause BSD license.

"""
This program checks C code for compliance to coding standards used in
libsndfile and other projects I run.
"""

import re
import sys


class Preprocessor:
	"""
	Preprocess lines of C code to make it easier for the CStyleChecker class to
	test for correctness. Preprocessing works on a single line at a time but
	maintains state between consecutive lines so it can preprocessess multi-line
	comments.
	Preprocessing involves:
	  - Strip C++ style comments from a line.
	  - Strip C comments from a series of lines. When a C comment starts and
	    ends on the same line it will be replaced with 'comment'.
	  - Replace arbitrary C strings with the zero length string.
	  - Replace '#define f(x)' with '#define f (c)' (The C #define requires that
	    there be no space between defined macro name and the open paren of the
	    argument list).
	Used by the CStyleChecker class.
	"""
	def __init__ (self):
		self.comment_nest = 0
		self.leading_space_re = re.compile ('^(\t+| )')
		self.trailing_space_re = re.compile ('(\t+| )$')
		self.define_hack_re = re.compile ("(#\s*define\s+[a-zA-Z0-9_]+)\(")

	def comment_nesting (self):
		"""
		Return the currect comment nesting. At the start and end of the file,
		this value should be zero. Inside C comments it should be 1 or
		(possibly) more.
		"""
		return self.comment_nest

	def __call__ (self, line):
		"""
		Strip the provided line of C and C++ comments. Stripping of multi-line
		C comments works as expected.
		"""

		line = self.define_hack_re.sub (r'\1 (', line)

		line = self.process_strings (line)

		# Strip C++ style comments.
		if self.comment_nest == 0:
			line = re.sub ("( |\t*)//.*", '', line)

		# Strip C style comments.
		open_comment = line.find ('/*')
		close_comment = line.find ('*/')

		if self.comment_nest > 0 and close_comment < 0:
			# Inside a comment block that does not close on this line.
			return ""

		if open_comment >= 0 and close_comment < 0:
			# A comment begins on this line but doesn't close on this line.
			self.comment_nest += 1
			return self.trailing_space_re.sub ('', line [:open_comment])

		if open_comment < 0 and close_comment >= 0:
			# Currently open comment ends on this line.
			self.comment_nest -= 1
			return self.trailing_space_re.sub ('', line [close_comment + 2:])

		if open_comment >= 0 and close_comment > 0 and self.comment_nest == 0:
			# Comment begins and ends on this line. Replace it with 'comment'
			# so we don't need to check whitespace before and after the comment
			# we're removing.
			newline = line [:open_comment] + "comment" + line [close_comment + 2:]
			return self.__call__ (newline)

		return line

	def process_strings (self, line):
		"""
		Given a line of C code, return a string where all literal C strings have
		been replaced with the empty string literal "".
		"""
		for k in range (0, len (line)):
			if line [k] == '"':
				start = k
				for k in range (start + 1, len (line)):
					if line [k] == '"' and line [k - 1] != '\\':
						return line [:start + 1] + '"' + self.process_strings (line [k + 1:])
		return line


class CStyleChecker:
	"""
	A class for checking the whitespace and layout of a C code.
	"""
	def __init__ (self, debug):
		self.debug = debug
		self.filename = None
		self.error_count = 0
		self.line_num = 1
		self.orig_line = ''
		self.trailing_newline_re = re.compile ('[\r\n]+$')
		self.indent_re = re.compile ("^\s*")
		self.last_line_indent = ""
		self.last_line_indent_curly = False
		self.re_checks = \
			[ ( re.compile ("  "),		"multiple space instead of tab" )
			, ( re.compile ("\t "), 	"space after tab" )
			, ( re.compile ("[^ ];"),	"missing space before semi-colon" )
			, ( re.compile ("{[^\s}]"),	"missing space after open brace" )
			, ( re.compile ("[^{\s]}"),	"missing space before close brace" )
			, ( re.compile ("[ \t]+$"),	"contains trailing whitespace" )

			, ( re.compile (",[^\s\n]"),		"missing space after comma" )
			, ( re.compile (";[^\s]"), 	"missing space after semi-colon" )
			, ( re.compile ("=[^\s\"'=]"),		"missing space after assignment" )

			# Open and close parenthesis.
			, ( re.compile ("[^\s\(\[\*&']\("),				"missing space before open parenthesis" )
			, ( re.compile ("\)(-[^>]|[^,'\s\n\)\]-])"),	"missing space after close parenthesis" )
			, ( re.compile ("\s(do|for|if|when)\s.*{$"),	"trailing open parenthesis at end of line" )
			, ( re.compile ("\( [^;]"),						"space after open parenthesis" )
			, ( re.compile ("[^;] \)"),						"space before close parenthesis" )

			# Open and close square brace.
			, ( re.compile ("[^\s\(\]]\["),			"missing space before open square brace" )
			, ( re.compile ("\][^,\)\]\[\s\.-]"),	"missing space after close square brace" )
			, ( re.compile ("\[ "),					"space after open square brace" )
			, ( re.compile (" \]"),					"space before close square brace" )

			# Space around operators.
			, ( re.compile ("[^\s][\*/%+-][=][^\s]"),		"missing space around opassign" )
			, ( re.compile ("[^\s][<>!=^/][=]{1,2}[^\s]"),	"missing space around comparison" )

			# Parens around single argument to return.
			, ( re.compile ("\s+return\s+\([a-zA-Z0-9_]+\)\s+;"),	"parens around return value" )

			# Parens around single case argument.
			, ( re.compile ("\s+case\s+\([a-zA-Z0-9_]+\)\s+:"),	"parens around single case argument" )

			# Open curly at end of line.
			, ( re.compile ("\)\s*{\s*$"),	"open curly brace at end of line" )

			# Pre and post increment/decrment.
			, ( re.compile ("[^\(\[][+-]{2}[a-zA-Z0-9_]"),       "space after pre increment/decrement" )
			, ( re.compile ("[a-zA-Z0-9_][+-]{2}[^\)\,]]"),       "space before post increment/decrement" )
			]

	def get_error_count (self):
		"""
		Return the current error count for this CStyleChecker object.
		"""
		return self.error_count

	def check_files (self, files):
		"""
		Run the style checker on all the specified files.
		"""
		for filename in files:
			self.check_file (filename)

	def check_file (self, filename):
		"""
		Run the style checker on the specified file.
		"""
		self.filename = filename
		cfile = open (filename, "r")

		self.line_num = 1

		preprocess = Preprocessor ()
		while 1:
			line = cfile.readline ()
			if not line:
				break

			line = self.trailing_newline_re.sub ('', line)
			self.orig_line = line

			self.line_checks (preprocess (line))

			self.line_num += 1

		cfile.close ()
		self.filename = None

		# Check for errors finding comments.
		if preprocess.comment_nesting () != 0:
			print ("Weird, comments nested incorrectly.")
			sys.exit (1)

		return

	def line_checks (self, line):
		"""
		Run the style checker on provided line of text, but within the context
		of how the line fits within the file.
		"""

		indent = len (self.indent_re.search (line).group ())
		if re.search ("^\s+}", line):
			if not self.last_line_indent_curly and indent != self.last_line_indent:
				None	# self.error ("bad indent on close curly brace")
			self.last_line_indent_curly = True
		else:
			self.last_line_indent_curly = False

		# Now all the regex checks.
		for (check_re, msg) in self.re_checks:
			if check_re.search (line):
				self.error (msg)

		if re.search ("[a-zA-Z0-9][<>!=^/&\|]{1,2}[a-zA-Z0-9]", line):
			if not re.search (".*#include.*[a-zA-Z0-9]/[a-zA-Z]", line):
				self.error ("missing space around operator")

		self.last_line_indent = indent
		return

	def error (self, msg):
		"""
		Print an error message and increment the error count.
		"""
		print ("%s (%d) : %s" % (self.filename, self.line_num, msg))
		if self.debug:
			print ("'" + self.orig_line + "'")
		self.error_count += 1

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

if len (sys.argv) < 1:
	print ("Usage : yada yada")
	sys.exit (1)

# Create a new CStyleChecker object
if sys.argv [1] == '-d' or sys.argv [1] == '--debug':
	cstyle = CStyleChecker (True)
	cstyle.check_files (sys.argv [2:])
else:
	cstyle = CStyleChecker (False)
	cstyle.check_files (sys.argv [1:])


if cstyle.get_error_count ():
	sys.exit (1)

sys.exit (0)
