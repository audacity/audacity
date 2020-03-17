#!/usr/bin/python

# Copyright (C) 2006-2017 Erik de Castro Lopo <erikd@mega-nerd.com>
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


# This parses C code using regexes (yes, thats horrible) and makes sure
# that calling conventions to the function psf_binheader_writef are
# correct.



import re, string, sys

_whitespace_re = re.compile ("\s+", re.MULTILINE)

def find_binheader_writefs (data):
    lst = re.findall ('psf_binheader_writef\s*\(\s*[a-zA-Z_]+\s*,\s*\"[^;]+;', data, re.MULTILINE)
    return [_whitespace_re.sub (" ", x) for x in lst]

def find_format_string (s):
    fmt = re.search ('"([^"]+)"', s)
    if not fmt:
        print ("Bad format in :\n\n\t%s\n\n" % s)
        sys.exit (1)
    fmt = fmt.groups ()
    if len (fmt) != 1:
        print ("Bad format in :\n\n\t%s\n\n" % s)
        sys.exit (1)
    return _whitespace_re.sub ("", fmt [0])

def get_param_list (data):
    dlist = re.search ("\((.+)\)\s*;", data)
    dlist = dlist.groups ()[0]
    dlist = dlist.split(",")
    dlist = [x.strip() for x in dlist]
    return dlist [2:]

def handle_file (fname):
    errors = 0
    data = open (fname, "r").read ()

    # return errors

    writefs = find_binheader_writefs (data)
    for item in writefs:
        fmt = find_format_string (item)
        params = get_param_list (item)
        param_index = 0

        # print item

        for ch in fmt:
            if ch in 'Eet ':
                continue

            if ch == 'b':
                if params [param_index][:4] == "BHWv" and params [param_index + 1][:4] == "BHWz":
                    param_index += 2
                    continue

            if "BHW" + ch == params [param_index][:4]:
                param_index += 1
                continue

            if errors == 0: sys.stdout.write ("\n")
            print ("\n%s: error for format specifier '%c' (index %d) in:\n    %s\n" % (fname, ch, param_index, item))
            errors += 1
            # Break out of 'for ch in fmt' loop
            break

    return errors

#===============================================================================

if len (sys.argv) > 1:
    sys.stdout.write ("\n    binheader_writef_check                   : ")
    sys.stdout.flush ()
    errors = 0
    for fname in sys.argv [1:]:
        errors += handle_file (fname)
    if errors > 0:
        print ("\nErrors : %d\n" % errors)
        sys.exit (1)

print ("ok\n")

