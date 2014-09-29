#!/usr/bin/python
#
# smartmsgmerge.py
#
# Written by Dominic Mazzoni, 2006
# GNU General Public License 2.0
#
# This is a replacement for the GNU gettext "msgmerge" program, which
# is typically used to update a .po file (def) to the latest .pot file
# (ref).  This program is not command-line compatible; it takes no
# flags but simply the def, ref, and output file names.
#
# It uses a much faster and also much stricter policy for finding new
# fuzzy matches: the edit-distance must be no more than 4%, or for very
# short strings, no more than 1 character.
#
# This makes it safe for you to enable fuzzy strings in your .mo file
# without worrying that they'll be too terrible.
#
# It also fixes translations where the beginning and ending newlines
# do not match the original string.
#

import sys, os

if len(sys.argv) != 4:
  print "Usage: %s def.po ref.pot out.po" % sys.argv[0]
  sys.exit()

def_filename = sys.argv[1]
ref_filename = sys.argv[2]
out_filename = sys.argv[3]

# Each object will contain the comments, msgid (untranslated),
# msgstr (translated), and a fuzzy flag.  For simplicity this is
# not a typechecked class, just a dummy dynamic container class.
class obj:
  pass

# Compute the edit-distance between str1 and str2, taking a couple
# of shortcuts such that it returns 999 quickly if the edit-distance
# is clearly not going to be less than 10 percent.
def edit_distance(str1, str2):
  l1 = len(str1)
  l2 = len(str2)

  # Exit if the difference in the string lenghts is 10% or more
  if l1*1.0/l2 < 0.9 or l1*1.0/l2 > 1.1:
    return 999

  # Compute a beam width of +/- 5% - the path through the matrix cannot
  # go outside the main diagonal +/- the beam.
  beam = int(0.5 + 0.1 * ((l1 + l2) / 2))

  # Create a 2D array
  d = [None]*(l1+1)
  for i in range(l1+1):
    d[i] = [999]*(l2+1)

  # Initialize the first row and column
  for i in range(l1+1):
    d[i][0] = i
  for j in range(l2+1):
    d[0][j] = j

  # Dynamic programming
  for i in range(1, l1+1):
    # Quick short-circuit after 30 rows; stop if things are
    # looking really bad
    if i==30 and l2>=30 and d[29][29] > 20:
      return 999
    for j in range(max(1, i-beam), min(l2+1, i+beam+1)):
      if str1[i-1] == str2[j-1]:
        cost = 0
      else:
        cost = 1
      d[i][j] = min(
                    d[i-1][j] + 1,       # deletion
                    d[i][j-1] + 1,       # insertion
                    d[i-1][j-1] + cost   # substitution
                   )
  return d[l1][l2]

# Take a string and format it on a bunch of separate lines in quotes
def quote(str):
  if str=="":
    return "\"\"\n"
  q = ""
  p = str.find("\\n")
  while p >= 0:
    line = str[:p]
    str = str[p+2:]
    q += "\"%s\\n\"\n" % line
    p = str.find("\\n")
  if len(str) > 0:
    q += "\"%s\"\n" % str
  return q

# Take a bunch of separate lines in quotes and turn them into a single string
def unquote(str):
  u = ""
  for line in str.split("\n"):
    line = line.strip()
    if len(line)>=2:
      if line[0]=='"' and line[-1]=='"':
        u += line[1:-1]
      else:
        print "Error with:"
        print '**%s**' % line
        sys.exit()
  return u

# Parse one file in the .po / .pot format, returning a hash of all
# msgids and a list of all msgids in order.
def parse(fname):
  h = {}
  l = []
  msgid = ""
  msgstr = ""
  comments = ""
  fuzzy = False
  first = True
  line_no = 0

  # Read the lines of the file and make sure it always ends in a
  # blank line
  lines = open(fname).readlines()
  lines.append("\n")

  for line in (lines + ["\n"]):
    # Handle DOS line endings
    if len(line)>=2 and line[-2]=='\r' and line[-1]=='\n':
      line = line[:-2]+'\n'

    line_no += 1
    if line=="\n":
      if len(msgid)==0 and not first:
        # We found a blank line or comments in the middle of nowhere
        comments = ""
        fuzzy = False
        msgstr = ""
        continue
      # Otherwise, a blank line in the middle of the file
      # signifies the end of a translation
      msgid = unquote(msgid)
      msgstr = unquote(msgstr)
      if msgid in h:
        print "Duplicate msgid in %s:" % (fname)
        print quote(msgid)
        print "Found on line %d, previously defined on line %d" % \
          (line_no, h[msgid].line_no)
        sys.exit()
      o = obj()
      o.comments = comments
      o.msgid = msgid
      o.msgstr = msgstr
      o.fuzzy = fuzzy
      o.line_no = line_no
      h[msgid] = o
      l.append(msgid)
      comments = ""
      msgstr = ""
      msgid = ""
      fuzzy = False
      first = False
    elif len(line)>=8 and line[:8] == "#, fuzzy":
      fuzzy = True
      comments += line
    elif line[0] == '#':
      comments += line
    elif len(line)>6 and line[:6]=="msgid ":
      msgid += line[6:]
    elif len(line)>7 and line[:7]=="msgstr ":
      msgstr += line[7:]
    else:
      if len(msgstr):
        msgstr += line
      else:
        msgid += line
  return (h, l)

(def_h, def_l) = parse(def_filename)
(ref_h, ref_l) = parse(ref_filename)

# Handle the exact matches
final_h = {}
for msgid in ref_l:
  if msgid in def_h:
    final_h[msgid] = def_h[msgid]

# Try for fuzzy matches
for ref_msgid in [x for x in ref_l if x not in final_h]:
  min_ed = 999
  min_msgid = None
  for def_msgid in [x for x in def_l if x not in final_h]:
    if len(def_h[def_msgid].msgstr) < 3:
      continue
    ed = edit_distance(ref_msgid, def_msgid)
    if ed < min_ed:
      min_ed = ed
      min_msgid = def_msgid
  if min_msgid != None:
    pct = min_ed * 100.0 / min(len(ref_msgid), len(min_msgid))
    if min_ed == 1 or pct <= 4.0:
      refstr = ref_msgid
      if len(refstr)>40:
        refstr = refstr[:37]+"..."
      minstr = min_msgid
      if len(minstr)>40:
        minstr = minstr[:37]+"..."
      print "Found fuzzy match:"
      print "   %s" % refstr
      print "   %s" % minstr
      print "     def_len=%d, ref_len=%d, edit_distance=%d" % \
          (len(min_msgid), len(ref_msgid), min_ed)
      o = obj()
      def_h[min_msgid]
      o.msgid = ref_msgid
      o.comments = ref_h[ref_msgid].comments
      o.msgstr = def_h[min_msgid].msgstr
      o.fuzzy = True
      final_h[ref_msgid] = o

# Generate output file
translated = 0
fuzzy = 0
empty = 0
out_fp = open(out_filename, "w")
for msgid in ref_l:
  if msgid in final_h:
    o = final_h[msgid]
    if o.fuzzy:
      fuzzy += 1
    elif msgid != "":
      translated += 1
  else:
    o = ref_h[msgid]
    empty += 1

  msgstr = o.msgstr
  # Fix leading and trailing newlines
  if len(msgid)>4 and len(msgstr)>4:
    # Add newline if missing
    if msgid[:2]=="\\n" and msgstr[:2]!="\\n":
      msgstr = "\\n" + msgstr
    if msgid[-2:]=="\\n" and msgstr[-2:]!="\\n":
      msgstr = msgstr + "\\n"

    # Remove newline if extraneous
    if msgid[:2]!="\\n" and msgstr[:2]=="\\n":
      msgstr = msgstr[2:]
    if msgid[-2:]!="\\n" and msgstr[-2:]=="\\n":
      msgstr = msgstr[:-2]

  # Write the entry
  out_fp.write(o.comments)
  if o.fuzzy and o.comments.find("fuzzy")==-1:
    out_fp.write("#, fuzzy\n")
  out_fp.write("msgid " + quote(msgid))
  out_fp.write("msgstr " + quote(msgstr))
  out_fp.write("\n")

# Print stats
print "Translated: %d Fuzzy: %d Empty: %d" % (translated, fuzzy, empty)
print "Wrote output to %s" % out_filename
