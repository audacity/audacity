/**********************************************************************

  Audacity: A Digital Audio Editor

  ImportLOF.h

  David I. Murray

  Supports the opening of ".lof" files which are text files that contain
  a list of individual files to open in audacity in specific formats.

  (In BNF) The syntax for an LOF file, denoted by <lof>:

  <lof> ::= [<window> | <file> | <#>]*
  <window> ::= window [<window-parameter>]* <newline>
  <window-parameter> ::= offset <time> | duration <time>
  <time> ::= [<digit>]+ [ . [<digit>]* ]
  <file> ::= file [<file-parameter>]* <newline>
  <file-parameter> ::= offset <time>
  <#> ::= <comment> <newline>

  EXAMPLE LOF file:

  # everything following the hash character is ignored
  window # an initial window command is implicit and optional
  file "C:\folder1\sample1.wav"    # sample1.wav is displayed
  file "C:\sample2.wav" offset 5   # sample2 is displayed with a 5s offset
  File "C:\sample3.wav"            # sample3 is displayed with no offset
  window offset 5 duration 10      # open a NEW window, zoom to display
  # 10 seconds total starting at 5 (ending at 15) seconds
  file "C:\sample3.wav" offset 2.5

  SEMANTICS:

  There are two commands: "window" creates a NEW window, and "file"
  appends a track to the current window and displays the file there. The
  first file is always placed in a NEW window, whether or not an initial
  "window" command is given.

  Commands have optional keyword parameters that may be listed in any
  order. A parameter should only occur once per command. The "offset"
  parameter specifies a time offset. For windows, this is the leftmost
  time displayed in the window. For files, the offset is an amount by
  which the file is shifted in time before display (only enabled for audio;
  not midi). The offset is specified as an integer or decimal number of
  seconds, and the default value is zero.

  Windows may also have a "duration" parameter, which specifies how much
  time should be displayed in the window. The default duration is equal
  to the duration of the longest track currently displayed.

**********************************************************************/

#ifndef __AUDACITY_IMPORT_LOF__
#define __AUDACITY_IMPORT_LOF__

#include "ImportForwards.h"

void GetLOFImportPlugin(ImportPluginList &importPluginList,
                        UnusableImportPluginList &unusableImportPluginList);

#endif
