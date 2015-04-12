/**********************************************************************

   Audacity: A Digital Audio Editor
   Audacity(R) is copyright (c) 1999-2015 Audacity Team.
   License: GPL v2.  See License.txt.

   RevisionIdent.h


********************************************************************//*!

\file RevisionIdent.h

  This entire file will be replaced by the revision identifier string 
  based on the branch SHA when the automated build system builds
  Audacity.  That striing will look something like:

  "<a href=\"https://github.com/audacity/audacity/commit/
  7f2e83995596367aeed69f3086ac9fd2039795a3\">7f2e839</a> of 
  Thu Apr 9 20:03:11 2015 +0100"

*//********************************************************************/

// The commented out string below is like the one the build servers
// will replace this file with.
//wxT("<a href=\"http://github.com/audacity/audacity/commit/28864acb238cb3ca71dda190a2d93242591dd80e\">28864ac</a> of Sun Apr 12 12:40:22 2015 +0100")


// The string below is what you get if 
// the build system does not replace this file.
wxT("No revision identifier was provided")

