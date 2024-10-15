<!-- ...................................................................... -->
<!-- XHTML Character Entities Module  ......................................... -->
<!-- file: xhtml-charent-1.mod

     This is XHTML, a reformulation of HTML as a modular XML application.
     Copyright 1998-2000 W3C (MIT, INRIA, Keio), All Rights Reserved.
     Revision: $Id: xhtml-charent-1.mod,v 1.1 2001/02/13 12:24:22 ht Exp $ SMI

     This DTD module is identified by the PUBLIC and SYSTEM identifiers:

       PUBLIC "-//W3C//ENTITIES XHTML Character Entities 1.0//EN"
       SYSTEM "http://www.w3.org/TR/xhtml-modulatization/DTD/xhtml-charent-1.mod"

     Revisions:
     (none)
     ....................................................................... -->

<!-- Character Entities for XHTML

     This module declares the set of character entities for XHTML,
     including the Latin 1, Symbol and Special character collections.
-->

<!ENTITY % xhtml-lat1
    PUBLIC "-//W3C//ENTITIES Latin 1 for XHTML//EN"
           "xhtml-lat1.ent" >
<!ENTITY % xhtml-symbol
    PUBLIC "-//W3C//ENTITIES Symbols for XHTML//EN"
           "xhtml-symbol.ent" >
<!ENTITY % xhtml-special
    PUBLIC "-//W3C//ENTITIES Special for XHTML//EN"
           "xhtml-special.ent" >

%xhtml-lat1;
%xhtml-symbol;
%xhtml-special;

<!-- end of xhtml-charent-1.mod -->
