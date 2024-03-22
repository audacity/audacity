#ifndef MU_TINYXML_PATCH_H
#define MU_TINYXML_PATCH_H

//! NOTE
//! By default, declarations are only allowed at document level.
//! But the XML specification says that they can be anywhere.
//! In particular, in the MusicXML there are declarations in different places (from Sibelius)
//! Therefore, the code was modified to simply skip such declarations.

#define TINYXML_SKIP_DECLARATION_IN_MIDDLE

#endif // MU_TINYXML_PATCH_H
