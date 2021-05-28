/**********************************************************************
 
 Audacity: A Digital Audio Editor
 
 @file UnusedStrings.h
 
 Paul Licameli
 
 **********************************************************************/

/*
This file contains strings and comments put into the source code tree for the
string freeze of a release, but while not yet in feature freeze.  This allows
the translation catalog template, locale/audacity.pot, to stabilize long enough
in advance of the release, that translators have enough opportunity to update
the language-specific catalogs.

These strings, and their i18n-hint comments, can be cut and pasted elsewhere
during the continued development in string freeze.

Some example strings are also given first, to document the syntax.
 */

// //////////////////////////////////////////////// Begin examples

// A translated string may also have an accompanying special comment

// i18n-hint to lower oneself
XO("Get down"),

/* i18n-hint An example of a longer special comment that can wrap lines.
 These comments must begin with the special word i18n-hint and immediately
 precede the source code line with the opening quotation mark.  The translators
 will see this comment text in the generated .po files, which they edit and
 complete.  This comment can give hints about unusual words, or the grammatical
 form of an English word (such as whether it's meant as noun or verb) when that
 is not clear from context.
 */
XO("Get down"),

XO(
   /* i18n-hint one more example of alternative correct placement of an
    internationalization comment.  All comments on different occurrences of the
    same string are collected in the template file.  */
   "Get down"),

// Next is an example of a string with a disambiguating context.  This allows
// one English string to have more than one entry in the translation catalog.
// The context string is not itself seen by users or translated by translators.
// It only allows the code at runtime to choose the correct translation.

// i18n-hint Have fun and dance
XC("Get down", "party"),

// i18n-hint Obtain the underfeathers of a goose
XC("Get down", "pillows"),

// Strings may have blanks in which names are substituted, such as a file name.
// For good internationalization, allowing for variations of grammar between
// languages, and giving translators the needed context, we substitute names
// into larger phrases.  Do not build up messages by concatenating less complete
// phrases.
XO("Your batch command of %s was not recognized."),

// Sometimes strings must be given as singular-plural pairs.  The translation
// system may make more or fewer than two entries in the catalog, depending
// on the grammatical rules of the language.  The 0 is a placeholder for an
// expression in real code that is used to choose the proper string at
// lookup time.
// Also %d is a blank appropriate for a number instead of a name.

// i18n-hint
XP("Got down one time", "Got down %d times", 0),

// Finally, singular-plural pairs may also have context strings.

// i18n-hint Obtained underfeathers of geese on one or more occasions
XPC("Got down one time", "Got down %d times", 0, "pillows"),

// //////////////////////////////////////////////// End examples
