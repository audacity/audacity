
"""
Manipulate HTML or XHTML documents.

Version 1.1.1.  This source code has been placed in the
public domain by Connelly Barnes.

Features:

 - Translate HTML back and forth to data structures.
   This allows you to read and write HTML documents
   programmably, with much flexibility.
 - Extract and modify URLs in an HTML document.
 - Compatible with Python 3+

See the L{examples} for a quick start.

Moved to Python3 by Jack Thomson May 2020

"""

__version__ = '1.1.2'

__all__ = ['examples', 'tagextract', 'tagjoin', 'urlextract',
           'urljoin', 'URLMatch']

# -------------------------------------------------------------------
# Globals
# -------------------------------------------------------------------

import re
import shlex
import string
import urllib.request, urllib.parse, urllib.error
import urllib.parse
import types

# Translate text between these strings as plain text (not HTML).
_IGNORE_TAGS = [('script', '/script'),
               ('style', '/style')]

# Special tags where we have to look for _END_X as part of the
# HTML/XHTML parsing rules.
_BEGIN_COMMENT = '<!--'
_END_COMMENT = '-->'
_BEGIN_CDATA = '<![CDATA['
_END_CDATA = ']]>'

# Mime types that can be parsed as HTML or HTML-like.
_HTML_MIMETYPES = ['text/html', 'application/xhtml',
                   'application/xhtml+xml', 'text/xml',
                   'application/xml']

# Mime types that can be parsed as CSS.
_CSS_MIMETYPES = ['text/css']

# -------------------------------------------------------------------
# HTML <-> Data structure
# -------------------------------------------------------------------

def tagextract(doc):
    """
    Convert HTML to data structure.

    Returns a list.  HTML tags become C{(name, keyword_dict)} tuples
    within the list, while plain text becomes strings within the
    list.  All tag names are lowercased and stripped of whitespace.
    Tags which end with forward slashes have a single forward slash
    placed at the end of their name, to indicate that they are XML
    unclosed tags.

    Example:

     >>> tagextract('<img src=hi.gif alt="hi">foo<br><br/></body>')
     [('img', {'src': 'hi.gif', 'alt': 'hi'}), 'foo',
      ('br', {}), ('br/', {}), ('/body', {})]

    Text between C{'<script>'} and C{'<style>'} is rendered directly to
    plain text. This prevents rogue C{'<'} or C{'>'} characters from
    interfering with parsing.

     >>> tagextract('<script type="a"><blah>var x; </script>')
     [('script', {'type': 'a'}), '<blah>var x; ', ('/script', {})]

    Comment strings and XML directives are rendered as a single long
    tag with no attributes.  The case of the tag "name" is not changed:

     >>> tagextract('<!-- blah -->')
     [('!-- blah --', {})]
     >>> tagextract('<?xml version="1.0" encoding="utf-8" ?>')
     [('?xml version="1.0" encoding="utf-8" ?', {})]
     >>> tagextract('<!DOCTYPE html PUBLIC etc...>')
     [('!DOCTYPE html PUBLIC etc...', {})]

    Greater-than and less-than characters occurring inside comments or
    CDATA blocks are correctly kept as part of the block:

     >>> tagextract('<!-- <><><><>>..> -->')
     [('!-- <><><><>>..> --', {})]
     >>> tagextract('<!CDATA[[><>><>]<> ]]>')
     [('!CDATA[[><>><>]<> ]]', {})]

    Note that if one modifies these tags, it is important to retain the
    C{"--"} (for comments) or C{"]]"} (for C{CDATA}) at the end of the
    tag name, so that output from L{tagjoin} will be correct HTML/XHTML.

    """
    L = _full_tag_extract(doc)
    for i in range(len(L)):
        if isinstance(L[i], _TextTag):
            # _TextTag object.
            L[i] = L[i].text
        else:
            # _HTMLTag object.
            L[i] = (L[i].name, L[i].attrs)
    return L

def _is_str(s):
    """
    True iff s is a string (checks via duck typing).
    """
    return hasattr(s, 'capitalize')

def tagjoin(L):
    """
    Convert data structure back to HTML.

    This reverses the L{tagextract} function.

    More precisely, if an HTML string is turned into a data structure,
    then back into HTML, the resulting string will be functionally
    equivalent to the original HTML.

     >>> tagjoin(tagextract(s))
     (string that is functionally equivalent to s)

    Three changes are made to the HTML by L{tagjoin}: tags are
    lowercased, C{key=value} pairs are sorted, and values are placed in
    double-quotes.
    """
    if _is_str(L):
        raise ValueError('got string arg, expected non-string iterable')
    ans = []
    for item in L:
        # Check for string using duck typing.
        if _is_str(item):
            # Handle plain text.
            ans.append(item)
        elif item[0] == '--':
            # Handle closing comment.
            ans.append('-->')
        elif item[0] == '!--':
            # Handle opening comment.
            ans.append('<!--')
        else:
            # Handle regular HTML tag.
            (name, d) = item
            if name[-1:] == '/':
                rslash = ' /'
                name = name[:-1]
            else:
                rslash = ''
            tag_items = []
            items = list(d.items())
            items.sort()
            for (key, value) in items:
                if value != None:
                    if '"' in value and "'" in value:
                        raise ValueError('attribute value contains both single' +
                                         ' and double quotes')
                    elif '"' in value:
                        tag_items.append(key + "='" + value + "'")
                    else:
                        tag_items.append(key + '="' + value + '"')
                else:
                    tag_items.append(key)
            tag_items = ' '.join(tag_items)
            if tag_items != '':
                tag_items = ' ' + tag_items
            ans.append('<' + name + tag_items + rslash + '>')
    return ''.join(ans)

def _enumerate(L):
    """
    Like C{enumerate}, provided for compatibility with Python < 2.3.

    Returns a list instead of an iterator.
    """
    return list(zip(list(range(len(L))), L))

def _ignore_tag_index(s, i):
    """
    Helper routine: Find index within C{_IGNORE_TAGS}, or C{-1}.

    If C{s[i:]} begins with an opening tag from C{_IGNORE_TAGS}, return
    the index.  Otherwise, return C{-1}.
    """
    for (j, (a, b)) in _enumerate(_IGNORE_TAGS):
        if s[i:i + len(a) + 1].lower() == '<' + a:
            return j
    return - 1

def _html_split(s):
    """
    Helper routine: Split string into a list of tags and non-tags.

     >>> html_split(' blah <tag text> more </tag stuff> ')
     [' blah ', '<tag text>', ' more ', '</tag stuff>', ' ']

    Tags begin with C{'<'} and end with C{'>'}.

    The identity C{''.join(L) == s} is always satisfied.

    Exceptions to the normal parsing of HTML tags:

    C{'<script>'}, C{'<style>'}, and HTML comment tags ignore all HTML
    until the closing pair, and are added as three elements:

     >>> html_split(' blah<style><<<><></style><!-- hi -->' +
     ...            ' <script language="Javascript"></>a</script>end')
     [' blah', '<style>', '<<<><>', '</style>', '<!--', ' hi ', '-->',
      ' ', '<script language="Javascript">', '</>a', '</script>', 'end']

    """
    s_lower = s.lower()
    L = []

    i = 0               # Index of char being processed
    while i < len(s):
        c = s[i]
        if c == '<':
            # Left bracket, handle various cases.
            if s[i:i + len(_BEGIN_COMMENT)].startswith(_BEGIN_COMMENT):
                # HTML begin comment tag, '<!--'.  Scan for '-->'.
                i2 = s.find(_END_COMMENT, i)
                if i2 < 0:
                    # No '-->'.  Append the remaining malformed content and stop.
                    L.append(s[i:])
                    break
                else:
                    # Append the comment.
                    L.append(s[i:i2 + len(_END_COMMENT)])
                    i = i2 + len(_END_COMMENT)
            elif s[i:i + len(_BEGIN_CDATA)].startswith(_BEGIN_CDATA):
                # XHTML begin CDATA tag.  Scan for ']]>'.
                i2 = s.find(_END_CDATA, i)
                if i2 < 0:
                    # No ']]>'.  Append the remaining malformed content and stop.
                    L.append(s[i:])
                    break
                else:
                    # Append the CDATA.
                    L.append(s[i:i2 + len(_END_CDATA)])
                    i = i2 + len(_END_CDATA)
            else:
                # Regular HTML tag.  Scan for '>'.
                orig_i = i
                found = False
                in_quot1 = False
                in_quot2 = False
                for i2 in range(i + 1, len(s)):
                    c2 = s[i2]
                    if c2 == '"' and not in_quot1:
                        in_quot2 = not in_quot2
                        # Only turn on double quote if it's in a realistic place.
                        if in_quot2 and not in_quot1:
                            if i2 > 0 and s[i2 - 1] not in [' ', '\t', '=']:
                                in_quot2 = False
                    elif c2 == "'" and not in_quot2:
                        in_quot1 = not in_quot1
                        # Only turn on single quote if it's in a realistic place.
                        if in_quot1 and not in_quot2:
                            if i2 > 0 and s[i2 - 1] not in [' ', '\t', '=']:
                                in_quot1 = False
                    elif c2 == '>' and (not in_quot2 and not in_quot1):
                        found = True
                        break

                if not found:
                    # No end '>'.  Append the rest as text.
                    L.append(s[i:])
                    break
                else:
                    # Append the tag.
                    L.append(s[i:i2 + 1])
                    i = i2 + 1

                # Check whether we found a special ignore tag, eg '<script>'
                tagi = _ignore_tag_index(s, orig_i)
                if tagi >= 0:
                    # It's an ignore tag.  Scan for the end tag.
                    i2 = s_lower.find('<' + _IGNORE_TAGS[tagi][1], i)
                    if i2 < 0:
                        # No end tag.  Append the rest as text.
                        L.append(s[i2:])
                        break
                    else:
                        # Append the text sandwiched between the tags.
                        L.append(s[i:i2])
                        # Catch the closing tag with the next loop iteration.
                        i = i2
        else:
            # Not a left bracket, append text up to next left bracket.
            i2 = s.find('<', i)
            if i2 < 0:
                # No left brackets, append the rest as text.
                L.append(s[i:])
                break
            else:
                L.append(s[i:i2])
            i = i2

    return L

def _shlex_split(s):
    """
    Like C{shlex.split}, but reversible, and for HTML.

    Splits a string into a list C{L} of strings.  List elements
    contain either an HTML tag C{name=value} pair, an HTML name
    singleton (eg C{"checked"}), or whitespace.

    The identity C{''.join(L) == s} is always satisfied.

     >>> _shlex_split('a=5 b="15" name="Georgette A"')
     ['a=5', ' ', 'b="15"', ' ', 'name="Georgette A"']

     >>> _shlex_split('a = a5 b=#b19 name="foo bar" q="hi"')
     ['a = a5', ' ', 'b=#b19', ' ', 'name="foo bar"', ' ', 'q="hi"']

     >>> _shlex_split('a="9"b="15"')
     ['a="9"', 'b="15"']
    """

    ans = []
    i = 0
    while i < len(s):
        c = s[i]
        if c in string.whitespace:
            # Whitespace.  Add whitespace while found.
            for i2 in range(i, len(s)):
                if s[i2] not in string.whitespace:
                    break
            # Include the entire string if the last char is whitespace.
            if s[i2] in string.whitespace:
                i2 += 1
            ans.append(s[i:i2])
            i = i2
        else:
            # Match 'name = "value"'
            c = re.compile(r'[^ \t\n\r\f\v"\']+\s*\=\s*"[^"]*"')
            m = c.match(s, i)
            if m:
                ans.append(s[i:m.end()])
                i = m.end()
                continue

            # Match "name = 'value'"
            c = re.compile(r'[^ \t\n\r\f\v"\']+\s*\=\s*\'[^\']*\'')
            m = c.match(s, i)
            if m:
                ans.append(s[i:m.end()])
                i = m.end()
                continue

            # Match 'name = value'
            c = re.compile(r'[^ \t\n\r\f\v"\']+\s*\=\s*[^ \t\n\r\f\v"\']*')
            m = c.match(s, i)
            if m:
                ans.append(s[i:m.end()])
                i = m.end()
                continue

            # Match 'name'
            c = re.compile(r'[^ \t\n\r\f\v"\']+')
            m = c.match(s, i)
            if m:
                ans.append(s[i:m.end()])
                i = m.end()
                continue

            # Couldn't match anything so far, so it's likely that the page
            # has malformed quotes inside a tag.  Add leading quotes
            # and spaces to the previous field until we see something.
            subadd = []
            while i < len(s) and s[i] in ['"', "'", ' ', '\t']:
                subadd.append(s[i])
                i += 1

            # Add whatever we could salvage from the situation and move on.
            if len(subadd) > 0:
                ans.append(''.join(subadd))
            else:
                # We totally failed at matching this character, so add it
                # as a separate item and move on.
                ans.append(s[i])

    return ans

def _test_shlex_split():
    """
    Unit test for L{_shlex_split}.
    """
    assert _shlex_split('') == []
    assert _shlex_split(' ') == [' ']
    assert _shlex_split('a=5 b="15" name="Georgette A"') == \
           ['a=5', ' ', 'b="15"', ' ', 'name="Georgette A"']
    assert _shlex_split('a=cvn b=32vsd  c= 234jk\te d \t="hi"') == \
           ['a=cvn', ' ', 'b=32vsd', '  ', 'c= 234jk', '\t', 'e', ' ',
            'd \t="hi"']
    assert _shlex_split(' a b c d=e f  g h i="jk" l mno = p  ' + \
                        'qr = "st"') == \
           [' ', 'a', ' ', 'b', ' ', 'c', ' ', 'd=e', ' ', 'f', '  ', \
            'g', ' ', 'h', ' ', 'i="jk"', ' ', 'l', ' ', 'mno = p', \
            '  ', 'qr = "st"']
    assert _shlex_split('a=5 b="9"c="15 dfkdfkj "d="25"') == \
           ['a=5', ' ', 'b="9"', 'c="15 dfkdfkj "', 'd="25"']
    assert _shlex_split('a=5 b="9"c="15 dfkdfkj "d="25" e=4') == \
           ['a=5', ' ', 'b="9"', 'c="15 dfkdfkj "', 'd="25"', ' ', \
            'e=4']
    assert _shlex_split('a=5 b=\'9\'c=\'15 dfkdfkj \'d=\'25\' e=4') == \
           ['a=5', ' ', 'b=\'9\'', 'c=\'15 dfkdfkj \'', 'd=\'25\'', \
            ' ', 'e=4']


def _tag_dict(s):
    """
    Helper routine: Extracts a dict from an HTML tag string.

     >>> _tag_dict('bgcolor=#ffffff text="#000000" blink')
     ({'bgcolor':'#ffffff', 'text':'#000000', 'blink': None},
      {'bgcolor':(0,7),  'text':(16,20), 'blink':(31,36)},
      {'bgcolor':(8,15), 'text':(22,29), 'blink':(36,36)})

    Returns a 3-tuple.  First element is a dict of
    C{(key, value)} pairs from the HTML tag.  Second element
    is a dict mapping keys to C{(start, end)} indices of the
    key in the text.  Third element maps keys to C{(start, end)}
    indices of the value in the text.

    Names are lowercased.

    Raises C{ValueError} for unmatched quotes and other errors.
    """
    d = _shlex_split(s)
    attrs = {}
    key_pos = {}
    value_pos = {}
    start = 0
    for item in d:
        end = start + len(item)
        equals = item.find('=')
        if equals >= 0:
            # Contains an equals sign.
            (k1, k2) = (start, start + equals)
            (v1, v2) = (start + equals + 1, start + len(item))

            # Strip spaces.
            while k1 < k2 and s[k1] in string.whitespace:   k1 += 1
            while k1 < k2 and s[k2 - 1] in string.whitespace: k2 -= 1

            while v1 < v2 and s[v1] in string.whitespace:   v1 += 1
            while v1 < v2 and s[v2 - 1] in string.whitespace: v2 -= 1

            # Strip one pair of double quotes around value.
            if v1 < v2 - 1 and s[v1] == '"' and s[v2 - 1] == '"':
                v1 += 1
                v2 -= 1

            # Strip one pair of single quotes around value.
            if v1 < v2 - 1 and s[v1] == "'" and s[v2 - 1] == "'":
                v1 += 1
                v2 -= 1

            (key, value) = (s[k1:k2].lower(), s[v1:v2])

            # Drop bad keys and values.
            if '"' in key or "'" in key:
                continue
            if '"' in value and "'" in value:
                continue

            attrs[key] = value
            key_pos[key] = (k1, k2)
            value_pos[key] = (v1, v2)
        elif item.split() == []:
            # Whitespace.  Ignore it.
            pass
        else:
            # A single token, like 'blink'.
            key = item.lower()

            # Drop bad keys.
            if '"' in key or "'" in key:
                continue

            attrs[key] = None
            key_pos[key] = (start, end)
            value_pos[key] = (end, end)
        start = end

    return (attrs, key_pos, value_pos)

def _test_tag_dict():
    """
    Unit test for L{_tag_dict}.
    """
    assert _tag_dict('') == ({}, {}, {})
    assert _tag_dict(' \t\r \n\n \r\n  ') == ({}, {}, {})
    assert _tag_dict('bgcolor=#ffffff text="#000000" blink') == \
      ({'bgcolor':'#ffffff', 'text':'#000000', 'blink': None},
       {'bgcolor':(0, 7), 'text':(16, 20), 'blink':(31, 36)},
       {'bgcolor':(8, 15), 'text':(22, 29), 'blink':(36, 36)})
    assert _tag_dict("bgcolor='#ffffff'text='#000000' blink") == \
      ({'bgcolor':'#ffffff', 'text':'#000000', 'blink': None},
       {'bgcolor':(0, 7), 'text':(17, 21), 'blink':(32, 37)},
       {'bgcolor':(9, 16), 'text':(23, 30), 'blink':(37, 37)})
    s = ' \r\nbg = val text \t= "hi you" name\t e="5"\t\t\t\n'
    (a, b, c) = _tag_dict(s)
    assert a == {'text': 'hi you', 'bg': 'val', 'e': '5', 'name': None}
    for key in list(a.keys()):
        assert s[b[key][0]:b[key][1]] == key
        if a[key] != None:
            assert s[c[key][0]:c[key][1]] == a[key]

def _full_tag_extract(s):
    """
    Like L{tagextract}, but different return format.

    Returns a list of L{_HTMLTag} and L{_TextTag} instances.

    The return format is very inconvenient for manipulating HTML, and
    only will be useful if you want to find the exact locations where
    tags occur in the original HTML document.
    """
    L = _html_split(s)

    # Starting position of each L[i] in s.
    Lstart = [0] * len(L)
    for i in range(1, len(L)):
        Lstart[i] = Lstart[i - 1] + len(L[i - 1])

    class NotTagError(Exception): pass

    for (i, text) in _enumerate(L):
        try:

            # Is it an HTML tag?
            is_tag = False
            if len(text) >= 2 and text[0] == '<' and text[-1] == '>':
                # Turn HTML tag text into (name, keyword_dict) tuple.
                is_tag = True

            is_special = False
            if len(text) >= 2 and (text[1] == '!' or text[1] == '?'):
                is_special = True

            if is_special:
                # A special tag such as XML directive or <!-- comment -->
                pos = (Lstart[i], Lstart[i] + len(L[i]))

                # Wrap inside an _HTMLTag object.
                L[i] = _HTMLTag(pos, text[1:-1].strip(), {}, {}, {})

            elif is_tag:
                # If an HTML tag, strip brackets and handle what's left.

                # Strip off '<>' and update offset.
                orig_offset = 0
                if len(text) >= 1 and text[0] == '<':
                    text = text[1:]
                    orig_offset = 1
                if len(text) >= 1 and text[-1] == '>':
                    text = text[:-1]

                if len(text) > 0 and text[-1] == '/':
                    rslash = True
                    text = text[:-1]
                else:
                    rslash = False

                m = re.search(r'\s', text)
                first_space = -1
                if m:
                    first_space = m.start()
                if first_space < 0:
                    (name, dtext) = (text, '')
                else:
                    name = text[:first_space]
                    dtext = text[first_space + 1:len(text)]

                # Position of dtext relative to original text.
                dtext_offset = len(name) + 1 + orig_offset    # +1 for space.

                # Lowercase everything except XML directives and comments.
                if not name.startswith('!') and not name.startswith('?'):
                    name = name.strip().lower()

                if rslash:
                    name += '/'

                # Strip off spaces, and update dtext_offset as appropriate.
                orig_dtext = dtext
                dtext = dtext.strip()
                dtext_offset += orig_dtext.index(dtext)

                (attrs, key_pos, value_pos) = _tag_dict(dtext)
                # Correct offsets in key_pos and value_pos.
                for key in list(attrs.keys()):
                    key_pos[key] = (key_pos[key][0] + Lstart[i] + dtext_offset,
                                      key_pos[key][1] + Lstart[i] + dtext_offset)
                    value_pos[key] = (value_pos[key][0] + Lstart[i] + dtext_offset,
                                      value_pos[key][1] + Lstart[i] + dtext_offset)

                pos = (Lstart[i], Lstart[i] + len(L[i]))

                # Wrap inside an _HTMLTag object.
                L[i] = _HTMLTag(pos, name, attrs, key_pos, value_pos)
            else:
                # Not an HTML tag.
                raise NotTagError
        except NotTagError:
            # Wrap non-HTML strings inside a _TextTag object.
            pos = (Lstart[i], Lstart[i] + len(L[i]))
            L[i] = _TextTag(pos, L[i])

    return L


class _HTMLTag:
    """
    HTML tag extracted by L{_full_tag_extract}.

    @ivar pos:       C{(start, end)} indices of the entire tag in the
                     HTML document.
    @ivar name:      Name of tag.  For example, C{'img'}.
    @ivar attrs:     Dictionary mapping tag attributes to corresponding
                     tag values.

                     Example:

                      >>> tag = _full_tag_extract('<a href="d.com">')[0]
                      >>> tag.attrs
                      {'href': 'd.com'}

                     Surrounding quotes are stripped from the values.
    @ivar key_pos:   Key position dict.

                     Maps the name of a tag attribute to C{(start, end)}
                     indices for the key string in the C{"key=value"}
                     HTML pair.  Indices are absolute, where 0 is the
                     start of the HTML document.

                     Example:

                      >>> tag = _full_tag_extract('<a href="d.com">')[0]
                      >>> tag.key_pos['href']
                      (3, 7)
                      >>> '<a href="d.com">'[3:7]
                      'href'

    @ivar value_pos: Value position dict.

                     Maps the name of a tag attribute to C{(start, end)}
                     indices for the value in the HTML document string.
                     Surrounding quotes are excluded from this range.
                     Indices are absolute, where 0 is the start of the
                     HTML document.

                     Example:

                      >>> tag = _full_tag_extract('<a href="d.com">')[0]
                      >>> tag.value_pos['href']
                      (9, 14)
                      >>> '<a href="d.com">'[9:14]
                      'd.com'
    """

    def __init__(self, pos, name, attrs, key_pos, value_pos):
        """
        Create an _HTMLTag object.
        """
        self.pos = pos
        self.name = name
        self.attrs = attrs
        self.key_pos = key_pos
        self.value_pos = value_pos

class _TextTag:
    """
    Text extracted from an HTML document by L{_full_tag_extract}.

    @ivar text:   Extracted text.
    @ivar pos:    C{(start, end)} indices of the text.
    """

    def __init__(self, pos, text):
        """
        Create a _TextTag object.
        """
        self.pos = pos
        self.text = text

# -------------------------------------------------------------------
# URL Editing
# -------------------------------------------------------------------

# Tags within which URLs may be found.
_URL_TAGS = ['a href', 'applet archive', 'applet code',
            'applet codebase', 'area href', 'base href',
            'blockquote cite', 'body background', 'del cite',
            'form action', 'frame longdesc', 'frame src',
            'head profile', 'iframe src', 'iframe longdesc',
            'img src', 'img ismap', 'img longdesc', 'img usemap',
            'input src', 'ins cite', 'link href', 'object archive',
            'object codebase', 'object data', 'object usemap',
            'script src', 'table background', 'tbody background',
            'td background', 'tfoot background', 'th background',
            'thead background', 'tr background']
_URL_TAGS = [tuple(s.split()) for s in _URL_TAGS]


def _finditer(pattern, string):
    """
    Like C{re.finditer}, provided for compatibility with Python < 2.3.

    Returns a list instead of an iterator.  Otherwise the return format
    is identical to C{re.finditer} (except possibly in the details of
    empty matches).
    """
    compiled = re.compile(pattern)
    ans = []
    start = 0

    while True:
        m = compiled.search(string, start)
        if m:
            ans.append(m)
        else:
            return ans

        m_start = m.start(m.lastindex)
        m_end = m.end(m.lastindex)
        if m_end > m_start:
            start = m_end
        else:
            start += 1

def _remove_comments(doc):
    """
    Replaces commented out characters with spaces in a CSS document.
    """
    ans = []
    i = 0
    while True:
        i2 = doc.find('/*', i)
        if i2 < 0:
            ans += [doc[i:]]
            break
        ans += [doc[i:i2]]
        i3 = doc.find('*/', i2 + 1)
        if i3 < 0:
            i3 = len(doc) - 2
        ans += [' ' * (i3 - i2 + 2)]
        i = i3 + 2

    return ''.join(ans)

def _test_remove_comments():
    """
    Unit test for L{_remove_comments}.
    """
    s = '/*d s kjlsdf */*//*/*//**/**/*//**/a' * 50
    assert len(_remove_comments(s)) == len(s)
    s = '/**/' * 50 + '/*5845*/*/*//*/**/dfd' + '/*//**//'
    assert len(_remove_comments(s)) == len(s)
    s = 'a/**/' * 50 + '/**//**/////***/****/*//**//*/' * 5
    assert len(_remove_comments(s)) == len(s)
    s = 'hi /* foo */ hello /* bar!!!!! \n\n */ there!'
    assert _remove_comments(s) == \
           'hi           hello                   there!'

def urlextract(doc, siteurl=None, mimetype='text/html'):
    """
    Extract URLs from HTML or stylesheet.

    Extracts only URLs that are linked to or embedded in the document.
    Ignores plain text URLs that occur in the non-HTML part of the
    document.

    Returns a list of L{URLMatch} objects.

     >>> L = urlextract('<img src="a.gif"><a href="www.google.com">')
     >>> L[0].url
     'a.gif'
     >>> L[1].url
     'www.google.com'

    If C{siteurl} is specified, all URLs are made into absolute URLs
    by assuming that C{doc} is located at the URL C{siteurl}.

     >>> doc = '<img src="a.gif"><a href="/b.html">'
     >>> L = urlextract(doc, 'http://www.python.org/~guido/')
     >>> L[0].url
     'http://www.python.org/~guido/a.gif'
     >>> L[1].url
     'http://www.python.org/b.html'

    If C{mimetype} is C{"text/css"}, the document will be parsed
    as a stylesheet.

    If a stylesheet is embedded inside an HTML document, then
    C{urlextract} will extract the URLs from both the HTML and the
    stylesheet.
    """
    mimetype = mimetype.lower()
    if mimetype.split()[0] in _CSS_MIMETYPES:
        doc = _remove_comments(doc)
        # Match URLs within CSS stylesheet.
        # Match url(blah) or url('blah') or url("blah").
        L = _finditer(
          r'''url\s*\(([^\r\n\("']*?)\)|''' +
          r'''url\s*\(\s*"([^\r\n]*?)"\s*\)|''' +
          r'''url\s*\(\s*'([^\r\n]*?)'\s*\)|''' +
          r'''@import\s+([^ \t\r\n"';@\(\)]+)[^\r\n;@\(\)]*[\r\n;]|''' +
          r'''@import\s+'([^ \t\r\n"';@\(\)]+)'[^\r\n;@\(\)]*[\r\n;]|''' +
          r'''@import\s+"([^ \t\r\n"';\(\)']+)"[^\r\n;@\(\)]*[\r\n;]''',
          doc + ';\n')

        L = [(x.start(x.lastindex), x.end(x.lastindex)) for x in L]
        ans = []
        for (s, e) in L:
            e = min(e, len(doc))
            if e > s:
                ans.append(URLMatch(doc, s, e, siteurl, False, True))
    elif mimetype.split()[0] in _HTML_MIMETYPES:
        # Match URLs within HTML document.
        ans = []
        L = _full_tag_extract(doc)
        item = None
        for i in range(len(L)):
            prev_item = item
            item = L[i]

            # Handle string item (text) or tuple item (tag).
            if isinstance(item, _TextTag):
                # Current item is text.
                if isinstance(prev_item, _HTMLTag) and prev_item.name == \
                   'style':
                    # And previous item is <style>.  Process a stylesheet.
                    temp = urlextract(item.text, siteurl, 'text/css')
                    # Offset indices and add to ans.
                    for j in range(len(temp)):
                        temp[j].start += item.pos[0]
                        temp[j].end += item.pos[0]
                    ans += temp
                else:
                    # Regular text.  Ignore.
                    pass
            else:
                # Current item is a tag.
                if 'style' in item.attrs:
                    # Process a stylesheet embedded in the 'style' attribute.
                    temp = urlextract(item.attrs['style'], siteurl, 'text/css')
                    # Offset indices and add to ans.
                    for j in range(len(temp)):
                        temp[j].start += item.value_pos['style'][0]
                        temp[j].end += item.value_pos['style'][0]
                    ans += temp

                for (a, b) in _URL_TAGS:
                    if item.name.startswith(a) and b in list(item.attrs.keys()):
                        # Got one URL.
                        url = item.attrs[b]
                        # FIXME: Some HTML tag wants a URL list, look up which
                        # tag and make it a special case.
                        (start, end) = item.value_pos[b]
                        tag_name = a
                        tag_attr = b
                        tag_attrs = item.attrs
                        tag_index = i
                        tag = URLMatch(doc, start, end, siteurl, True, False, \
                                       tag_attr, tag_attrs, tag_index, tag_name)
                        ans.append(tag)
        # End of 'text/html' mimetype case.
    else:
        raise ValueError('unknown MIME type: ' + repr(mimetype))

    # Filter the answer, removing duplicate matches.
    start_end_map = {}
    filtered_ans = []
    for item in ans:
        if (item.start, item.end) not in start_end_map:
            start_end_map[(item.start, item.end)] = None
            filtered_ans.append(item)
    return filtered_ans

def _tuple_replace(s, Lindices, Lreplace):
    """
    Replace slices of a string with new substrings.

    Given a list of slice tuples in C{Lindices}, replace each slice
    in C{s} with the corresponding replacement substring from
    C{Lreplace}.

    Example:

     >>> _tuple_replace('0123456789',[(4,5),(6,9)],['abc', 'def'])
     '0123abc5def9'
    """
    ans = []
    Lindices = Lindices[:]
    Lindices.sort()
    if len(Lindices) != len(Lreplace):
        raise ValueError('lists differ in length')
    for i in range(len(Lindices) - 1):
        if Lindices[i][1] > Lindices[i + 1][0]:
            raise ValueError('tuples overlap')
        if Lindices[i][1] < Lindices[i][0]:
            raise ValueError('invalid tuple')
        if min(Lindices[i][0], Lindices[i][1]) < 0 or                    \
           max(Lindices[i][0], Lindices[i][1]) >= len(s):
            raise ValueError('bad index')

    j = 0
    offset = 0
    for i in range(len(Lindices)):

        len1 = Lindices[i][1] - Lindices[i][0]
        len2 = len(Lreplace[i])

        ans.append(s[j:Lindices[i][0] + offset])
        ans.append(Lreplace[i])

        j = Lindices[i][1]
    ans.append(s[j:])
    return ''.join(ans)

def _test_tuple_replace():
    """
    Unit test for L{_tuple_replace}.
    """
    assert _tuple_replace('', [], []) == ''
    assert _tuple_replace('0123456789', [], []) == '0123456789'
    assert _tuple_replace('0123456789', [(4, 5), (6, 9)], ['abc', 'def']) == \
           '0123abc5def9'
    assert _tuple_replace('01234567890123456789', \
           [(1, 9), (13, 14), (16, 18)], ['abcd', 'efg', 'hijk']) == \
           '0abcd9012efg45hijk89'

def urljoin(s, L):
    """
    Write back document with modified URLs (reverses L{urlextract}).

    Given a list C{L} of L{URLMatch} objects obtained from
    L{urlextract}, substitutes changed URLs into the original
    document C{s}, and returns the modified document.

    One should only modify the C{.url} attribute of the L{URLMatch}
    objects.  The ordering of the URLs in the list is not important.

     >>> doc = '<img src="a.png"><a href="b.png">'
     >>> L = urlextract(doc)
     >>> L[0].url = 'foo'
     >>> L[1].url = 'bar'
     >>> urljoin(doc, L)
     '<img src="foo"><a href="bar">'

    """
    return _tuple_replace(s, [(x.start, x.end) for x in L], \
                             [x.url for x in L])


def examples():
    """
    Examples of the C{htmldata} module.

    Example 1:
    Print all absolutized URLs from Google.

    Here we use L{urlextract} to obtain all URLs in the document.

     >>> import urllib2, htmldata
     >>> url = 'http://www.google.com/'
     >>> contents = urllib2.urlopen(url).read()
     >>> for u in htmldata.urlextract(contents, url):
     ...   print u.url
     ...
     http://www.google.com/images/logo.gif
     http://www.google.com/search
     (More output)

    Note that the second argument to L{urlextract} causes the
    URLs to be made absolute with respect to that base URL.

    Example 2:
    Print all image URLs from Google in relative form.


     >>> import urllib2, htmldata
     >>> url = 'http://www.google.com/'
     >>> contents = urllib2.urlopen(url).read()
     >>> for u in htmldata.urlextract(contents):
     ...   if u.tag_name == 'img':
     ...     print u.url
     ...
     /images/logo.gif

    Equivalently, one can use L{tagextract}, and look for occurrences
    of C{<img>} tags. The L{urlextract} function is mostly a convenience
    function for when one wants to extract and/or modify all URLs in a
    document.

    Example 3:
    Replace all C{<a href>} links on Google with the Microsoft web page.

    Here we use L{tagextract} to turn the HTML into a data structure,
    and then loop over the in-order list of tags (items which are not
    tuples are plain text, which is ignored).

     >>> import urllib2, htmldata
     >>> url = 'http://www.google.com/'
     >>> contents = urllib2.urlopen(url).read()
     >>> L = htmldata.tagextract(contents)
     >>> for item in L:
     ...   if isinstance(item, tuple) and item[0] == 'a':
     ...     # It's an HTML <a> tag!  Give it an href=.
     ...     item[1]['href'] = 'http://www.microsoft.com/'
     ...
     >>> htmldata.tagjoin(L)
     (Microsoftized version of Google)

    Example 4:
    Make all URLs on an HTML document be absolute.

     >>> import urllib2, htmldata
     >>> url = 'http://www.google.com/'
     >>> contents = urllib2.urlopen(url).read()
     >>> htmldata.urljoin(htmldata.urlextract(contents, url))
     (Google HTML page with absolute URLs)

    Example 5:
    Properly quote all HTML tag values for pedants.

     >>> import urllib2, htmldata
     >>> url = 'http://www.google.com/'
     >>> contents = urllib2.urlopen(url).read()
     >>> htmldata.tagjoin(htmldata.tagextract(contents))
     (Properly quoted version of the original HTML)

    Example 6:
    Modify all URLs in a document so that they are appended
    to our proxy CGI script C{http://mysite.com/proxy.cgi}.

     >>> import urllib2, htmldata
     >>> url = 'http://www.google.com/'
     >>> contents = urllib2.urlopen(url).read()
     >>> proxy_url = 'http://mysite.com/proxy.cgi?url='
     >>> L = htmldata.urlextract(contents)
     >>> for u in L:
     ...   u.url = proxy_url + u.url
     ...
     >>> htmldata.urljoin(L)
     (Document with all URLs wrapped in our proxy script)

    Example 7:
    Download all images from a website.

     >>> import urllib, htmldata, time
     >>> url = 'http://www.google.com/'
     >>> contents = urllib.urlopen(url).read()
     >>> for u in htmldata.urlextract(contents, url):
     ...   if u.tag_name == 'img':
     ...     filename = urllib.quote_plus(u.url)
     ...     urllib.urlretrieve(u.url, filename)
     ...     time.sleep(0.5)
     ...
     (Images are downloaded to the current directory)

    Many sites will protect against bandwidth-draining robots by
    checking the HTTP C{Referer} [sic] and C{User-Agent} fields.
    To circumvent this, one can create a C{urllib2.Request} object
    with a legitimate C{Referer} and a C{User-Agent} such as
    C{"Mozilla/4.0 (compatible; MSIE 5.5)"}.  Then use
    C{urllib2.urlopen} to download the content.  Be warned that some
    website operators will respond to rapid robot requests by banning
    the offending IP address.

    """
    print(examples.__doc__)

class URLMatch:
    """
    A matched URL inside an HTML document or stylesheet.

    A list of C{URLMatch} objects is returned by L{urlextract}.

    @ivar url:       URL extracted.
    @ivar start:     Starting character index.
    @ivar end:       End character index.
    @ivar in_html:   C{True} if URL occurs within an HTML tag.
    @ivar in_css:    C{True} if URL occurs within a stylesheet.
    @ivar tag_attr:  Specific tag attribute in which URL occurs.

                     Example: C{'href'}.
                     C{None} if the URL does not occur within an HTML
                     tag.
    @ivar tag_attrs: Dictionary of all tag attributes and values.

                     Example: C{{'src':'http://X','alt':'Img'}}.
                     C{None} if the URL does not occur within an HTML
                     tag.
    @ivar tag_index: Index of the tag in the list that would be
                     generated by a call to L{tagextract}.
    @ivar tag_name:  HTML tag name in which URL occurs.

                     Example: C{'img'}.
                     C{None} if the URL does not occur within an HTML
                     tag.

    """

    def __init__(self, doc, start, end, siteurl, in_html, in_css,
                 tag_attr=None, tag_attrs=None, tag_index=None,
                 tag_name=None):
        """
        Create a URLMatch object.
        """
        self.doc = doc
        self.start = start
        self.end = end
        self.url = doc[start:end]
        self.in_html = in_html
        self.in_css = in_css

        if siteurl != None:
            self.url = urllib.parse.urljoin(siteurl, self.url)

        self.tag_attr = tag_attr
        self.tag_attrs = tag_attrs
        self.tag_index = tag_index
        self.tag_name = tag_name


def _cast_to_str(arg, str_class):
    """
    Casts string components of several data structures to str_class.

    Casts string, list of strings, or list of tuples (as returned by
    L{tagextract}) such that all strings are made to type str_class.
    """
    if _is_str(arg):
        return str_class(arg)
    elif isinstance(arg, list):
        ans = []
        for item in arg:
            if _is_str(item):
                ans.append(str_class(item))
            elif isinstance(item, tuple) and len(item) == 2:
                (a, b) = item
                b_prime = {}
                for (b_key, b_value) in list(b.items()):
                    if b_value is None:
                        b_prime[str_class(b_key)] = None
                    else:
                        b_prime[str_class(b_key)] = str_class(b_value)
                ans.append((str_class(a), b_prime))
            else:
                raise ValueError('unknown argument type')
        return ans
    else:
        raise ValueError('unknown argument type')


# -------------------------------------------------------------------
# Unit Tests: HTML <-> Data structure
# -------------------------------------------------------------------

def _test_tagextract(str_class=str):
    """
    Unit tests for L{tagextract} and L{tagjoin}.

    Strings are cast to the string class argument str_class.
    """

    # Work around lack of nested scopes in Python <= 2.1.
    def f(obj, str_class2=str_class):
        return _cast_to_str(obj, str_class2)

    # Simple HTML document to test.
    doc1 = f('\n\n<Html><BODY bgcolor=#ffffff>Hi<h1>Ho</h1><br>' +
             '<br /><img SRc="text%5f.gif"><TAG NOshow>' +
             '<img test="5%ff" /></body></html>\nBye!\n')
    doc2 = f('\r<HTML><!-- Comment<a href="blah"> --><hiYa><foo>' +
           '<test tag="5" content=6><is broken=False><yay>' +
           '<style><><>><</style><foo bar=5>end<!-- <!-- nested --> ' +
           '<script language="JavaScript"><>!><!_!_!-->!_-></script>')
    doc3 = f('\r\t< html >< tag> <!--comment--> <tag a = 5> ' +
           '<foo \r\nbg = val text \t= "hi you" name\t e="5"\t\t\t\n>')
    doc4 = f('<?xml ??><foo><!-- <img> --><!DOCTYPE blah""/>' +
           '<![CDATA[ more and weirder<bar> ] ][]]><![C[DATA[[>' +
           '<abc key=value><![CDATA[to eof')
    doc5 = f('<a href="foobar/ \t="base="10" x="15"><a x="9"t="20">')

    # -----------------------------------------------------------------
    # Test _html_split()
    # -----------------------------------------------------------------

    s = doc1
    assert s == f('').join(_html_split(s))
    assert _html_split(s) == f(
      ['\n\n', '<Html>', '<BODY bgcolor=#ffffff>', 'Hi', '<h1>', 'Ho',
       '</h1>', '<br>', '<br />', '<img SRc="text%5f.gif">',
       '<TAG NOshow>', '<img test="5%ff" />', '</body>', '</html>',
       '\nBye!\n'])

    s = doc2
    assert s == f('').join(_html_split(s))

    # Test single quotes
    s = doc2.replace(f('"'), f("'"))
    assert s == f('').join(_html_split(s))

    s = f('<!-- test weird comment <body> <html> --> <h1>Header' +
          '</h1 value=10 a=11>')
    assert s == f('').join(_html_split(s))
    assert _html_split(s) == f(
    ['<!-- test weird comment <body> <html> -->', ' ',
     '<h1>', 'Header', '</h1 value=10 a=11>'])

    s = f('<!-- <!-- nested messed up --> blah ok <now> what<style>hi' +
          '<><>></style><script language="Java"><aL><>><>></script>a')
    assert s == f('').join(_html_split(s))
    assert _html_split(s) == f(
    ['<!-- <!-- nested messed up -->', ' blah ok ', '<now>',
     ' what', '<style>', 'hi<><>>', '</style>',
     '<script language="Java">', '<aL><>><>>', '</script>', 'a'])

    s = f('<!-- ><# -->!<!-!._-><!-- aa--> <style><tag//</style> <tag ' +
          '<tag <! <! -> <!-- </who< <who> tag> <huh-->-</style>' +
          '</style<style>')
    assert s == f('').join(_html_split(s))
    assert _html_split(s) == f(
    ['<!-- ><# -->', '!', '<!-!._->', '<!-- aa-->',
     ' ', '<style>', '<tag//', '</style>', ' ', '<tag <tag <! <! ->',
     ' ', '<!-- </who< <who> tag> <huh-->', '-', '</style>',
     '</style<style>'])

    s = doc4
    assert s == f('').join(_html_split(s))
    assert _html_split(s) == f(
    ['<?xml ??>', '<foo>', '<!-- <img> -->', '<!DOCTYPE blah""/>',
     '<![CDATA[ more and weirder<bar> ] ][]]>', '<![C[DATA[[>',
     '<abc key=value>', '<![CDATA[to eof'])

    # -----------------------------------------------------------------
    # Test tagextract() and tagjoin()
    # -----------------------------------------------------------------

    # Test for whitespace handling in tags.
    assert (tagextract('<a\n\t\t\t\v\rhref="a.png"\tsize=10>') ==
            [('a', {'href': 'a.png', 'size': '10'})])

    s = doc1
    s2 = doc1.replace(f('"'), f("'"))   # Test single quotes, too.
    assert tagextract(f('')) == []
    assert tagextract(s) == tagextract(s2) == \
           f(['\n\n', ('html', {}), ('body', {'bgcolor': '#ffffff'}),
              'Hi', ('h1', {}), 'Ho', ('/h1', {}), ('br', {}),
              ('br/', {}), ('img', {'src': 'text%5f.gif'}),
              ('tag', {'noshow': None}), ('img/', {'test': '5%ff'}),
              ('/body', {}), ('/html', {}), '\nBye!\n'])
    s2 = f('\n\n<html><body bgcolor="#ffffff">Hi<h1>Ho</h1><br>' +
           '<br /><img src="text%5f.gif"><tag noshow>' +
           '<img test="5%ff" /></body></html>\nBye!\n')
    assert tagjoin(tagextract(s)) == s2


    doc2old = doc2
    doc2 = f('\r<HTML><!-- Comment<a href="blah"> --><hiYa><foo>' +
             '<test tag="5" content=6><is broken=False><yay>' +
             '<style><><>><</style><foo bar=5>end<!-- <!-- nested --> ' +
             '<script language="JavaScript"><>!><!_!_!-->!_-></script>')
    assert doc2old == doc2

    s = doc2
    assert tagextract(s) == f(
    ['\r', ('html', {}), ('!-- Comment<a href="blah"> --', {}),
    ('hiya', {}), ('foo', {}),
    ('test', {'content': '6', 'tag': '5'}),
    ('is', {'broken': 'False'}), ('yay', {}), ('style', {}), '<><>><',
    ('/style', {}), ('foo', {'bar': '5'}), 'end',
    ('!-- <!-- nested --', {}), ' ',
    ('script', {'language': 'JavaScript'}), ('>!><!_!_!-->!_-', {}),
    ('/script', {})])

    assert tagjoin(tagextract(s)) == f(
    '\r<html><!-- Comment<a href="blah"> --><hiya><foo><test ' +
    'content="6" tag="5"><is broken="False"><yay><style><><>><' +
    '</style><foo bar="5">end<!-- <!-- nested --> ' +
    '<script language="JavaScript"><>!><!_!_!-->!_-></script>')

    s = doc5
    assert tagextract(s) == f(
           [('a', {'href':'foobar/ \t=', 'base':'10', 'x':'15'}),
            ('a', {'x':'9', 't':'20'})])
    assert tagjoin(tagextract(s)) == f(
           '<a base="10" href="foobar/ \t=" x="15"><a t="20" x="9">')


    # -----------------------------------------------------------------
    # Test _full_tag_extract()
    # -----------------------------------------------------------------

    for s in [doc1, doc2, doc3,
              doc1.replace(f('"'), f("'")), doc2.replace(f('"'), f("'")),
              doc3.replace(f('"'), f("'"))]:
        L = _full_tag_extract(s)
        for (i, item) in _enumerate(L):
            if isinstance(item, _HTMLTag):
                for key in list(item.attrs.keys()):
                    assert s[item.key_pos[key][0]:item.key_pos[key][1]].lower()\
                           == key
                    if item.attrs[key] != None:
                        assert s[item.value_pos[key][0]:item.value_pos[key][1]]  \
                               == item.attrs[key]

    n = 1000
    doc4 = f('<tag name = "5" value ="6afdjherknc4 cdk j" a="7" b=8/>')
    doc4 *= n
    L = tagextract(doc4)
    assert len(L) == n
    for i in range(n):
        assert L[i] == f([('tag/', {'name':'5', 'value':'6afdjherknc4 cdk j',
                               'a':'7', 'b':'8'})])[0]

    # -----------------------------------------------------------------
    # Test tagextract() and tagjoin() with XML directives.
    # -----------------------------------------------------------------

    doc1 = f(
    'a<?xml version="1.0"?>' +
    'b<!DOCTYPE html' +
    'PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"' +
    '"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd" >c' +
    '<html a=b><!-- Comment <><> hi! -->' +
    'z<![CDATA[ some content  ]]>rx' +
    '<![C[DATA[ more and weirder ] ][]]>tt')

    doc1join = f(
    'a<?xml version="1.0"?>b<!DOCTYPE htmlPUBLIC "-//W3C//DTD ' +
    'XHTML 1.0 Transitional//EN""http://www.w3.org/TR/xhtml1/DTD/' +
    'xhtml1-transitional.dtd">c<html a="b"><!-- Comment <><> hi! ' +
    '-->z<![CDATA[ some content  ]]>rx<![C[DATA[ more and weirder ]' +
    ' ][]]>tt')

    ans1 = f(
    ['a', ('?xml version="1.0"?', {}), 'b',
     ('!DOCTYPE html' +
      'PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"' +
      '"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd"', {}),
      'c', ('html', {'a':'b'}), ('!-- Comment <><> hi! --', {}), 'z',
      ('![CDATA[ some content  ]]', {}), 'rx',
      ('![C[DATA[ more and weirder ] ][]]', {}), 'tt'])

    assert (tagextract(f('<?xml version="1.0" encoding="utf-8" ?>')) ==
            f([('?xml version="1.0" encoding="utf-8" ?', {})]))
    assert (tagextract(f('<!DOCTYPE html PUBLIC etc...>')) ==
            f([('!DOCTYPE html PUBLIC etc...', {})]))

    assert tagextract(doc1) == ans1

    assert tagjoin(tagextract(doc1)) == doc1join


# -------------------------------------------------------------------
# Unit Tests: URL Parsing
# -------------------------------------------------------------------

def _test_urlextract(str_class=str):
    """
    Unit tests for L{urlextract} and L{urljoin}.

    Strings are cast to the string class argument str_class.
    """

    # Work around lack of nested scopes in Python <= 2.1.
    def f(obj, str_class2=str_class):
        return _cast_to_str(obj, str_class2)

    doc1 = f('urlblah, url ( blah2, url( blah3) url(blah4) ' +
           'url("blah5") hum("blah6") url)"blah7"( url ( " blah8 " );;')
    doc2 = f('<html><img src="a.gif" alt="b"><a href = b.html name=' +
        '"c"><td background =  ./c.png width=100%><a value=/f.jpg>' +
        '<img src="http://www.abc.edu/d.tga">http://www.ignore.us/' +
        '\nhttp://www.nowhere.com <style>url(h.gif) ' +
        'url(http://www.testdomain.com/) http://ignore.com/a' +
        '</style><img alt="c" src = "a.gif"><img src=/i.png>')
    doc3 = f('@import foo;\n@import bar\n@import url(\'foo2\');' +
           '@import url(\'http://bar2\')\n@import\turl("foo!");' +
           '@import \'foo3\'\n@import "bar3";\n@importfails;' +
           '@import;@import\n;url(\'howdy!\')\n@import  foo5 ;' +
           '@import  \'foo6\' \n@import  "foo7";')
    doc4 = f('@import foo handheld;\n@import \'bar\' handheld\n' +
             '@import url(\'foo2\') handheld; @import url(bar2) ha\n' +
             '@import url("foo3") handheld\n')
    doc5 = f('<html><img src="a.gif" alt="b" style="url(\'foo\')">' +
             '<a href = b.html name="c" style="@import \'bar.css\'">')
    doc6 = doc2.replace(f('"'), f("'"))   # Test single quotes, too.

    # Test CSS.
    s = doc1
    L = urlextract(s, mimetype='text/css')
    L2 = [x.url for x in L]
    assert L2 == f([' blah3', 'blah4', 'blah5', ' blah8 '])
    assert [s[x.start:x.end] == x.url for x in L].count(False) == 0

    # Test CSS more.
    s = doc3
    L = urlextract(s, mimetype='text/css')
    L2 = [x.url for x in L]
    assert L2 == f(['foo', 'bar', 'foo2', 'http://bar2', 'foo!',
                    'foo3', 'bar3', 'howdy!', 'foo5', 'foo6', 'foo7'])
    assert [s[x.start:x.end] == x.url for x in L].count(False) == 0

    # Test CSS even more.
    s = doc4
    L = urlextract(s, mimetype='text/css')
    L2 = [x.url for x in L]
    assert L2 == f(['foo', 'bar', 'foo2', 'bar2', 'foo3'])
    assert [s[x.start:x.end] == x.url for x in L].count(False) == 0

    # Test HTML.
    s = doc2
    L = urlextract(s)
    L2 = [x.url for x in L]
    L3 = [x.url for x in urlextract(doc6)]
    ans = f(['a.gif', 'b.html', './c.png',
             'http://www.abc.edu/d.tga', 'h.gif',
             'http://www.testdomain.com/', 'a.gif', '/i.png'])
    assert L2 == L3 == ans

    for i in range(len(L)):
        assert s[L[i].start:L[i].end] == L[i].url

    # Test HTML more.
    n = 100
    s2 = s * n

    L3 = urlextract(s2)
    L4 = [x.url for x in L3]
    assert L4 == L2 * n
    for i in range(len(L3)):
        assert s2[L3[i].start:L3[i].end] == L3[i].url

    # Test HTML w/ siteurl.
    base = f('http://www.python.org/~guido/')
    L = urlextract(s, base)
    L2 = [x.url for x in L]
    assert L2 == [urllib.parse.urljoin(base, x) for x in ans]

    # Test urljoin().
    assert urljoin(doc1, urlextract(doc1, mimetype='text/css')) == doc1
    assert urljoin(doc2, urlextract(doc2)) == doc2

    s = doc2
    L = urlextract(s)
    L[3].url = f('FOO')
    L[5].url = f('BAR')
    L[7].url = f('F00!')
    assert urljoin(s, L) == f(
    '<html><img src="a.gif" alt="b"><a href = b.html name="c">' +
    '<td background =  ./c.png width=100%><a value=/f.jpg>' +
    '<img src="FOO">http://www.ignore.us/\nhttp://www.nowhere.com ' +
    '<style>url(h.gif) url(BAR) http://ignore.com/a</style>' +
    '<img alt="c" src = "a.gif"><img src=F00!>')

    # Test HTML yet more.
    s = doc5
    L = urlextract(s)
    L2 = [x.url for x in L]
    assert L2 == f(['foo', 'a.gif', 'bar.css', 'b.html'])
    assert [s[x.start:x.end] == x.url for x in L].count(False) == 0

# -------------------------------------------------------------------
# Unit Test Main Routine
# -------------------------------------------------------------------

def _test():
    """
    Unit test main routine.
    """
    print('Unit tests:')
    _test_remove_comments()
    print('  _remove_comments:       OK')
    _test_shlex_split()
    print('  _shlex_split:           OK')
    _test_tag_dict()
    print('  _tag_dict:              OK')
    _test_tuple_replace()
    print('  _tuple_replace:         OK')

    _test_tagextract()
    print('  tagextract*:            OK')

    _test_tagextract(str)
    print('  tagextract (unicode)*:  OK')

    _test_urlextract()
    print('  urlextract*:            OK')

    _test_urlextract(str)
    print('  urlextract (unicode)*:  OK')

    print()
    print('* The corresponding join method has been tested as well.')


if __name__ == '__main__':
    _test()
