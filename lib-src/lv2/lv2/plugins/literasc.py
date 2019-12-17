#!/usr/bin/env python
# -*- coding: utf-8 -*-
#
# Literasc, a simple literate programming tool for C, C++, and Turtle.
# Copyright 2012 David Robillard <d@drobilla.net>
#
# Unlike many LP tools, this tool uses normal source code as input, there is no
# tangle/weave and no special file format.  The literate parts of the program
# are written in comments, which are emitted as paragraphs of regular text
# interleaved with code.  Asciidoc is both the comment and output syntax.

import os
import re
import sys

def format_text(text):
    'Format a text (comment) fragment and return it as a marked up string'
    return '\n\n' + re.sub('\n *', '\n', text.strip()) + '\n\n'

def format_code(lang, code):
    if code.strip() == '':
        return code

    head = '[source,%s]' % lang
    sep  = '-' * len(head) + '\n'
    return head + '\n' + sep + code.strip('\n') + '\n' + sep

def format_c_source(filename, file):
    output           = '=== %s ===\n' % os.path.basename(filename)
    chunk            = ''
    prev_c           = 0
    in_comment       = False
    in_comment_start = False
    n_stars          = 0
    code             = ''
    for line in file:
        code += line

    # Skip initial license comment
    if code[0:2] == '/*':
        code = code[code.find('*/') + 2:]

    for c in code:
        if prev_c == '/' and c == '*':
            in_comment_start = True
            n_stars = 1
        elif in_comment_start:
            if c == '*':
                n_stars += 1
            else:
                if n_stars > 1:
                    output += format_code('c', chunk[0:len(chunk) - 1])
                    chunk = ''
                    in_comment = True
                else:
                    chunk += '*' + c
                in_comment_start = False
        elif in_comment and prev_c == '*' and c == '/':
            if n_stars > 1:
                output += format_text(chunk[0:len(chunk) - 1])
            else:
                output += format_code('c', '/* ' + chunk[0:len(chunk) - 1] + '*/')
            in_comment = False
            in_comment_start = False
            chunk = ''
        elif in_comment_start and c == '*':
            n_stars += 1
        else:
            chunk += c
        prev_c = c

    return output + format_code('c', chunk)

def format_ttl_source(filename, file):
    output           = '=== %s ===\n' % os.path.basename(filename)

    in_comment = False
    chunk      = ''
    for line in file:
        is_comment = line.strip().startswith('#')
        if in_comment:
            if is_comment:
                chunk += line.strip().lstrip('# ') + ' \n'
            else:
                output += format_text(chunk)
                in_comment = False
                chunk = line
        else:
            if is_comment:
                output += format_code('n3', chunk)
                in_comment = True
                chunk = line.strip().lstrip('# ') + ' \n'
            else:
                chunk += line

    if in_comment:
        return output + format_text(chunk)
    else:
        return output + format_code('n3', chunk)

def gen(out, filenames):
    for filename in filenames:
        file = open(filename)
        if not file:
            sys.stderr.write('Failed to open file %s\n' % filename)
            continue

        if filename.endswith('.c') or filename.endswith('.h'):
            out.write(format_c_source(filename, file))
        elif filename.endswith('.ttl') or filename.endswith('.ttl.in'):
            out.write(format_ttl_source(filename, file))
        elif filename.endswith('.txt'):
            for line in file:
                out.write(line)
            out.write('\n')
        else:
            sys.stderr.write("Unknown source format `%s'" % (
                filename[filename.find('.'):]))

        file.close()

if __name__ == "__main__":
    if len(sys.argv) < 2:
        sys.stderr.write('Usage: %s FILENAME...\n' % sys.argv[1])
        sys.exit(1)

    gen(sys.argv[1:])
