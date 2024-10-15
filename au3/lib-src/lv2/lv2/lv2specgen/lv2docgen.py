#!/usr/bin/env python
# -*- coding: utf-8 -*-
#
# lv2docgen, a documentation generator for LV2 plugins
# Copyright 2012 David Robillard <d@drobilla.net>
#
# Permission to use, copy, modify, and/or distribute this software for any
# purpose with or without fee is hereby granted, provided that the above
# copyright notice and this permission notice appear in all copies.
#
# THIS SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
# WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
# MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
# ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
# WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
# ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
# OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

import errno
import os
import sys

__date__    = '2012-03-27'
__version__ = '0.0.0'
__authors__ = 'David Robillard'
__license__ = 'ISC License <http://www.opensource.org/licenses/isc>'
__contact__ = 'devel@lists.lv2plug.in'

try:
    import rdflib
except ImportError:
    sys.exit('Error importing rdflib')

doap = rdflib.Namespace('http://usefulinc.com/ns/doap#')
lv2  = rdflib.Namespace('http://lv2plug.in/ns/lv2core#')
rdf  = rdflib.Namespace('http://www.w3.org/1999/02/22-rdf-syntax-ns#')
rdfs = rdflib.Namespace('http://www.w3.org/2000/01/rdf-schema#')

def uri_to_path(uri):
    path = uri[uri.find(':'):]
    while not path[0].isalpha():
        path = path[1:]
    return path

def get_doc(model, subject):
    comment = model.value(subject, rdfs.comment, None)
    if comment:
        return '<p class="content">%s</p>' % comment
    return ''

def port_doc(model, port):
    name = model.value(port, lv2.name, None)
    comment = model.value(port, rdfs.comment, None)
    html = '<div class="specterm"><h3>%s</h3>' % name
    html += get_doc(model, port)
    html += '</div>'
    return html

def plugin_doc(model, plugin, style_uri):
    uri  = str(plugin)
    name = model.value(plugin, doap.name, None)

    html = '''<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML+RDFa 1.0//EN" "http://www.w3.org/MarkUp/DTD/xhtml-rdfa-1.dtd">
<html about="%s"
      xmlns="http://www.w3.org/1999/xhtml"
      xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
      xmlns:rdfs="http://www.w3.org/2000/01/rdf-schema#"
      xmlns:lv2="http://lv2plug.in/ns/lv2core#"
      xml:lang="en">''' % uri

    html += '''<head>
    <title>%s</title>
    <meta http-equiv="content-type" content="text/xhtml+xml; charset=utf-8" />
    <meta name="generator" content="lv2docgen" />
    <link href="%s" rel="stylesheet" type="text/css" />
  </head>
  <body>''' % (name, style_uri)

    html += '''
  <!-- HEADER -->
  <div id="header">
    <h1 id="title">%s</h1>
    <table id="meta">
      <tr><th>URI</th><td><a href="%s">%s</a></td></tr>
      <tr><th>Version</th><td>%s</td></tr>
    </table>
  </div>
''' % (name, uri, uri, '0.0.0')

    html += get_doc(model, plugin)

    ports_html = ''
    for p in model.triples([plugin, lv2.port, None]):
        ports_html += port_doc(model, p[2])

    if len(ports_html):
        html += '''
  <h2 class="sec">Ports</h2>
  <div class="content">
%s
  </div>''' % ports_html

    html += '  </body></html>'
    return html

if __name__ == '__main__':
    'LV2 plugin documentation generator'

    if len(sys.argv) < 2:
        print('Usage: %s OUTDIR FILE...' % sys.argv[0])
        sys.exit(1)

    outdir = sys.argv[1]
    files = sys.argv[2:]
    model = rdflib.ConjunctiveGraph()
    for f in files:
        model.parse(f, format='n3')

    style_uri = os.path.abspath(os.path.join(outdir, 'style.css'))
    for p in model.triples([None, rdf.type, lv2.Plugin]):
        plugin = p[0]
        html = plugin_doc(model, plugin, style_uri)
        path = uri_to_path(plugin)

        outpath = os.path.join(outdir, path + '.html')
        try:
            os.makedirs(os.path.dirname(outpath))
        except OSError:
            e = sys.exc_info()[1]
            if e.errno == errno.EEXIST:
                pass
            else:
                raise

            print('Writing <%s> documentation to %s' % (plugin, outpath))
            out = open(outpath, 'w')
            out.write(html)
            out.close()
