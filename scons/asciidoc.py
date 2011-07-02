
# asciidoc tool

# Builder: Html
#   Builds an html document from a given asciidoc .txt file.
#   Figures out include dependencies automatically.
#
# Builder: Man
#   Builds a man page from a given asciidoc .n.txt file.
#   Figures out include dependencies automatically.
#
# Influential Variables:
#   ASCIIDOCFLAGS - additional flags passed to asciidoc
#
# Requirements:
#   asciidoc, a2x programs
#   python
#   xmllint (from libxml2)
#   docbook 4.5 (may need to set XML_CATALOG_FILES appropriately)
#   docbook-xsl (may need to set XML_CATALOG_FILES appropriately)
#   xsltproc (from libxslt)

from SCons.Builder import Builder
from SCons.Scanner import Scanner, FindPathDirs
import SCons.Node.FS

import re
import os.path

include_re = re.compile(r"include::?([A-Za-z0-9._/]+)\[.*\]")

def asciidoc_scan(node, env, path):
    contents = node.get_text_contents()
    includes = include_re.findall(contents)
    return includes

def exists(env):
    # This might be right. You'll know if it isn't I suspect.
    return True

def generate(env):
    asciidocscan = Scanner(function = asciidoc_scan, skeys=['.txt'],
            recursive=True)

    htmlbld = Builder(action = "asciidoc -o $TARGET ${ASCIIDOCFLAGS} $SOURCE",
            suffix = ".html", src_suffix = ".txt")

    manbld = Builder(
            action = "a2x -v -f manpage -D ${TARGET.dir} ${ASCIIDOCFLAGS} $SOURCE",
            src_suffix = ".txt")

    env.Append(SCANNERS = asciidocscan)
    env.Append(BUILDERS = {'Html' : htmlbld})
    env.Append(BUILDERS = {'Man' : manbld})

