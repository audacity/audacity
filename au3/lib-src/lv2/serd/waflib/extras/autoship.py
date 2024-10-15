#!/usr/bin/env python

import sys
import os


def report(msg):
    sys.stderr.write(msg + "\n")


def warn(msg):
    sys.stderr.write("warning: %s\n" % msg)


def error_exit(msg):
    sys.stderr.write("error: %s\n" % msg)
    sys.exit(1)


def ensure(condition, message):
    if not condition:
        error_exit(message)


def get_project_info(top=None):
    """Load wscript to get project information (name, version, and so on)"""

    import importlib

    project_dir = top or os.getcwd()
    wscript_path = os.path.join(project_dir, "wscript")
    sys.path.insert(0, os.path.dirname(wscript_path))

    loader = importlib.machinery.SourceFileLoader("wscript", wscript_path)
    spec = importlib.util.spec_from_loader("wscript", loader)
    wscript = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(wscript)

    return {
        "name": wscript.APPNAME,
        "version": wscript.VERSION,
        "uri": getattr(wscript, "uri", None),
        "title": getattr(wscript, "title", wscript.APPNAME.title()),
        "dist_pattern": wscript.dist_pattern,
        "post_tags": wscript.post_tags,
    }


def parse_version(revision):
    """Convert semver string `revision` to a tuple of integers"""
    return tuple(map(int, revision.split(".")))


def is_release_version(version):
    """Return true if `version` is a stable version number"""
    if isinstance(version, tuple):
        return version[len(version) - 1] % 2 == 0

    return is_release_version(parse_version(version))


def get_blurb(in_file):
    """Get the first paragraph of a Markdown file"""
    with open(in_file, "r") as f:
        f.readline()  # Title
        f.readline()  # Title underline
        f.readline()  # Blank

        out = ""
        line = f.readline()
        while len(line) > 0 and line != "\n":
            out += line.replace("\n", " ")
            line = f.readline()

        return out.strip()


def get_items_markdown(items, indent=""):
    """Return a list of NEWS entries as a Markdown list"""
    return "".join([indent + "* %s\n" % item for item in items])


def get_release_json(entry):
    """Return a release description in Gitlab JSON format"""
    import json

    version = entry["revision"]
    desc = {
        "name": "Serd %s" % version,
        "tag_name": "v%s" % version,
        "description": get_items_markdown(entry["items"]),
        "released_at": entry["date"].isoformat(),
    }

    return json.dumps(desc)


def read_text_news(in_file, preserve_timezones=False, dist_pattern=None):
    """Read NEWS entries"""

    import datetime
    import email.utils
    import re

    entries = {}
    with open(in_file, "r") as f:
        while True:
            # Read header line
            head = f.readline()
            matches = re.match(r"([^ ]*) \((.*)\) ([a-zA-z]*)", head)
            if matches is None:
                break

            e = {
                "name": matches.group(1),
                "revision": matches.group(2),
                "status": matches.group(3),
                "items": [],
            }

            semver = parse_version(e["revision"])
            if is_release_version(semver) and dist_pattern is not None:
                e["dist"] = dist_pattern % semver

            # Read blank line after header
            if f.readline() != "\n":
                raise SyntaxError("expected blank line after NEWS header")

            def add_item(item):
                if len(item) > 0:
                    e["items"] += [item.replace("\n", " ").strip()]

            # Read entries for this revision
            item = ""
            line = f.readline()
            while line:
                if line.startswith("  * "):
                    add_item(item)
                    item = line[3:].lstrip()
                elif line == "\n":
                    add_item(item)
                    break
                else:
                    item += line.lstrip()

                line = f.readline()

            matches = re.match(r" -- (.*) <(.*)>  (.*)", f.readline())
            date = email.utils.parsedate_to_datetime(matches.group(3))
            if not preserve_timezones:
                date = date.astimezone(datetime.timezone.utc)

            e.update(
                {
                    "date": date,
                    "blamee_name": matches.group(1),
                    "blamee_mbox": matches.group(2),
                }
            )

            entries[semver] = e

            # Skip trailing blank line before next entry
            space = f.readline()
            if space != "\n" and space != "":
                raise SyntaxError("expected blank line, not '%s'" % space)

    return entries


def write_text_news(entries, news):
    """Write NEWS in standard Debian changelog format"""
    import textwrap

    revisions = sorted(entries.keys(), reverse=True)
    for r in revisions:
        e = entries[r]
        summary = "%s (%s) %s" % (e["name"], e["revision"], e["status"])
        news.write("\n" if r != revisions[0] else "")
        news.write("%s;\n" % summary)

        for item in e["items"]:
            wrapped = textwrap.wrap(item, width=74)
            news.write("\n  * " + "\n    ".join(wrapped))

        email = e["blamee_mbox"].replace("mailto:", "")
        author = "%s <%s>" % (e["blamee_name"], email)
        date = e["date"].strftime("%a, %d %b %Y %H:%M:%S %z")
        news.write("\n\n -- %s  %s\n" % (author, date))


def read_ttl_news(name, in_files, top_entries=None, dist_pattern=None):
    """Read news entries from Turtle"""

    import datetime
    import rdflib

    doap = rdflib.Namespace("http://usefulinc.com/ns/doap#")
    dcs = rdflib.Namespace("http://ontologi.es/doap-changeset#")
    rdfs = rdflib.Namespace("http://www.w3.org/2000/01/rdf-schema#")
    foaf = rdflib.Namespace("http://xmlns.com/foaf/0.1/")
    rdf = rdflib.Namespace("http://www.w3.org/1999/02/22-rdf-syntax-ns#")
    g = rdflib.ConjunctiveGraph()

    # Parse input files
    for i in in_files:
        g.parse(i, format="turtle")

    proj = g.value(None, rdf.type, doap.Project)
    for f in g.triples([proj, rdfs.seeAlso, None]):
        if f[2].endswith(".ttl"):
            g.parse(f[2], format="turtle")

    entries = {}
    for r in g.triples([proj, doap.release, None]):
        release = r[2]
        revision = g.value(release, doap.revision, None)
        date = g.value(release, doap.created, None)
        blamee = g.value(release, dcs.blame, None)
        changeset = g.value(release, dcs.changeset, None)
        dist = g.value(release, doap["file-release"], None)

        semver = parse_version(revision)
        if not dist:
            if dist_pattern is not None:
                dist = dist_pattern % semver
            else:
                warn("No file release for %s %s" % (proj, revision))

        if revision and date and blamee and changeset:
            status = "stable" if is_release_version(revision) else "unstable"
            iso_date = datetime.datetime.strptime(date, "%Y-%m-%dT%H:%M:%S%z")
            e = {
                "name": name,
                "revision": str(revision),
                "date": iso_date,
                "status": status,
                "items": [],
            }

            if dist is not None:
                e["dist"] = dist

            for i in g.triples([changeset, dcs.item, None]):
                item = str(g.value(i[2], rdfs.label, None))
                e["items"] += [item]
                if dist and top_entries is not None:
                    if dist not in top_entries:
                        top_entries[dist] = {"items": []}
                    top_entries[dist]["items"] += ["%s: %s" % (name, item)]

            e["blamee_name"] = str(g.value(blamee, foaf.name, None))
            e["blamee_mbox"] = str(g.value(blamee, foaf.mbox, None))

            entries[semver] = e
        else:
            warn("Ignored incomplete %s release description" % name)

    return entries


def write_ttl_news(entries, out_file, template=None, subject_uri=None):
    """Write NEWS in Turtle format"""
    import rdflib
    import rdflib.namespace
    import rdflib.resource
    import datetime

    # Set up namespaces and make a graph for the output
    doap = rdflib.Namespace("http://usefulinc.com/ns/doap#")
    dcs = rdflib.Namespace("http://ontologi.es/doap-changeset#")
    rdfs = rdflib.Namespace("http://www.w3.org/2000/01/rdf-schema#")
    rdf = rdflib.Namespace("http://www.w3.org/1999/02/22-rdf-syntax-ns#")
    xsd = rdflib.Namespace("http://www.w3.org/2001/XMLSchema#")
    g = rdflib.ConjunctiveGraph()
    ns = rdflib.namespace.NamespaceManager(g)
    ns.bind("doap", doap)
    ns.bind("dcs", dcs)

    # Load given template file
    if template is not None:
        g.load(template, format="turtle")

    if subject_uri is not None:
        # Use given subject uri
        subject = rdflib.URIRef(subject_uri)
        g.add((subject, rdf.type, doap.Project))
    else:
        # Find project URI to use as subject, and optionally the maintainer
        subject = g.value(None, rdf.type, doap.Project)
        ensure(subject is not None, "Unable to find project URI for subject")

    maintainer = g.value(subject, doap.maintainer, None)

    for r, e in entries.items():
        semver = parse_version(e["revision"])
        ver_string = "%03d%03d%03d" % semver

        release = rdflib.BNode("r%s" % ver_string)
        g.add((subject, doap.release, release))
        g.add((release, doap.revision, rdflib.Literal(e["revision"])))

        if "dist" in e:
            g.add((release, doap["file-release"], rdflib.URIRef(e["dist"])))

        utc_date = e["date"].astimezone(datetime.timezone.utc)
        date_str = utc_date.strftime("%Y-%m-%dT%H:%M:%S") + "Z"
        time = rdflib.Literal(date_str, datatype=xsd.dateTime, normalize=False)
        g.add((release, doap.created, time))

        if maintainer is not None:
            g.add((release, dcs.blame, maintainer))

        changeset = rdflib.BNode("c%s" % ver_string)
        g.add((release, dcs.changeset, changeset))
        for index, item in enumerate(e["items"]):
            item_node = rdflib.BNode("i%s%08d" % (ver_string, index))
            g.add((changeset, dcs.item, item_node))
            g.add((item_node, rdfs.label, rdflib.Literal(item)))

    g.serialize(out_file, format="turtle")


def read_news(path=None, format="NEWS", unsorted=False, utc=True, top=None):
    """Read news in either text changelog or Turtle format"""

    if format == "NEWS" and path is None:
        path = os.path.join(top or "", "NEWS")

    top = top or os.path.dirname(path)
    info = get_project_info(top)
    dist_pattern = info.get("dist_pattern", None)

    if format == "NEWS":
        entries = read_text_news(path, not utc, dist_pattern)
    else:
        ensure(path is not None, "Input path must be given for Turtle input")
        entries = read_ttl_news(info["name"], [path])

    if not unsorted:
        for r, e in entries.items():
            e["items"] = list(sorted(e["items"]))

    return entries


def write_news_file(entries, news, format, template, subject):
    """Write news entries to a file object"""
    if format == "NEWS":
        write_text_news(entries, news)
    else:
        write_ttl_news(entries, news, template, subject)


def write_news(entries, news, format="NEWS", template=None, subject=None):
    """Write news entries to a file object or path"""
    if isinstance(news, str):
        with open(news, "w" if format == "NEWS" else "wb") as f:
            write_news_file(entries, f, format, template, subject)
    else:
        write_news_file(entries, news, format, template, subject)


def news_command():
    ap = argparse.ArgumentParser(description="Generate NEWS file")
    ap.add_argument("out_path", help="news output file")
    ap.add_argument("--in-path", help="input file")
    ap.add_argument("--unsorted", action="store_true", help="don't sort items")
    ap.add_argument("--in-format", default="NEWS", choices=["NEWS", "turtle"])
    ap.add_argument("--timezones", action="store_true", help="keep timezones")

    args = ap.parse_args(sys.argv[2:])
    entries = read_news(
        args.in_path, args.in_format, args.unsorted, args.timezones
    )

    with open(args.out_path, "w") as news:
        write_news(entries, news)


def ttl_news_command():
    ap = argparse.ArgumentParser(description="Generate Turtle changeset")
    ap.add_argument("--in-path", help="news input file")
    ap.add_argument("out_path", help="news output file")
    ap.add_argument("--template")
    ap.add_argument("--unsorted", action="store_true", help="don't sort items")
    ap.add_argument("--in-format", default="NEWS", choices=["NEWS", "turtle"])

    args = ap.parse_args(sys.argv[2:])
    info = get_project_info()
    entries = read_news(args.in_path, args.in_format, info["dist_pattern"])

    write_ttl_news(
        entries, args.out_path, template=args.template, subject_uri=info["uri"]
    )


def write_posts(entries, out_dir, meta={}):
    """Write news posts in Pelican Markdown format"""
    import datetime

    report("Writing posts to %s" % out_dir)

    info = get_project_info()
    description = get_blurb("README.md")
    title = info["title"]
    meta["Tags"] = ", ".join(info["post_tags"])
    meta["Author"] = meta.get("Author", os.getenv("USER"))

    try:
        os.mkdir(out_dir)
    except Exception:
        pass

    for r, e in entries.items():
        name = e["name"]
        revision = e["revision"]
        if "dist" not in e:
            warn("No file release for %s %s" % (name, revision))
            continue

        date = e["date"].astimezone(datetime.timezone.utc)
        date_str = date.strftime("%Y-%m-%d")
        datetime_str = date.strftime("%Y-%m-%d %H:%M")
        slug_version = revision.replace(".", "-")
        filename = "%s-%s-%s.md" % (date_str, name, slug_version)

        with open(os.path.join(out_dir, filename), "w") as post:
            slug = "%s-%s" % (name, slug_version)
            post.write("Title: %s %s\n" % (title, revision))
            post.write("Date: %s\n" % datetime_str)
            post.write("Slug: %s\n" % slug)
            for k in sorted(meta.keys()):
                post.write("%s: %s\n" % (k, meta[k]))

            url = e["dist"]
            link = "[%s %s](%s)" % (title, revision, url)
            post.write("\n%s has been released." % link)
            post.write("  " + description + "\n")

            if e["items"] != ["Initial release"]:
                post.write("\nChanges:\n\n")
                post.write(get_items_markdown(e["items"], indent=" "))


def posts_command():
    ap = argparse.ArgumentParser(description="Generate Pelican posts")
    ap.add_argument("out_dir", help="output directory")
    ap.add_argument("--author", help="post author")
    ap.add_argument("--in-path", help="input file")
    ap.add_argument("--in-format", default="NEWS", choices=["NEWS", "turtle"])
    ap.add_argument("--title", help="Title for posts")

    args = ap.parse_args(sys.argv[2:])
    info = get_project_info()
    entries = read_news(args.in_path, args.in_format, info["dist_pattern"])
    meta = {"Author": args.author} if args.author else {}

    write_posts(entries, args.out_dir, meta)


def release(args, posts_dir=None, remote_dist_dir=None, dist_name=None):
    import json
    import os
    import shlex
    import subprocess

    def run_cmd(cmd):
        if args.dry_run:
            print(" ".join([shlex.quote(i) for i in cmd]))

    info = get_project_info()
    name = info["name"]
    title = info["title"]
    version = info["version"]
    semver = parse_version(version)
    dry_run = args.dry_run

    # Check that this is a release version first of all
    ensure(is_release_version(semver), "%s is an unstable version" % version)
    report("Releasing %s %s" % (name, version))

    # Check that NEWS is up to date
    entries = read_news()
    ensure(semver in entries, "%s has no NEWS entries" % version)

    # Check that working copy is up to date
    fetch_cmd = ["git", "fetch", "--dry-run"]
    fetch_status = subprocess.check_output(fetch_cmd).decode("utf-8")
    ensure(len(fetch_status) == 0, "Local copy is out of date")

    # Remove distribution if one was already built
    dist = "%s-%s.tar.bz2" % (dist_name or name.lower(), version)
    sig = dist + ".sig"
    try:
        os.remove(dist)
        os.remove(sig)
    except Exception:
        pass

    # Check that working copy is clean
    branch_cmd = ["git", "rev-parse", "--abbrev-ref", "HEAD"]
    branch = subprocess.check_output(branch_cmd).decode('ascii').strip()
    status_cmd = ["git", "status", "--porcelain", "-b", "--ignore-submodules"]
    status = subprocess.check_output(status_cmd).decode("utf-8")
    sys.stdout.write(status)
    expected_status = "## %s...origin/%s\n" % (branch, branch)
    ensure(status == expected_status, "Working copy is dirty")

    # Fetch project description and ensure it matches
    url = "https://%s/api/v4/projects/%s%%2F%s" % (args.lab, args.group, name)
    desc_cmd = ["curl", "-HPRIVATE-TOKEN: " + args.token, url]
    desc = json.loads(subprocess.check_output(desc_cmd))
    proj_name = desc["name"]
    ensure(proj_name == name, "Project name '%s' != '%s'" % (proj_name, name))

    # Build distribution
    run_cmd(["./waf", "configure", "--docs"])
    run_cmd(["./waf", "build"])
    run_cmd(["./waf", "distcheck"])
    ensure(dry_run or os.path.exists(dist), "%s was not created" % dist)

    # Sign distribution
    run_cmd(["gpg", "-b", dist])
    ensure(dry_run or os.path.exists(sig), "%s.sig was not created" % dist)
    run_cmd(["gpg", "--verify", sig])

    # Tag release
    tag = "v" + version
    run_cmd(["git", "tag", "-s", tag, "-m", "%s %s" % (title, version)])
    run_cmd(["git", "push", "--tags"])

    # Generate posts
    if posts_dir is not None:
        write_posts(entries, posts_dir)

    # Upload distribution and signature
    if remote_dist_dir is not None:
        run_cmd(["scp", dist, os.path.join(remote_dist_dir, dist)])
        run_cmd(["scp", sig, os.path.join(remote_dist_dir, sig)])

    # Post Gitlab release
    post_cmd = [
        "curl",
        "-XPOST",
        "-HContent-Type: application/json",
        "-HPRIVATE-TOKEN: " + args.token,
        "-d" + get_release_json(entries[semver]),
        "https://gitlab.com/api/v4/projects/drobilla%2Fserd/releases",
    ]
    run_cmd(post_cmd)

    report("Released %s %s" % (name, version))
    report("Remember to upload posts and push to other remotes!")

def release_command():
    ap = argparse.ArgumentParser(description="Release project")
    ap.add_argument("group", help="Gitlab user or group for project")
    ap.add_argument("token", help="Gitlab access token")
    ap.add_argument("--lab", default="gitlab.com", help="Gitlab instance")
    ap.add_argument("--dry-run", action="store_true", help="do nothing")
    ap.add_argument("--posts", help="Pelican posts directory")
    ap.add_argument("--scp", help="SSH path to distribution directory")
    args = ap.parse_args(sys.argv[2:])

    release(args, posts_dir=args.posts, remote_dist_dir=args.scp)


if __name__ == "__main__":
    import argparse

    # Get list of command names from handler functions for help text
    global_names = list(globals().keys())
    handlers = [k[0:-8] for k in global_names if k.endswith("_command")]

    # Run simple top level argument parser to get command name
    ap = argparse.ArgumentParser(
        description="Automatic release building",
        epilog="commands: " + " ".join(handlers),
    )
    ap.add_argument("command", help="Subcommand to run")
    args = ap.parse_args(sys.argv[1:2])

    # Check that a handler is defined for the given command
    function_name = args.command + "_command"
    if function_name not in globals():
        sys.stderr.write("error: Unknown command '%s'\n" % args.command)
        ap.print_help()
        sys.exit(1)

    # Dispatch to command handler
    globals()[function_name]()
    sys.exit(0)
