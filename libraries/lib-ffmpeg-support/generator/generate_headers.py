import os
import re
import argparse

parser = argparse.ArgumentParser()

parser.add_argument("--input")
parser.add_argument("--include-dir", nargs="+")
parser.add_argument("--output")

args = parser.parse_args()

system_includes = set()
definitions = set()


def resolve_path(file_name, relative_to):
    if os.path.isabs(file_name):
        return file_name

    base_dir = os.path.dirname(relative_to)
    result = os.path.join(base_dir, file_name)

    if os.path.isfile(result):
        return result

    for include_dir in args.include_dir:
        result = os.path.join(include_dir, file_name)
        if os.path.isfile(result):
            return os.path.abspath(result)

    return None


def cleanup_comments(lines):
    single_line_comment = re.compile(r'(?://.*)|(?:/\*.*\*/)')

    multiline_comment_start = re.compile(r'/\*.*')
    multiline_comment_end = re.compile(r'.*\*/')

    output = []

    in_multiline_comment = False

    for line in lines:
        if in_multiline_comment:
            if multiline_comment_end.match(line):
                in_multiline_comment = False
                output.append(re.sub(multiline_comment_end, '', line))
        else:
            line = re.sub(single_line_comment, '', line)

            if multiline_comment_start.search(line):
                in_multiline_comment = True
                line = re.sub(multiline_comment_start, '', line)

            output.append(line)

    return output


def is_definition_enabled(definition):
    return definition in definitions


def get_definition(match_result):
    for group in match_result.groups():
        if group:
            return group

    return None


def process_include(line, base_path):
    output = []

    include_re = re.compile(r'^#\s*include\s+"(.+)"\s*$')

    match = include_re.match(line)

    if not match:
        output.append(line)
    else:
        include_name = match.group(1)
        full_file_path = resolve_path(include_name, base_path)

        if full_file_path is not None:
            output = preprocess_file(resolve_path(include_name, base_path))

    return output


def cleanup_ifs(lines, base_path):
    output = []

    stack = [{
        "context_enabled": True
    }]

    def is_context_enabled():
        for ctx in stack:
            if not ctx["context_enabled"]:
                return False

        return True

    positive_re = re.compile(r'^(?:\s*(?:#\s*ifdef\s+(.+))|(?:#\s*if\s+([^!].*))|(?:#\s*elif\s+([^!].*)))$')
    negative_re = re.compile(r'^(?:\s*(?:#\s*ifndef\s+(.+))|(?:#\s*if\s+!(.+))|(?:#\s*elif\s+!(.+)))$')
    else_re = re.compile(r'^\s*#\s*else\s*$')
    endif_re = re.compile(r'^\s*#\s*endif\s*$')
    define_re = re.compile(r'^\s*#\s*define\s+([A-Za-z0-9_]+)(?:\s.*)?$')

    for line in lines:
        if positive_re.match(line):
            match = positive_re.match(line)
            definition = get_definition(match)

            if line.find("elif") < 0:
                stack.append({})

            stack[-1]["context_enabled"] = is_definition_enabled(definition)
        elif negative_re.match(line):
            match = negative_re.match(line)
            definition = get_definition(match)

            if line.find("elif") < 0:
                stack.append({})

            stack[-1]["context_enabled"] = not is_definition_enabled(definition)
        elif else_re.match(line):
            stack[-1]["context_enabled"] = not stack[-1]["context_enabled"]
        elif endif_re.match(line):
            stack.pop()
        elif is_context_enabled():
            define_match = define_re.match(line)

            if define_match is not None:
                definitions.add(define_match.group(1))

            output = output + process_include(line, base_path)

    return output


def gather_system_includes(lines):
    output = []

    include_re = re.compile(r'^#\s*include <([a-zA-Z0-9_.\/]*)>\s*$')

    for line in lines:
        match = include_re.match(line)

        if match:
            system_includes.add(match.group(1))
        else:
            output.append(line)

    return output



def preprocess_file(filename):
    output_lines = []

    with open(filename, 'r') as f:
        output_lines = f.readlines()

    output_lines = cleanup_comments(output_lines)
    output_lines = cleanup_ifs(output_lines, filename)

    return gather_system_includes(output_lines)


with open(args.output, "w") as outfile:
    outfile.write("// This header was generated from the FFMPEG headers\n")
    outfile.write("#pragma once\n\n")

    output = preprocess_file(os.path.abspath(args.input))

    for global_include in system_includes:
        outfile.write("#include <{}>\n".format(global_include))

    outfile.write("\n")

    prev_len = 0

    for line in output:
        curr_len = len(line.strip())

        if curr_len > 0 or prev_len > 0:
            outfile.write(line)

        prev_len = curr_len
