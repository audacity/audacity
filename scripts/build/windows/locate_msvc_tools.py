import os

def _locate_tools(msvc_dir, msvc_version, edition):
    tools_base = os.path.join(msvc_dir, msvc_version, edition, 'VC', 'Tools', 'MSVC')

    if not os.path.isdir(tools_base):
        return None

    for name in sorted(os.listdir(tools_base), reverse=True):
        location = os.path.join(tools_base, name, 'bin', 'Hostx64', 'x64')
        if os.path.isdir(location):
            return location
        location = os.path.join(tools_base, name, 'bin', 'Hostx64', 'x86')
        if os.path.isdir(location):
            return location
        location = os.path.join(tools_base, name, 'bin', 'Hostx86', 'x64')
        if os.path.isdir(location):
            return location
        location = os.path.join(tools_base, name, 'bin', 'Hostx86', 'x86')
        if os.path.isdir(location):
            return location

    return None


def _locate_msvc_edition(msvc_dir, msvc_version):
    __editions = [
        'Enterprise',
        'Professional',
        'Community',
    ]

    for __edition in __editions:
        location = _locate_tools(msvc_dir, msvc_version, __edition)
        if location is not None:
            return location

    return None


def _locate_msvc_version(msvc_dir):
    for name in sorted(os.listdir(msvc_dir), reverse=True):
        location = _locate_msvc_edition(msvc_dir, name)
        if location is not None:
            return location

    return None


def locate_msvc_tools():
    __dirs = [
        r'C:\Program Files\Microsoft Visual Studio',
        r'C:\Program Files (x86)\Microsoft Visual Studio',
    ]

    for __dir in __dirs:
        if os.path.isdir(__dir):
            location = _locate_msvc_version(__dir)
            if location is not None:
                return location

    return None
