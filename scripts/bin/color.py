#!/usr/bin/env python3

"""
Generate files which can be 'cat'-ed to change the terminal color.

Uses ~/dotfiles/color.yaml as imput, and outputs into
$XDG_CACHE_HOME/colors/$term/$host
"""

import yaml
import os.path
from typing import (
    Literal,
    Optional,
    TypeAlias,
    TypedDict,
    Union,
)

import pwd
HOME = os.getenv('HOME') or pwd.getpwnam(os.getlogin()).pw_dir

try:
    from xdg.BaseDirectory import xdg_cache_home
except ModuleNotFoundError:
    xdg_cache_home = os.path.join(HOME, '.cache')


# https://bottosson.github.io/misc/colorpicker/#335e73


class Colors(TypedDict):
    normal: Optional[str]
    bright: Optional[str]


class Colorscheme(TypedDict):
    primary: dict[str, str]
    colors: dict[str, Colors]


default_colorscheme: Colorscheme = {
    'primary': {
        'foreground': 'ffffff',
        'background': '000000',
    },
    'colors': {
        'black': {
            'normal': '000000',
            'bright': '4d4d4d',
        },
        'red': {
            'normal': 'cd0000',
            'bright': 'ff0000',
        },
        'green': {
            'normal': '00cd00',
            'bright': '00ff00',
        },
        'yellow': {
            'normal': 'cdcd00',
            'bright': 'ffff00',
        },
        'blue': {
            'normal': '0000cd',
            'bright': '0000ff',
        },
        'magenta': {
            'normal': 'cd00cd',
            'bright': 'ff00ff',
        },
        'cyan': {
            'normal': '00cdcd',
            'bright': '00ffff',
        },
        'white': {
            'normal': 'e5e5e5',
            'bright': 'ffffff',
        },
    }
}

# https://man7.org/linux/man-pages/man4/console_codes.4.html
# console_codes(4)


color_codes: dict[str, int] = {
    'black':   0,
    'red':     1,
    'green':   2,
    'yellow':  3,
    'blue':    4,
    'magenta': 5,
    'cyan':    6,
    'white':   7,
}


NORMAL = 0
BRIGHT = 8

offkey: dict[int, str] = {
    NORMAL: 'normal',
    BRIGHT: 'bright',
}

ColorName: TypeAlias = Union[Literal['black'],
                             Literal['red'],
                             Literal['green'],
                             Literal['yellow'],
                             Literal['blue'],
                             Literal['magenta'],
                             Literal['cyan'],
                             Literal['white']]



OSC = b'\033]'
ST = b'\033\\'


def osc(ps: bytes, pt: Optional[bytes] = None) -> bytes:
    """
    Returns an OSC escape sequence.

    OSC (Operating system command)
    https://invisible-island.net/xterm/ctlseqs/ctlseqs.html#h3-Operating-System-Commands
    """
    if pt:
        return b''.join([OSC, ps, b';', pt, ST])
    else:
        return b''.join([OSC, ps, ST])


# for linux:
# "\e]P0${color}"


class Primary(TypedDict):
    foreground: Optional[str]
    background: Optional[str]


def handle_primary(primary: Primary) -> bytes:
    b = b''
    if fg := primary['foreground']:
        b += osc(b'10', b'#' + fg.encode('ASCII'))
    if bg := primary['background']:
        b += osc(b'11', b'#' + bg.encode('ASCII'))
    return b


def handle_colors(color_dict: dict[ColorName, Colors]) -> bytes:
    b = b''
    for key, values in color_dict.items():
        for offset in (NORMAL, BRIGHT):
            k = offkey[offset]
            value = values.get(k, default_colorscheme['colors'][key][k])
            b += osc(bytes([color_codes[key] + offset + ord('0')]),
                     b'#' + value.encode('ASCII'), )
    return b


def mkdir_safe(dir):
    try:
        os.mkdir(dir)
    except FileExistsError:
        pass


def main():
    with open(os.path.join(HOME, 'dotfiles/color.yaml')) as f:
        data = yaml.safe_load(f)

    cache = os.path.join(xdg_cache_home, 'colors')
    mkdir_safe(cache)
    mkdir_safe(os.path.join(cache, 'osc'))
    for host, data in data['hosts'].items():
        with open(os.path.join(cache, 'osc', host), 'wb') as f:
            if x := data.get('colors'):
                f.write(handle_colors(x))
            if x := data.get('primary'):
                f.write(handle_primary(x))


if __name__ == '__main__':
    main()
