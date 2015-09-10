# Borrowed from http://www.blaenkdenum.com/
import sys

# for pig
from pygments import highlight
from pygments.formatters import HtmlFormatter
from pygments.lexers import get_lexer_by_name
from pygments.util import ClassNotFound

# for GDB
from pygments.lexer import RegexLexer, bygroups
from pygments.token import *

# courtesy of https://github.com/snarez/gdb_lexer
class GDBLexer(RegexLexer):
    name = 'GDB'
    aliases = ['gdb']
    filenames = ['*.gdb']

    string = r'"[^"]*"'
    char = r'[a-zA-Z$._0-9@]'
    identifier = r'(?:[a-zA-Z$_]' + char + '*|\.' + char + '+)'
    number = r'(?:0[xX][a-zA-Z0-9]+|\d+)'

    tokens = {
        'root': [
            (r'\s+', Text),
            (r'(\(?gdb[\)\$]|>)( )('+identifier+')(/?)(\d*)(\w*)',
                bygroups(Keyword.Type, Text, Name.Builtin, Text, Literal.Number.Integer, Keyword.Constant)),
            (number, Number.Hex),
            (string, String),
            (r'=', Operator),
            (r'(\$\d+)( = {)', bygroups(Name.Variable, Text), 'struct'),
            (r'\$'+identifier+'+', Name.Variable),
            (r'\$'+number+'+', Name.Variable),
            (r'#.*', Comment),
            (r'<snip>', Comment.Special),
            (r'<'+identifier+'+\+?\d*>', Name.Function),
            (r'->'+identifier+'+', Name.Attribute),
            (r'(\()(\s*struct\s*'+identifier+'+\s*\*)(\))', bygroups(Text, Keyword.Type, Text)),
            (r'\((int|long|short|char)\s*\*?', Keyword.Type),
            (r'\b(if)\b', Name.Builtin),
            (r'.', Text),
        ],
        'struct': [
            (r'(\s*)([^\s]*)( = {)', bygroups(Text, Name.Variable, Text), '#push'),
            (r'(\s*)([^\s]*)( = )', bygroups(Text, Name.Variable, Text)),
            (r'\s*},?', Text, '#pop'),
            (number, Number.Hex),
            (string, String),
            (r'.', Text)
        ],
   }

try:
    while True:
        lang = sys.stdin.readline().rstrip("\n")
        # EOF?
        if not lang:
            break
        kvs  = sys.stdin.readline().rstrip("\n")
        numb = int(sys.stdin.readline().rstrip("\n"))
        code = sys.stdin.read(numb)

        kvs_ = {}
        for kv in kvs.split('\0'):
            if '=' not in kv:
                continue
            k, v = kv.split('=', 1)
            kvs_[k] = v
        kvs = kvs_

        hl_lines = []
        for lines in kvs.get('hl_lines', '').split(','):
            if lines.isdigit():
                hl_lines.append(int(lines))
            elif '-' in lines:
                start, end = map(int, lines.split('-', 1))
                hl_lines += range(start, end + 1)

        linenostart = int(kvs.get('linenostart', '1'))

        hl_lines = [x - linenostart + 1 for x in hl_lines]

        html = HtmlFormatter(encoding='utf-8',
                             linenos='inline',
                             linespans='line',
                             hl_lines=hl_lines,
                             linenostart=linenostart,
        )

        rv = ""
        try:
            try:
                if lang == "gdb":
                    lex = GDBLexer(encoding="utf-8")
                else:
                    lex = get_lexer_by_name(lang, encoding="utf-8")
            except ClassNotFound as err:
                lex = get_lexer_by_name("text", encoding="utf-8")

            rv = highlight(code, lex, html)
        except ValueError as err:
            rv = "Pygments Error: {}".format(err)

        sys.stdout.write(str(len(rv)))
        sys.stdout.write("\n")
        sys.stdout.flush()

        if not hasattr(sys.stdout, 'buffer'):
            sys.stdout.write(rv)
            sys.stdout.flush()
        else:
            sys.stdout.buffer.write(rv)
            sys.stdout.buffer.flush()
except Exception as e:
    import traceback
    open('pygments-server.exception', 'a').write(traceback.format_exc())
