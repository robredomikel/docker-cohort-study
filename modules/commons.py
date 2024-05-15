# COMMON PATHS & DATA USED WITHIN DIFFERENT MODULES
import pandas as pd

CASES_PATH = "/Users/mrobredo23/OULU/docker_cohort-24/data/cases"
CONTROLS_PATH = "/Users/mrobredo23/OULU/docker_cohort-24/data/controls"
DATA_PATH = "/Users/mrobredo23/OULU/docker_cohort-24/data/"
LOG_FILE_PATH = "/Users/mrobredo23/OULU/docker_cohort-24/error-repo-log.txt"

GITHUB_REPO_PATH = "https://api.github.com/repos/"
PROJECT_CREATION_FIRST = pd.to_datetime("01/01/2016", utc=True)  # We consider a longer project creation window
PROJECT_CREATION_LAST = pd.to_datetime("01/06/2021", utc=True)
github_token = ""

COMPLETE_ANALYSIS = False
CLONE_PROJECTS = True
POWER_ANALYSIS = False
REPO_ANALYSIS = True
FINAL_FILE_CREATION = True

CHECKOUT_BACK_TO_MASTER = False


# list of most used languages based on TIOBE INDEX (https://www.tiobe.com/tiobe-index/programminglanguages_definition/#instances)
TIOBE_LIST = ['foxpro', 'fox pro', 'vfp', 'vfpa', 'enterprise script', '4d', '4th dimension', 'abap', 'abc',
              'actionscript', 'as1', 'as2', 'as3', 'ada', 'agilent vee', 'algol', 'alice', 'angelscript', 'apex',
              'apl', 'applescript', 'arc', 'aspectj', 'assembly', 'assembly language', 'atlas', 'autohotkey', 'ahk',
              'autoit', 'autolisp', 'automator', 'avenue', 'awk', 'mawk', 'gawk', 'nawk', 'b4x', 'ballerina', 'bash',
              'basic (confidence', 'bbc basic', 'bc', 'bcpl', 'beta', 'blitzmax', 'blitzbasic', 'blitz basic', 'boo',
              'bourne shell', 'sh', 'brainfuck', 'csh', 'c shell', 'c#', 'c-sharp', 'c sharp', 'csharp', 'csharp.net',
              'c#.net', 'c++', 'c++/cli', 'c-omega', 'c', 'caml', 'carbon', 'ceylon', 'cfml', 'coldfusion', 'cg', 'ch',
              'chapel', 'chill', 'cil', 'citrine', 'cl', 'clarion', 'visual basic', 'clean', 'clipper', 'clips',
              'clojure', 'clojurescript', 'clu', 'cobol', 'cobra', 'coffeescript', 'comal', 'common lisp', 'coral 66',
              'crystal', 'ct', 'curl', 'd', 'dart', 'dcl', 'dwscript', 'object pascal', 'delphi', 'delphi.net', 'pascal',
              'dbl', 'synergy/de', 'dibol', 'dylan', 'e', 'ecmascript', 'egl', 'eiffel', 'elixir', 'elm', 'emacs lisp',
              'elisp', 'emerald', 'erlang', 'etoys', 'euphoria', 'exec', 'f#', 'f-sharp', 'fsharp', 'f sharp', 'factor',
              'falcon', 'fantom', 'felix', 'forth', 'fortran', 'fortress', 'freebasic', 'gambas', 'gams', 'glsl', 'gml',
              'gamemaker language', 'gnu octave', 'go, golang', 'gosu', 'groovy', 'gpath', 'gsql', 'groovy++', 'hack',
              'harbour', 'haskell', 'haxe', 'heron', 'hpl', 'hypertalk', 'icon',
              'idl', 'idris', 'inform', 'informix-4gl', 'intercal', 'io', 'ioke', 'j#', 'j', 'jade',
              'java', 'javafx script', 'javascript', 'js', 'ssjs', 'jscript', 'jscript.net', 'julia', 'julialang',
              'julia-lang',
              'korn shell', 'ksh', 'kotlin', 'labview', 'ladder logic', 'lasso', 'limbo', 'lingo', 'lisp', 'revolution', 'livecode',
              'logo', 'lotusscript', 'lpc', 'lua', 'luajit', 'lustre', 'm4', 'mad', 'magic',
              'magik', 'malbolge', 'mantis', 'maple', 'matlab', 'max/msp', 'maxscript', 'mdx', 'mel', 'mercury', 'miva', 'ml',
              'modula-2', 'modula-3', 'mojo', 'monkey', 'moo', 'moto', 'mql4, mql5', 'ms-dos batch', 'mumps', 'natural',
              'nemerle', 'netlogo', 'nim', 'nimrod', 'nix', 'nqc', 'nsis', 'nxt-g', 'oberon', 'object rexx', 'oorexx', 'open object rexx',
              'objective-c', 'objc', 'obj-c', 'objective caml', 'ocaml', 'occam', 'opencl',
              'progress', 'progress 4gl', 'abl', 'advanced business language', 'openedge', 'opl', 'oxygene', 'oz', 'paradox',
              'pascal', 'perl', 'php', 'pike', 'pilot', 'pl/1', 'pl/i', 'pl/sql', 'pliant', 'pony',
              'postscript', 'ps', 'pov-ray', 'powerbasic', 'powerscript', 'powershell', 'processing',
              'programming without coding technology', 'pwct', 'prolog', 'pure data', 'pd', 'purebasic', 'python', 'q',
              'r', 'racket', 'perl 6', 'raku', 'rebol', 'red', 'rexx', 'ring', 'rpg',
              'ruby', 'rust', 'rustlang', 's-plus', 's', 'sas', 'sather', 'scala', 'scheme',
              'scratch', 'sed', 'seed7', 'signal', 'simula', 'simulink', 'slate', 'small basic',
              'smalltalk', 'smarty', 'snap!', 'snobol', 'solidity', 'spark', 'spss', 'sql', 'sqr', 'squeak', 'squirrel',
              'standard ml', 'sml', 'stata', 'structured text', 'suneido', 'supercollider', 'swift',
              'systemverilog', 'verilog', 'tacl', 'tcl/tk', 'tcl', 'tcsh', 'tex', 'thinbasic', 'tom',
              't-sql', 'transact-sql', 'tsql', 'typescript', 'ts', 'uniface', 'vala', 'genie', 'vbscript', 'vhdl',
              'visual basic .net', 'vb.net', 'visual basic.net', 'visual basic', 'wasm', 'webassembly', 'webdna',
              'whitespace', 'mathematica', 'wolfram', 'x++', 'x10', 'xbase', 'xbase++', 'xc', 'xen', 'realbasic', 'xojo',
              'xpl', 'xquery', 'xslt', 'xtend', 'yacc', 'yorick', 'z shell', 'zsh', 'zig', 'zlang']