#!/usr/bin/env python3
""" This is my attempt at making a default YCM config file for c++ projects """
import os.path

def Settings(**kwargs):
    """ This returns the default global settings to use for YCM """
    language = kwargs['language']
    filename = kwargs['filename']
    if language == 'cfamily':
        flags = [
            "-pedantic",
            "-Wall",
            "-Wcast-qual",
            "-Wextra",
            "-Wmissing-include-dirs",
            "-Wold-style-cast",
            "-Wredundant-decls",
            "-Wshadow",
            "-Wsign-conversion",
            "-Wswitch-default",
            '-x',
            'c++',
        ]
        hasSetVersion = False
        makefileloc = os.path.dirname(filename) + "/makefile"
        if os.path.exists(makefileloc):
            for line in open(makefileloc):
                lineloc = line.find("-std=c++")
                if lineloc is not -1:
                    flags.append(line[lineloc : lineloc + 10])
                    hasSetVersion = True
                    break
        if not hasSetVersion:
            flags.append("-std=c++14")
        return {
            'flags': flags
        }
    return {}
