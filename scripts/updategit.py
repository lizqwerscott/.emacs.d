#!/usr/bin/python3

import os
import sys

site_lisp = ""

if len(sys.argv) > 0:
    if os.path.isdir(sys.argv[1]):
        site_lisp = sys.argv[1]
        print(site_lisp)

g = os.listdir(site_lisp)

print(g)

def isGitDir(path):
    if os.path.isdir(path):
        g = os.listdir(path)
        return ".git" in g
    return None

def runGitPull(path):
    if os.path.isdir(path):
        os.chdir(path)
        os.system("git pull .")

for i in g:
    path = site_lisp + i
    if isGitDir(path) and "emacs-application-framework" != i:
        print("git update", i)
        runGitPull(path)

