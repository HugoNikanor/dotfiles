#!/usr/bin/env python2

# This file is run for offlineimap, it should be moved somewhere better

from subprocess import check_output

def get_pass(account):
    return check_output("pass " + account, shell=True).splitlines()[0]

def get_name():
    return "hugo"
