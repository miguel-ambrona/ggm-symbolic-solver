#!/usr/bin/python
# -*- coding: utf-8 -*-

import sys
from time import *

from os import listdir
from os.path import isfile, join

from subprocess import Popen, PIPE, STDOUT

def main():
    global p
    files = Popen(["find", "."], stdout=PIPE).communicate()[0].split("\n")
    ggm_files = [f for f in files if f[-4:] == ".ggm"]
    ggm_files.sort()
    maxlen = max([len(name) for name in ggm_files])
    
    for fname in ggm_files:
        complete_name = " "*(maxlen-len(fname))
        print fname, complete_name,
        f = open(fname, 'r')
        content = ""
        for line in f.readlines():
            content += line
        content = content.replace("\n","")
        timesum = 0
        N = 1 # Number of repetitions for every scheme
        for i in range(N):
            t1 = time()
            p.stdin.write(content + "\n")
            p.stdin.flush()
            output = p.stdout.readline()
            t2 = time()
            timesum += t2-t1

        total = timesum/N

        if "Error" in output or "Failure" in output:
            print('\x1b[1;31m' + 'Error\t' + '\x1b[0m'),
        elif "no goals" in output:
            print('\x1b[1;32m' + 'Proven!\t' + '\x1b[0m'),
        else:
            print('\x1b[1;31m' + 'Not proven\t' + '\x1b[0m'),

        print " Time:",
        print('\x1b[1;33m' + str(int(1000*(total))/1000.0) + '\x1b[0m')
        

if __name__ == "__main__":
    p = Popen("./solver.native", stdout=PIPE, stdin=PIPE, stderr=STDOUT)
    p.stdin.write("1 = 0.contradiction.\n") # Initialize the solver with a dummy problem.
    p.stdin.flush()
    output = p.stdout.readline()
    print "\nInitialized solver!\n"
    main()
