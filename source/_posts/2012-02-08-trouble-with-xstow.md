---
published: true
layout: post
title: Trouble with xstow
---

I am trying to use xstow to manage deploying a set of scripts to ~/bin. However,
both the version installed on Ubuntu 11.10 and a version compiled from scratch
do not work as expected.

### Intended Results


My scripts live in a git repo with the following layout:

    ~/src/scripts/bin/*
    ~/src/scripts/README
    ~/src/scripts/...

I would like to *only* install the files under bin/. I assumed the following
xstow.ini would work:

    [matches]
    nignore = bins

    [bins]
    dir = /home/b/bin
    follow = false

### Actual Results

The version in the Ubuntu repo seems to do exactly the opposite of what is
intended: rather than *not* ignoring files being installed to ~/bin, it *does*
ignore them.

The version from source seems to just ignore everything.

#### From Ubuntu 11.10 repo

    b@avery:~/src$ xstow -V
    XStow Version 0.5.1 (C) 2002-2003 by Martin Oberzalek
             regular expression support
             shell pattern matching support
             configuration file support
    b@avery:~/src$ xstow -v -n scripts
    SET_LINK: src/scripts/tickler.bashrc => /home/b/tickler.bashrc
    SET_LINK: src/scripts/README-vimdiffem.md => /home/b/README-vimdiffem.md
    SET_LINK: src/scripts/README.md => /home/b/README.md
    SET_LINK: src/scripts/README-tickler.md => /home/b/README-tickler.md
    SET_LINK: src/scripts/.git => /home/b/.git
    b@avery:~/src$ 

#### From source

    b@avery:~/src$ ~/tmp/xstow-1.0.0/src/xstow -V
    XStow Version 1.0.0 (C) 2002-2005 by Martin Oberzalek
             regular expression support
             shell pattern matching support
             configuration file support
    b@avery:~/src$ ~/tmp/xstow-1.0.0/src/xstow -v -n scripts

    b@avery:~/src$ 
