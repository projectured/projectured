![My image](http://s9.postimage.org/mxnmsv4en/projectured.png)

# Description #

ProjecturEd is a general purpose projectional editor written in Common Lisp.

It supports the integrated presentation and editing of arbitrary domains. The edited data is represented in their natural, domain specific data structures (as opposed to a flat string of characters), which accommodates for the implementation of many interesting, but yet to be explored features of structured editing.

It also supports multiple projections of the same object, and thus it can simultaneously provide different notations, potentially all of them editable. These views can be textual or fully graphical, are easy to add to the framework, and due to the architecture they combine well.

# Running the demos #

The easiest way to run the ProjecturEd demos is to install it using [Quicklisp](http://quicklisp.org).

### 1. Get an SBCL with a fairly new ASDF ###

At the time of writing (2014-02-17) the [ASDF](http://common-lisp.net/project/asdf/) version in most packaged lisp implementations is not recent enough for PrEd. If you don't want to compile SBCL yourself, then you can follow these instructions to have an SBCL with a fairly new ASDF:

```
sudo apt-get install sbcl rlwrap
mkdir -p ~/quicklisp/local-projects/
cd ~/quicklisp/local-projects/
git clone git://common-lisp.net/projects/asdf/asdf.git
cd asdf
git checkout 3.1.0.65
make
```

Then fire up your favorite editor and make sure the following is part of your ```~/.sbclrc``` file:

```
;;; -*- mode: common-lisp -*-

(require :asdf)

(funcall (read-from-string "asdf:initialize-source-registry")
        `(:source-registry
          (:directory ,(merge-pathnames "quicklisp/local-projects/asdf/" (user-homedir-pathname)))
          :inherit-configuration))

;; initiate asdf upgrade
(asdf:load-system :asdf)
```

### 2. Install Quicklisp ###

Once you have an SBCL and a fairly fresh ASDF, let's [install Quicklisp](http://www.quicklisp.org/beta/#installation):

```
cd ~
wget http://beta.quicklisp.org/quicklisp.lisp

rlwrap sbcl --load quicklisp.lisp
```

When SBCL started up properly, copy these into its REPL (one by one):

```
(quicklisp-quickstart:install)
(ql:add-to-init-file)
```

### 3. Run some of the tests ###

Start SBCL with ```rlwrap sbcl```, and copy these into its REPL:

```
(ql:quickload :projectured.sdl.test)
(projectured.test::test/editor/json/sorting)
```

If everything went fine, then you should see a window presenting a json test document.

# Collaboration #

It's an unfunded opensource project, under much slower development than we would like it to be. If you want to help us, either financially or with your patches, then feel free to get in touch at [projectured@gmail.com](mailto:projectured@gmail.com) or the [google group](http://groups.google.com/d/forum/projectured).

### Donation ###

You may want to donate to help this project by sending bitcoins (BTC) to 15XjRQUnSy8U7j2EFZyGFAi2KFnDnTEwqZ

### Wiki ###

There's a separate [**wiki**](https://github.com/projectured/projectured/wiki) with an increasing number of pages. It gives an overview of the whole idea, and it also describes how to install, run and test the projectional editor.

### Internals ###

Currently the best thing to do is to look at the source code. Luckily the codebase is relatively small (around 10-15k LoC). You may also read the source files in the documentation folder, although it's far from being well structured.

### Check out the latest version ###

If you want to use with the latest version, then you should git clone the repository into ```~/quicklisp/local-projects/``` and Quicklisp will automatically use that version instead of the snapshot from its archives.

# Status #

ProjecturEd is work in progress, and at this stage it's interesting primarily for programmers who are ready to experiment with it.

### Screenshots ###

If you would like to take a look at the editor in action, then look at this [**page**](https://github.com/projectured/projectured/wiki/Screenshots). It contains a couple of screenshots showing various problem domains using different projections.

### Videos ###

There are some [screencasts on youtube](http://www.youtube.com/user/projectured).

### Performance ###

It would be too early to extensively optimize the codebase at this point, but the design accommodates for future improvements. The printer and the reader are purely functional algorithms, so it wouldn't be that hard to utilize multiple cores for them. And both of them are written using a constraint based change propagation [library](http://dwim.hu/darcsweb/darcsweb.cgi?r=HEAD%20hu.dwim.computed-class;a=summary) that allows lazy and incremental computation of results. So, the infrastructure is in place to be fast enough to be useful one day...

# License #

[BSD License](https://github.com/projectured/projectured/wiki/License)

Happy hacking!
