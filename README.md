![My image](http://s9.postimage.org/mxnmsv4en/projectured.png)

# Description #

ProjecturEd is a general purpose projectional editor written in Common Lisp.

It supports the integrated presentation and editing of arbitrary problem domains. These domains potentially include but not limited to: word processing, spreadsheets, markup languages, programming langueges, modelling, graphs, graphics, etc. and any combination of them. The edited data is represented in their natural, domain specific data structures (as opposed to a flat string of characters), which accommodates for the implementation of many interesting, but yet to be explored features of structured editing.

It also supports multiple projections of the same data, and thus it can simultaneously provide different notations, potentially all of them editable. These views can be textual, fully graphical, or in between, and due to the internal architecture of the editor projections combine well. It is expected that users would not only add new documents, but also new projections as needed.

# Running the Editor #

The easiest way to run the editor is to install it using [Quicklisp](http://quicklisp.org).

### 1. Get a reasonably fresh SBCL ###

For example, on Debian Jessie:

```
sudo apt-get install sbcl rlwrap git libsdl2-2.0-0 libsdl2-image-2.0-0 libsdl2-ttf-2.0-0
```

Otherwise visit the [SBCL website](http://sbcl.org/platform-table.html) for instructions, and make sure you have the right version of libSDL2 libraries installed for the SDL backend.

### 2. Install Quicklisp ###

Once you have SBCL installed, you can continue with [installing Quicklisp](http://www.quicklisp.org/beta/#installation):

```
cd ~
wget http://beta.quicklisp.org/quicklisp.lisp

sbcl --load quicklisp.lisp --eval "(quicklisp-quickstart:install)" --eval "(ql:add-to-init-file)" --eval "(exit)"
```

To finish the installation of Quicklisp approve the prompted question.

### 3. Check out some git repos ###

If you want to play with the latest (and potentially less stable) version of ProjecturEd:
```
cd ~/quicklisp/local-projects/
git clone https://github.com/projectured/projectured.git
```

At the time of writing the ASDF shipped with Quicklisp is too old,
so you may need to clone the latest release if your lisp implementation
is also shipped with an old ASDF:
```
cd ~/quicklisp/local-projects/
git clone https://gitlab.common-lisp.net/asdf/asdf.git --branch release
```

At the time of writing the following systems are not yet in Quicklisp
or contain unmerged changes:
```
cd ~/quicklisp/local-projects/
git clone https://github.com/attila-lendvai/cffi.git --branch c2ffi
git clone https://github.com/attila-lendvai/hu.dwim.sdl.git
```

### 4. Build a standalone executable of ProjecturEd ###

Run the following shell script:

```
~/quicklisp/local-projects/projectured/bin/build.sh
```

The output and the build log will be saved next to the build.sh script.

### 5. Run the editor ###

After the build has been completed you can run the editor from the command line with:

```
cd ~/quicklisp/local-projects/projectured/
bin/projectured -h
```

There are a few small example documents that you can promptly start editing:

```
cd ~/quicklisp/local-projects/projectured/
bin/projectured example/contact-list.json
```

Alternatively, you can run the editor by loading the project in your lisp. This allows experimentation with the code, changing this and that. Start SBCL with ```rlwrap sbcl```, and copy these into its REPL:

```
(ql:quickload :projectured.executable)
(projectured::executable-toplevel)
```

If everything went fine, then you should see a window presenting an empty generic document.

```
(ql:quickload :projectured.executable)
(projectured::executable-toplevel "~/quicklisp/local-projects/projectured/example/contact-list.json")
```

This last example will show a window presenting an example JSON document. You can always press Control + H to get context sensitive help in the editor.

### 5. Run some of the tests ###

Start SBCL with ```rlwrap sbcl```, and copy these into its REPL:

```
(ql:quickload :projectured.sdl.test)
(projectured.test::test)
```

This will run the automated test suite and print the result in the REPL.

### 4. If something goes wrong with loading ###

If you have some Common Lisp libraries installed somewhere, then try to make sure that nothing besides the quicklisp installed libs get loaded. You can achieve that by adding this to the beginning of your .sbclrc:

```
(require :asdf)
(funcall (read-from-string "asdf:initialize-source-registry")
         '(:source-registry :ignore-inherited-configuration))
```

# Collaboration #

It's an unfunded opensource project, under much slower development than we would like it to be. If you want to help us, either financially or with your patches, then feel free to get in touch at [projectured@gmail.com](mailto:projectured@gmail.com) or the [google group](http://groups.google.com/d/forum/projectured).

### Donation ###

The less we need to work to pay our bills, the more we can work on ProjecturEd. Bitcoin donations are welcome to the following address: 15XjRQUnSy8U7j2EFZyGFAi2KFnDnTEwqZ

### Wiki ###

There's a separate [**wiki**](https://github.com/projectured/projectured/wiki) with an increasing number of pages. It gives an overview of the whole idea, and it also allows contributing documentation to the project.

### Documentation ###

Currently the best thing to do is to look at the source code. Luckily the codebase is relatively small (around 20k LoC).

### Check out the latest version ###

If you want to try the latest version, then you should git clone the repository into ```~/quicklisp/local-projects/``` and Quicklisp will automatically use that version instead of the snapshot from its archives.

# Status #

ProjecturEd is work in progress, and at this stage it's interesting primarily for programmers who are ready to experiment with it.

### Screenshots ###

If you would like to take a look at the editor in action, then look at this [**page**](https://github.com/projectured/projectured/wiki/Screenshots). It contains a couple of screenshots showing various problem domains using different projections. Please note that these screenshots are somewhat outdated.

### Videos ###

There are some [screencasts on youtube](http://www.youtube.com/user/projectured) that shows the editor in action and demonstrate certain features.

### Performance ###

The printer and the reader are written using a "purely functional" constraint based change propagation algorithm that allows lazy and incremental computation of results. So, the infrastructure is in place to be fast enough to be useful one day...

# License #

[BSD License](https://github.com/projectured/projectured/wiki/License)

Happy hacking!
