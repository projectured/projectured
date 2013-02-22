![My image](http://s9.postimage.org/mxnmsv4en/projectured.png)

Description
-----------

ProjecturEd is a generic purpose projectional editor written in Common Lisp. It supports editing multiple domains represented in arbitrary data structures. It also supports multiple bidirectional projections providing different notations varying from textual to graphics.

Join Us
-------

Unfortunately due to lack of time it's under quite *slow* construction. Send us a mail at projectured@gmail.com if you would like to participate.

Wiki
----

There's a separate [**wiki**](https://github.com/projectured/projectured/wiki) with an increasing number of pages. It gives an overview of the whole idea, and it also describes how to install, run and test the projectional editor.

Internals
---------

Currently the best thing to do is to look at the code. Surprisingly the code isn't that long, in fact it's less than 10,000 lines as of february, 2013. You may also read the source files in the documentation folder, although it's far from being well structured. 

Screenshots
-----------

If you would like to take a look at the editor in action, then look at this [**page**](https://github.com/projectured/projectured/wiki/Screenshots). It contains a couple of screenshots showing various problem domains using different projections.

Performance
-----------

Unfortunately performance is just not that good right now, although there are a few low hanging fruits in the code. The design also helps future improvements, because the printer and the reader are purely functional algorithms, so it wouldn't be that hard to utilize multiple cores for them. Finally, both the printer and the reader are written using a constraint based change propagation [library](http://dwim.hu/darcsweb/darcsweb.cgi?r=HEAD%20hu.dwim.computed-class;a=summary) that allows lazy and incremental computation of results. One day it might even be fast enough to be useful...

Limitations
-----------

The editor is far from being usable in the real world at the moment, so don't expect too much right now. You have been warned!

License
-------

[BSD License](https://github.com/projectured/projectured/wiki/License)

Happy hacking!
