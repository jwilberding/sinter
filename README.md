Sinter
======

Sinter provides a data definition language that is designed to be
machine readable. Sinter is designed to provide an api for to parse
this machine readable data definition language along with a plugin
framework as well as unified code generation capabilities based on
that plugin framework. By default Sinter ships with a plugin to
generate json.

Adding Types
------------

### Validation

### Plugin Support

**NOTE**

  Sinter creates Atoms in the Erlang Atom table based on the input it
reads in from the Sinter description files. With that in mind it is
not recommened parse Sinter files uploaded to your app as part of
continuing services. All Sinter compilation should be ahead of time.
