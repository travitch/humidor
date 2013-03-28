This project is a [Qt](http://qt-project.org/) binding generator for Haskell
based on the [Smoke](http://techbase.kde.org/Development/Languages/Smoke)
project from KDE.  Smoke is a special Qt-oriented parser along with some
extra metadata to provide some introspection information via some shared
libraries accessible from C.

Dynamic languages can use Smoke to generate bindings at run-time on-demand.
More static languages can use tools like humidor to generate the bindings
statically ahead of time.  The Smoke library also provides a convenient
mechanism to call any Qt method through a plain C calling convention, rather
than relying on a C++ calling convention.

The project isn't quite in a working state right now, but it generates
a Haskell module for each Qt class, along with some typeclasses to
establish the relevant subtyping relationships.  The code to generate
a typeclass for each method is nearly working.  A few major TODO items:

 * Map Haskell types to C, especially Text and some container types

 * Finish generating method dispatchers

 * Permit subclassing (possibly via TH) from Haskell; this is critical.
   Lessons learned from hsqml could be useful here for registering
   subclasses with the Qt metaobject system.  Smoke also seems to provide
   a little support in this area.

