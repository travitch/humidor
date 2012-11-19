# Smoke Overview (for Binding Writers)

The [smoke](http://techbase.kde.org/Development/Languages/Smoke)
project was created as a part of KDE to allow languages besides C++ to
interact with Qt and KDE libraries.  There is some minimal
[API documentation](http://techbase.kde.org/Development/Languages/Smoke/API_Documentation),
which I am augmenting with this document as I discover more details.
Also note that this document is oriented towards people writing language
bindings to smoke, and not other potential uses.

# The Smoke Packages

Smoke is available from the
[kdebindings repository](https://projects.kde.org/projects/kde/kdebindings/smoke).
Note that there are three packages: smokegen, smokeqt, and smokekde.
smokegen is the main workhorse of the smoke project: it provides the
infrastructure to parse the headers of smokified libraries (like Qt
and the KDE libraries).  The most important parts of smokegen for
binding writers are `${PREFIX}/include/smoke.h` and
`${PREFIX}/lib/libsmokebase.so`.  Any binding generator will need to
compile against the former and link against the latter.

The smokeqt repository provides the metadata (patterns of classes to
skip and some other minor details) used by smokegen to generate
wrapper libraries for Qt.  The build process for smokeqt invokes
smokegen and generates the aforementioned wrapper libraries and puts
them in `${PREFIX}/lib/libsmoke${MODULE}.so`.  The generated wrapper
libraries are C++; note that your language binding will *not* directly
call into these generated C++ wrappers (though they are, of course,
still important).  Most importantly for binding writers, the build
process installs headers in
`${PREFIX}/include/smoke/${MODULE}_smoke.h`.  The value of `${MODULE}`
is one of the sub-modules of Qt like `qtgui` or `qtwebkit`.  The
smokekde repository is very similar, but it generates wrappers for the
KDE libraries.

## Smoke Libraries

If a language binding does not call into the generated C++ wrapper
libraries directly, what are they for?  Long story short, they have
two purposes:

 1) abstract the C++ calling convention and normalize the type
    signatures of all methods, and

 2) provide hooks to allow bindings to override virtual methods.

All methods are wrapped to have a type signature `void f(Stack x)`
where all arguments and return values are passed via the `Stack` type.
This is just an array of `StackItem`s (a union representing all types
that can be passed to Qt functions).  The largest element in this
union is a `double`, so `struct` types that are passed by value are
actually passed by pointer and munged along the way.  The
`${MODULE}_smoke.h` header generated with each wrapper library
provides metadata and a simple way to call each of these C++ methods
without having to deal with the C++ calling convention.  Instead, it
uses plain `stdcall` through C `struct`s and function pointers.

Smoke also provides a mechanism for overriding virtual methods: the
`SmokeBinding` class.  Each wrapped virtual method issues its call
through its `SmokeBinding` instance, which must be provided at object
construction time.  If the method has not been overridden, the
`SmokeBinding` simply delegates the call to the appropriate method in
Qt.

# Making a Language Binding

## Breaking Down `smoke.h`

This section provides a quick tour of `smoke.h`, the main interface
for a Qt language binding.  The bulk of this interface is through the
`Smoke` class.  Instances of this class are obtained from the headers
installed in `${PREFIX}/include/smoke`.  Each `Smoke` class is an
interface to one smoke-wrapped library (e.g., qtgui and qtwebkit).  At
a high level, each module is a list of the classes in that module,
along with some metadata about them and tools to call their methods.

### Module Data Members

Each `Smoke` object has a few important public data members:

```cpp
  Class *classes;
  Index numClasses;

  /**
   * The methods array defines every method in every class for this module
   */
  Method *methods;
  Index numMethods;

  /**
   * methodMaps maps the munged method prototypes
   * to the methods entries.
   */
  MethodMap *methodMaps;
  Index numMethodMaps;

  /**
   * Array of method names, for Method.name and MethodMap.name
   */
  const char **methodNames;
  Index numMethodNames;

  /**
   * List of all types needed by the methods (arguments and return values)
   */
  Type *types;
  Index numTypes;

  /**
   * Groups of Indexes (0 separated) used as super class lists.
   * For classes with super classes: Class.parents = index into this array.
   */
  Index *inheritanceList;
  /**
   * Groups of type IDs (0 separated), describing the types of argument for a
   * Method.args = index into this array.
   */
  Index *argumentList;
  /**
   * Groups of method prototypes with the same number of arguments, but diffe
   * Used to resolve overloading.
   */
  Index *ambiguousMethodList;
```

These are all important and are explained below.  Note that each of
these lists points to static data in the generated smoke libraries and
is not modified at run-time.  The pointer-typed members are arrays
that will be indexed into frequently.

### Smoke Classes

The smoke class descriptor is defined as:

```cpp
typedef void (*ClassFn)(Index method, void* obj, Stack args);
typedef void* (*CastFn)(void* obj, Index from, Index to);
typedef void (*EnumFn)(EnumOperation, Index, void*&, long&);

enum ClassFlags {
  cf_constructor = 0x01,  // has a constructor
  cf_deepcopy = 0x02,     // has copy constructor
  cf_virtual = 0x04,      // has virtual destructor
  cf_namespace = 0x08,    // is a namespace
  cf_undefined = 0x10     // defined elsewhere
};

struct Class {
  const char *className;  // Name of the class
  bool external;          // Whether the class is in another module
  Index parents;          // Index into inheritanceList
  ClassFn classFn;        // Calls any method in the class
  EnumFn enumFn;          // Handles enum pointers
  unsigned short flags;   // ClassFlags
  unsigned int size;
};
```

The smoke `Class` structure contains mostly metadata.  The `parents`
field is an index into the `inheritanceList` array.  That array
contains zero-terminated lists of the direct parent classes of each
class in the module.

```cpp
Index* parentIxs = smoke->inheritanceList[c->parents];
for(int i = 0; parentIxs[i] != 0; ++i) {
  Smoke::Class *parent = classes[parentIxs[i]];
}
```

The `classFn` member is used to invoke methods on objects.  It takes
(1) the Index of the method to call (ways to get this are discussed
later), (2) the object, and (3) a `Stack` of arguments (with space for
the return value -- note that calls made via `classFn` are void.

The `enumFn` member is similar, but to allocate and manipulate `enum`
values abstractly.  This pushes enums to the heap because the C++
compiler decides how big an `enum` value should be.  To extract the
integer values of enums, there are *static methods* in the Smoke class
that return concrete values in the `s_enum` field of `StackItem`s.
This field is the same size as a long and can accommodate any enum
value.

### Smoke Methods

```cpp
enum MethodFlags {
  mf_static = 0x01,
  mf_const = 0x02,
  mf_copyctor = 0x04,  // Copy constructor
  mf_internal = 0x08,   // For internal use only
  mf_enum = 0x10,   // An enum value
  mf_ctor = 0x20,
  mf_dtor = 0x40,
  mf_protected = 0x80,
  mf_attribute = 0x100,   // accessor method for a field
  mf_property = 0x200,    // accessor method of a property
  mf_virtual = 0x400,
  mf_purevirtual = 0x800,
  mf_signal = 0x1000, // method is a signal
  mf_slot = 0x2000,   // method is a slot
  mf_explicit = 0x4000    // method is an 'explicit' constructor
};

/**
 * Describe one method of one class.
 */
struct Method {
  Index classId;    // Index into classes
  Index name;   // Index into methodNames; real name
  Index args;   // Index into argumentList
  unsigned char numArgs;  // Number of arguments
  unsigned short flags; // MethodFlags (const/static/etc...)
  Index ret;    // Index into types for the return type
  Index method;   // Passed to Class.classFn, to call method
};
```

The `Method` contains just metadata.  The `classId` is an index into
the list of classes (a reference to the class owning the method).
Likewise the `name` is an index into the `methodNames` list (which
contains strings).  The `argumentList` is another index into a
zero-terminated list of indices, which index into the list of types.
Note that the names of arguments are not available through smoke --
they need to be recovered through other means.  The Qyoto bindings
(C#) have such a list.  I'm not exactly sure why the `numArgs` field
is present, given that `argumentList` is zero-terminated.  The `ret`
member is an index into the list of types.  The final member is the
index that is passed to the `callFn`:

```cpp
Smoke::Class *c = &smoke->classes[m->classId];
c->classFn(m->method, obj, argStack);
```

### Smoke Types

```cpp
enum TypeFlags {
  // The first 4 bits indicate the TypeId value, i.e. which field
  // of the StackItem union is used.
  tf_elem = 0x0F,

  // Always only one of the next three flags should be set
  tf_stack = 0x10,  // Stored on the stack, 'type'
  tf_ptr = 0x20,    // Pointer, 'type*'
  tf_ref = 0x30,    // Reference, 'type&'
  // Can | whatever ones of these apply
  tf_const = 0x40   // const argument
};

/**
 * One Type entry is one argument type needed by a method.
 * Type entries are shared, there is only one entry for "int" etc.
 */
struct Type {
  const char *name; // Stringified type name
  Index classId;    // Index into classes. -1 for none
  unsigned short flags;   // TypeFlags
};
```

### Smoke Bindings

```cpp
class SmokeBinding {
protected:
    Smoke *smoke;
public:
    SmokeBinding(Smoke *s) : smoke(s) {}
    virtual void deleted(Smoke::Index classId, void *obj) = 0;
    virtual bool callMethod(Smoke::Index method, void *obj, Smoke::Stack args,
    virtual char* className(Smoke::Index classId) = 0;
    virtual ~SmokeBinding() {}
};
```

## Dynamic Binding

## Static Binding
