#include <smoke.h>
#ifdef HAVE_QTGUI
#include <smoke/qtgui_smoke.h>
#endif

#include <algorithm>
#include <vector>
#include <stdlib.h>

extern "C" {
  Smoke** smokeInitialize()
  {
    std::vector<Smoke*> smokes;

#ifdef HAVE_QTGUI
    init_qtgui_Smoke();
    smokes.push_back(qtgui_Smoke);
#endif
    smokes.push_back(0);

    Smoke **ret = (Smoke**)calloc(smokes.size(), sizeof(Smoke*));
    std::copy(smokes.begin(), smokes.end(), ret);

    return ret;
  }

  Smoke::Class* smokeClasses(Smoke *smoke)
  {
    return smoke->classes;
  }

  int smokeNumClasses(Smoke *smoke)
  {
    return smoke->numClasses;
  }

  Smoke::Method* smokeMethods(Smoke *smoke)
  {
    return smoke->methods;
  }

  int smokeNumMethods(Smoke *smoke)
  {
    return smoke->numMethods;
  }

  const char** smokeMethodNames(Smoke *smoke)
  {
    return smoke->methodNames;
  }

  const int smokeNumMethodNames(Smoke *smoke)
  {
    return smoke->numMethodNames;
  }

  Smoke::Type* smokeTypes(Smoke *smoke)
  {
    return smoke->types;
  }

  int smokeNumTypes(Smoke *smoke)
  {
    return smoke->numTypes;
  }

  short* smokeArgumentList(Smoke *smoke)
  {
    return smoke->argumentList;
  }

  short* smokeInheritanceList(Smoke *smoke)
  {
    return smoke->inheritanceList;
  }

  const char* smokeModuleName(Smoke *smoke)
  {
    return smoke->moduleName();
  }

  const char* smokeClassName(Smoke::Class *klass)
  {
    return klass->className;
  }

  int smokeClassExternal(Smoke::Class *klass)
  {
    return klass->external;
  }

  int smokeClassParents(Smoke::Class *klass)
  {
    return klass->parents;
  }

  unsigned int smokeClassFlags(Smoke::Class *klass)
  {
    return klass->flags;
  }

  int smokeClassSize(Smoke::Class *klass)
  {
    return klass->size;
  }

  int smokeMethodClassId(Smoke::Method *m)
  {
    return m->classId;
  }

  int smokeMethodName(Smoke::Method *m)
  {
    return m->name;
  }

  int smokeMethodArgs(Smoke::Method *m)
  {
    return m->args;
  }

  int smokeMethodNumArgs(Smoke::Method *m)
  {
    return m->numArgs;
  }

  unsigned int smokeMethodFlags(Smoke::Method *m)
  {
    return m->flags;
  }

  int smokeMethodRet(Smoke::Method *m)
  {
    return m->ret;
  }

  int smokeMethodMethod(Smoke::Method *m)
  {
    return m->method;
  }

  const char* smokeTypeName(Smoke::Type *t)
  {
    return t->name;
  }

  int smokeTypeClassId(Smoke::Type *t)
  {
    return t->classId;
  }

  unsigned int smokeTypeFlags(Smoke::Type *t)
  {
    return t->flags;
  }
}

#if defined(PROBE)
template<typename T> struct align { char c; T member; };
#define ALIGNOF(ty) offsetof(align<ty>, member)

#include <stdio.h>

int main() {
  printf("Class align=%d and size=%d\n",
      ALIGNOF(Smoke::Class), sizeof(Smoke::Class));
  printf("Method align=%d and size=%d\n",
      ALIGNOF(Smoke::Method), sizeof(Smoke::Method));
  printf("Type align=%d and size=%d\n",
      ALIGNOF(Smoke::Type), sizeof(Smoke::Type));

  return 0;
}
#endif
