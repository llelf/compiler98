#include <stdio.h>
#include <string.h>
#include "newmacros.h"
#include "node.h"
#include "getconstr.h"
#include "ident.h"


void
showDbgInfo(ModInfo *modInfo)
{
    IdEntry *identry;
    int r, c, i;
 
#if 1
    fprintf(stderr, "%s(%s)\n", modInfo->srcfile, modInfo->modname);
#endif
    identry = modInfo->idtable;
    while (identry->constr != 0) {
	r = identry->srcpos / 10000;
	c = identry->srcpos % 10000;
#if 1
	fprintf(stderr, "  %c %s %d:%d\n",
		CONINFO_NUMBER(identry->constr) == NTId ? 'S' : 'T',
		identry->name, r, c);
#endif
	identry++;
    }
    i = 0;
    while (modInfo->modinfo[i] != NULL) {
	showDbgInfo(modInfo->modinfo[i++]);
    }
}


int zero = 0;
#define NULL_ID_TABLE (IdEntry*)&zero
#define NULL_MOD_TABLE (ModInfo **)&zero

ModInfo NMOD__Apply1 = {"_Apply1", NULL_ID_TABLE, NULL_MOD_TABLE, "Prelude"};
ModInfo NMOD__Apply2 = {"_Apply2", NULL_ID_TABLE, NULL_MOD_TABLE, "Prelude"};
ModInfo NMOD__Apply3 = {"_Apply3", NULL_ID_TABLE, NULL_MOD_TABLE, "Prelude"};
ModInfo NMOD__Apply4 = {"_Apply4", NULL_ID_TABLE, NULL_MOD_TABLE, "Prelude"};
ModInfo NMOD_PreludeBuiltin = {"PreludeBuiltin", NULL_ID_TABLE, NULL_MOD_TABLE, "Prelude"};
ModInfo NMOD_PreludeDebug = {"PreludeDebug", NULL_ID_TABLE, NULL_MOD_TABLE, "Prelude"};
ModInfo NMOD__EqInteger = {"_EqInteger", NULL_ID_TABLE, NULL_MOD_TABLE, "Prelude"};
ModInfo NMOD__Id = {"_Id", NULL_ID_TABLE, NULL_MOD_TABLE, "Prelude"};


#define	TRUST	CONSTR(NTTrusted, 3, 3)
#define	SUSPECT	CONSTR(NTId, 3, 3)

ModInfo *
findModule(char *modname, ModInfo *modinfo)
{
    ModInfo *found;
    int i;

    if (modinfo == NULL) {
      fprintf(stderr,"findModule: Reached null when looking for %s\n",modname);
      exit(1);
    }
#if 0
    fprintf(stderr, "Looking for %s in %s\n", modname, modinfo->srcfile);
#endif
    if (strcmp(modinfo->srcfile, modname) == 0)
	return modinfo;
    i = 0;
    if (modinfo->modinfo == NULL) {
      fprintf(stderr, "findModule: modinfo->modinfo == NULL !!!");
      exit(1);
    }
    while (modinfo->modinfo[i] != NULL) {
	found = findModule(modname, modinfo->modinfo[i]);
	if (found != NULL)
	    return found;
	i++;
    }
    return NULL;
}

int
isConstr(char *name) 
{
  int ch = name[0];
  return !((ch >= 'a' && ch <= 'z') || ch == '_');
}

void
changeTrustedness(ModInfo *modInfo, char *fun, int constr)
{
   IdEntry *identry = modInfo->idtable;
   int changed = 0;

   fprintf(stderr, "Changing trustedness for %s (%s)\n", modInfo->modname, 
	   fun == NULL ? "all functions" : fun);
   while (identry->constr != 0) {
     fprintf(stderr, "Module: %s\n", identry->srcmod);
     fprintf(stderr, "Checking %s\n", identry->name);
     if ((fun == NULL) || (strcmp(fun, identry->name) == 0)) {
       changed++;
       if (!(constr == TRUST && isConstr(identry->name)))
	 identry->constr = constr;
     }
     identry++;
   }
   if ((fun != NULL) && (changed == 0)) {
       fprintf(stderr, "Couldn't find %s in %s\n", fun, modInfo->srcfile);
   }
}

void
changeTrustednessRecursively(ModInfo *modInfo, int constr)
{
    int i;

#if 0
    fprintf(stderr, "%s(%s)\n", modInfo->srcfile, modInfo->modname);
#endif
    changeTrustedness(modInfo, NULL, constr);
    i = 0;
    while (modInfo->modinfo[i] != NULL) {
	changeTrustednessRecursively(modInfo->modinfo[i++], constr);
    }
}

void
trustModule(char *mod, char *fun, int recursively)
{
    ModInfo *modInfo = findModule(mod, MODULE_Main);
    
    if (modInfo == NULL)
        modInfo = findModule(mod, &NMOD_Prelude);

    if (modInfo != NULL) {
        if (recursively) {
	    changeTrustednessRecursively(modInfo, TRUST);
	} else {
	    changeTrustedness(modInfo, fun, TRUST);
        }
    } else {
	fprintf(stderr, "trustModule: Cannot find module %s\n", mod);
    }
}

void
suspectModule(char *mod, char *fun, int recursively)
{
    ModInfo *modInfo = findModule(mod, MODULE_Main);
    if (modInfo == NULL) {
        modInfo = findModule(mod, &NMOD_Prelude);
    }
    if (modInfo != NULL) {
        if (recursively) {
	    changeTrustednessRecursively(modInfo, SUSPECT);
	} else {
	    changeTrustedness(modInfo, fun, SUSPECT);
        }
    } else {
	fprintf(stderr, "suspectModule: Cannot find module %s\n", mod);
    }
}

