#ifndef _IDENT_H
#define _IDENT_H

typedef unsigned long FileOffset;

typedef struct _IdEntry {
    int constr;
#ifdef PROFILE
    int profInfo[EXTRA];
#endif
    struct _ModInfo *srcmod;
    int srcpos;
    char *name;
    int pri;
    FileOffset fileoffset;
} IdEntry;

typedef struct _ModInfo {
    char *srcfile;
    IdEntry *idtable;
    struct _ModInfo **modinfo;
    char *modname;
    FileOffset fileoffset;
} ModInfo;

typedef struct _SrcRef {
    int constr;
    int posn;
    ModInfo *modinfo;
    FileOffset fileoffset;
} SrcRef;

extern ModInfo *MODULE_Main;
extern ModInfo NMOD_Prelude;

void		showDbgInfo	(ModInfo *modInfo);
ModInfo*	findModule	(char *modname, ModInfo *modinfo);
int		isConstr	(char *name);
void		changeTrustedness (ModInfo *modInfo, char *fun, int constr);
void		changeTrustednessRecursively (ModInfo *modInfo, int constr);
void		trustModule	(char *mod, char *fun, int recursively);
void		suspectModule	(char *mod, char *fun, int recursively);

#endif
