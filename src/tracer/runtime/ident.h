#ifndef _IDENT_H
#define _IDENT_H

typedef struct _IdEntry {
    int constr;
#ifdef PROFILE
    int profInfo[EXTRA];
#endif
    char *srcmod;
    int srcpos;
    char *name;
    int pri;
} IdEntry;

typedef struct _ModInfo {
    char *srcfile;
    IdEntry *idtable;
    struct _ModInfo **modinfo;
    char *modname;
} ModInfo;

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
