/* tprof.c
 *   Author: Phil Hassall
 *           University of York
 *           Spring 1998
 */

#include <string.h>
#include "runtime.h"
#include "newmacros.h"  /* only for DIR_DEL macro ! */

#define MAX_HEIGHT(x,y) (x>y ? x : y)
#define CMP_KEY(x,y) (x==y ? 0 : ((x<y) ? -1 : 1))
#define MAX_FILE_NAME 80
#define MAX_MODULES 200

/* No need to do any ifdef PROFILE as this only included in Profile version */

int tprof;
FILE *tpFILE;
int collapse_mode;
int listedby;
int enter_percentages;
int combine_System_IO;
int total_enters;
int total_ticks;
int total_g_enters;
int total_g_ticks;
int in_greencard;
int collecting_garbage;
int collect_garbage_enters;
int collect_garbage_ticks;

/****************************************************** AVL tree functions */

typedef struct avl
{
  struct avl *left, *right;
  int height;
  CodePtr *address;
  char *funname;
  char *module;
  int *enters;
  int ticks;
  int g_enters;
  int g_ticks;
} AVLnode;

AVLnode *address_AVL_root;
AVLnode *last_right_taken;
AVLnode *last_entered_node;
int **enterPtrTmp;
int *last_enter;
int *penultimate_enter;
int *in_greencard_ticks;
int assign_subfn_enters;
int *junk_enters;

struct moduletotals {
  char *module;
  int enters;
  int ticks;
} moduleTotals[MAX_MODULES];

int tprofModules_size;
struct tproftotals {
  char *module;
  int already_made_node;
  AVLnode *node;
} tprofModules[MAX_MODULES];

int moduleTotals_size;
char *last_module_printed = "Talyllyn";

char *tprofModDefaults[] = {"System","IO","Prelude"};
int tprofModDefaultsUse[] = {1,1,1};
int tprofModDefaults_size = 3;

static int get_height_AVL(AVLnode *tree)
{ 
  if (tree == NULL)
    return(-1);
  else
    return(tree->height);
}

static void set_height_AVL(AVLnode *tree)
{ 
  if (tree != NULL)
    tree->height = 1 + MAX_HEIGHT(get_height_AVL(tree->left), get_height_AVL(tree->right));
}

static void right_rotate_AVL(AVLnode **tree)
{
  AVLnode *old_root, *old_left, *middle;
  
  old_root = *tree;
  old_left = old_root->left;
  middle = old_left->right;
  old_left->right = old_root;
  old_root->left = middle;
  *tree = old_left;
  set_height_AVL(old_root);
  set_height_AVL(old_left);
}

static void left_rotate_AVL(AVLnode **tree)
{
  AVLnode *old_root, *old_right, *middle;

  old_root = *tree;
  old_right = old_root->right;
  middle = old_right->left;
  old_right->left = old_root;
  old_root->right = middle;
  *tree = old_right;
  set_height_AVL(old_root);
  set_height_AVL(old_right);
}

static void double_right_rotate_AVL(AVLnode **tree)
{
  left_rotate_AVL(&((*tree)->left));
  right_rotate_AVL(tree);
}

static void double_left_rotate_AVL(AVLnode **tree)
{
  right_rotate_AVL(&((*tree)->right));
  left_rotate_AVL(tree);
}

static void balance_AVL(AVLnode **tree)
{
  int left_height,right_height;
  AVLnode *subtree;

  left_height = get_height_AVL((*tree)->left);
  right_height = get_height_AVL((*tree)->right);
  if (left_height > right_height+1)
    {
      subtree = (*tree)->left;
      left_height = get_height_AVL(subtree->left);
      right_height = get_height_AVL(subtree->right);
      if (left_height >= right_height)
        {          
	  right_rotate_AVL(tree);   
	}
      else
        {
          double_right_rotate_AVL(tree);
        }
    }
  else
  if (left_height+1 < right_height)
    {
      subtree = (*tree)->right;
      left_height = get_height_AVL(subtree->left);
      right_height = get_height_AVL(subtree->right);
      if (right_height >= left_height)
        {
          left_rotate_AVL(tree);
        }
      else
        {
          double_left_rotate_AVL(tree);
        }
    }
}

static void init_AVL(AVLnode **tree)
{ 
  *tree = NULL;
}

static void insert_info_address_AVL(AVLnode **tree, CodePtr *address, char *funname, int insert_mode)
{
  AVLnode *newtree;
  int cmp;
  char *moduledot;

  if (*tree == NULL) {
    if (insert_mode==1) {
      newtree = (AVLnode *) malloc(sizeof(AVLnode));
      newtree->left = NULL;
      newtree->right = NULL;
      newtree->height = 0;
      (newtree->address) = address;
      moduledot = strchr(funname,'.');
      *moduledot = '\0';
      newtree->module = funname;
      newtree->funname = moduledot+1;
      newtree->enters = *enterPtrTmp;
      *(newtree->enters) = 1;
      newtree->g_enters = 0;
      newtree->ticks = 0;
      newtree->g_ticks = 0;
      *tree = newtree;
      penultimate_enter = last_enter;
      last_enter = (*tree)->enters;
      last_entered_node = *tree;
    }
    else {
      if (last_right_taken == NULL) 
	fprintf(stderr,"Oops, trying to record enter/tick into an empty AVL tree");
      else {
	if (insert_mode>2) {
	  int *lrt_enters;
	  lrt_enters = last_right_taken->enters;
	  *enterPtrTmp = lrt_enters;
	  (**enterPtrTmp)++;
	  penultimate_enter = last_enter;
	  last_enter = *enterPtrTmp;
	}
	else
	  if (insert_mode == 2) {
	    last_right_taken->g_enters++;
	    in_greencard_ticks = &(last_right_taken->g_ticks);
	  }
	  else
	    last_right_taken->ticks++;
      }
    }
  }
  else {
    cmp = CMP_KEY(address,(*tree)->address);
    if (cmp == 0) {
      if (insert_mode) {
	if (insert_mode == 2) {
	  (*tree)->g_enters++;
	  in_greencard_ticks = &((*tree)->g_ticks);
	}
	else
	  (*((*tree)->enters))++;
	penultimate_enter = last_enter;
	last_enter = (*tree)->enters;
      }
      else {
	(*tree)->ticks++;
      }
    }
    else {
      if (cmp < 0)
	insert_info_address_AVL(&((*tree)->left), address, funname, insert_mode);
      else {
	if (insert_mode!=1) last_right_taken = *tree;
	insert_info_address_AVL(&((*tree)->right), address, funname, insert_mode);
      }
      set_height_AVL(*tree);
      balance_AVL(tree);
    }
  }
}

static void module_totals(AVLnode *node)
{ 
  int i;

  total_enters   += *(node->enters);
  total_ticks    += node->ticks+node->g_ticks;
  total_g_enters += node->g_enters;
  total_g_ticks  += node->g_ticks;

  for (i=0;i<moduleTotals_size;i++) {
    if (strcmp(moduleTotals[i].module,node->module)==0) {
      moduleTotals[i].enters += *(node->enters);
      moduleTotals[i].ticks  += node->ticks+node->g_ticks;
      break;
    }
  }
  if (i==moduleTotals_size) {
    moduleTotals[moduleTotals_size].module   = node->module;
    moduleTotals[moduleTotals_size].enters   = *(node->enters);
    moduleTotals[moduleTotals_size++].ticks  = node->ticks+node->g_ticks;
  }
  if (moduleTotals_size==MAX_MODULES) {
    fprintf(stderr,"Array of Module names > MAX_MODULES");
    exit(-1);
  }
}

static void insert_node_ticks_AVL(AVLnode **tree, AVLnode **node)
{  
  int cmp;

  if (*tree == NULL) {
    *tree = *node;
    module_totals(*tree);
  }
  else {
    if (!listedby) {    /* Order by module name      */
      cmp = strcmp((*node)->module,(*tree)->module);
    }
    else
      cmp = 0;
    if (listedby!=2) {        /* listedby==0 or 1          */
      if (cmp == 0) {         /* Order by ticks (normal+g) */
	cmp = CMP_KEY((*tree)->ticks+(*tree)->g_ticks,(*node)->ticks+(*node)->g_ticks);
	if (cmp == 0) {       /* Order by enters           */
	  cmp = CMP_KEY(*((*tree)->enters),*((*node)->enters));
	  if (cmp == 0)       /* Order by funname          */
	    cmp = strcmp((*node)->funname,(*tree)->funname);
	}
      }  
    }
    else {                    /* listedby==2               */
      if (cmp == 0) {         /* Order by enters           */
	cmp = CMP_KEY(*((*tree)->enters),*((*node)->enters));
	if (cmp == 0) {       /* Order by ticks (normal+g) */
	  cmp = CMP_KEY((*tree)->ticks+(*tree)->g_ticks,(*node)->ticks+(*node)->g_ticks);
	  if (cmp == 0)       /* Order by funname          */
	    cmp = strcmp((*node)->funname,(*tree)->funname);
	}
      }  
    }
    if (cmp < 0)                                          
      insert_node_ticks_AVL(&((*tree)->left), node);
    else 
      insert_node_ticks_AVL(&((*tree)->right), node);
    set_height_AVL(*tree);
    balance_AVL(tree); 
  }
}

static void discard_AVL(AVLnode **tree)
{                                        /* Not "delete node and rebalance" as    */
  AVLnode *left_sub, *right_sub, *root;  /* I never need to do that.  This simply */
                                         /* frees up memory when I'm done with it */
  if (*tree != NULL) {                
    discard_AVL(&((*tree)->left));  
    discard_AVL(&((*tree)->right));
    free(root->funname);
    free(root->module);
    free(root);
    *tree = NULL;
  }
}

static int collapse_module(char *module)
{
  int i;
  if(collapse_mode==3) return 0;
  for (i=0;i<tprofModules_size;i++) {
    if (strcmp(module,tprofModules[i].module)==0)
      return (i+1);
  }
  if(collapse_mode==2) {
    tprofModules[tprofModules_size++].module = module;
    return tprofModules_size;
  }
  return 0;
}

static void print_AVL_summary_info(char *module, int collapsed, FILE *fp)
{
  int i;
  char sp;

  if (combine_System_IO && (strcmp(module,"System")==0)) {
    module = strdup("System & IO");
  }
  if (!(combine_System_IO && (strcmp(module,"IO")==0))) {
    if(strlen(module)<7) sp='\t'; else sp=' ';
    for (i=0;i<moduleTotals_size;i++) {
      if (strcmp(moduleTotals[i].module,module)==0) {
	if(!collapsed)
	  fprintf(fp, "\n----------------------------------------------------------");
	if (listedby==2)
	  if (enter_percentages)
	      fprintf(fp, "\n%s%c\tEnters = %d (%.1f%%)\tTime = %.1f%%\n", module, sp, moduleTotals[i].enters, 100*((float) (moduleTotals[i].enters))/total_enters, 100*((float) (moduleTotals[i].ticks))/total_ticks);
	  else
	    fprintf(fp, "\n%s%c\tEnters = %d\tTime = %.1f%%\n", module, sp, moduleTotals[i].enters, 100*((float) (moduleTotals[i].ticks))/total_ticks);
	else
	  if (enter_percentages)
	    fprintf(fp, "\n%s%c\tTime = %.1f%%\tEnters = %d (%.1f%%)\n", module, sp, 100*((float) (moduleTotals[i].ticks))/total_ticks, moduleTotals[i].enters, 100*((float) (moduleTotals[i].enters))/total_enters);
	  else
	    fprintf(fp, "\n%s%c\tTime = %.1f%%\tEnters = %d\n", module, sp, 100*((float) (moduleTotals[i].ticks))/total_ticks, moduleTotals[i].enters);
	if(!collapsed)
	  fprintf(fp, "----------------------------------------------------------\n");
	break;
      }
    }
  }
}

static void print_AVL_info(AVLnode *tree, FILE *fp)
{
  if (!collapse_module(tree->module)) {
    if (strcmp("_Driver",tree->module)!=0) {
      if (listedby!=2)
	fprintf(fp, "%.1f\t", 100*((float) (tree->ticks+tree->g_ticks))/total_ticks);
      if (tree->g_enters)
	fprintf(fp, "%d\t", tree->g_enters);
      else
	fprintf(fp, "%d\t", *(tree->enters));
      if (listedby==2)
	fprintf(fp, "%.1f\t", 100*((float) (tree->ticks+tree->g_ticks))/total_ticks);
      if (listedby) {
	if (tree->g_enters)
	  fprintf(fp, "*%s.",  tree->module);
	else
	  fprintf(fp, " %s.",  tree->module);
      }
      if (!listedby)
	if (tree->g_enters)
	  fprintf(fp, "*%s\n", tree->funname);
	else
	  fprintf(fp, " %s\n", tree->funname);
      else
	fprintf(fp, "%s\n", tree->funname);
    }
  }
}

static void print_AVL_node(AVLnode *tree, FILE *fp)
{
  if (tree != NULL) {
    print_AVL_node(tree->left, fp);
    if (!listedby && (strcmp(tree->module,last_module_printed)!=0)) {
      if (!collapse_module(tree->module)) {
	if (strcmp("_Driver",tree->module)!=0)
	  print_AVL_summary_info(tree->module, 0, fp);
	last_module_printed = tree->module;
      }
    }
    print_AVL_info(tree,fp);
    print_AVL_node(tree->right, fp);
  }
}

static int rearrange_AVL(AVLnode **tick_tree, AVLnode **address_tree)
{ 
  AVLnode **lft, **rht;

  if (*address_tree != NULL) {
    lft = &((*address_tree)->left); 
    if (*lft != NULL) rearrange_AVL(tick_tree, lft);
    ((*address_tree)->left) = NULL;
    rht = &((*address_tree)->right);
    if (*rht != NULL) rearrange_AVL(tick_tree, rht);
    ((*address_tree)->right) = NULL;
    insert_node_ticks_AVL(tick_tree, address_tree);
  }
}

void output_AVL_as_orderd_table(AVLnode **tree, FILE *fp)
{ 
  AVLnode *ticks_AVL_root;
  int i;

  if (*tree != NULL) {
    init_AVL(&ticks_AVL_root);
    rearrange_AVL(&ticks_AVL_root, tree);
    tree = &ticks_AVL_root;
    if(combine_System_IO) {
      int system, io;
      for (i=0;i<moduleTotals_size;i++) {
	if(strcmp(moduleTotals[i].module,"System")==0)
	  system=i;
	if(strcmp(moduleTotals[i].module,"IO")==0)
	  io=i;
	}
      moduleTotals[system].module = strdup("System & IO");
      moduleTotals[system].enters += moduleTotals[io].enters;
      moduleTotals[system].ticks  += moduleTotals[io].ticks;
      moduleTotals[io].module = moduleTotals[moduleTotals_size-1].module;
      moduleTotals[io].enters = moduleTotals[moduleTotals_size-1].enters;
      moduleTotals[io].ticks  = moduleTotals[moduleTotals_size-1].ticks;
      moduleTotals_size--;
    }
    total_ticks += collect_garbage_ticks;
    if (total_ticks==0) total_ticks=1;
    if (listedby && collapse_mode!=2) {
      if (listedby==1) {
	fprintf(fp, "\nTime\tEnters\t Function\n");
	fprintf(fp, "----------------------------------------------------------\n");
      }
      else {
	fprintf(fp, "\nEnters\tTime\t Function\n");
	fprintf(fp, "----------------------------------------------------------\n");
      }
    }
    print_AVL_node(*tree, fp);

    if (collapse_mode!=3)
      for (i=0;i<tprofModules_size;i++) { /* Summarise any collapsed modules */
	print_AVL_summary_info(tprofModules[i].module, 1, fp);
      }

    fprintf(fp, "\n");
    if(listedby==2) {
      if(total_g_enters)
	if(enter_percentages)
	  fprintf(fp, "GreenCard\tEnters = %d (%.1f%%)\tTime = %.1f%%\n\n", total_g_enters, 100*((float) total_g_enters)/total_enters, 100*((float) total_g_ticks)/total_ticks);
	else
	  fprintf(fp, "GreenCard\tEnters = %d\tTime = %.1f%%\n\n", total_g_enters, 100*((float) total_g_ticks)/total_ticks);
      if(enter_percentages)
	fprintf(fp, "GarbageCollect\tEnters = %d (%.1f%%)\tTime = %.1f%%\n\nTime figures based on %d samples\n\n", collect_garbage_enters, 100*((float) collect_garbage_enters)/total_enters, 100*((float) collect_garbage_ticks)/total_ticks, total_ticks);
      else
	fprintf(fp, "GarbageCollect\tEnters = %d\tTime = %.1f%%\n\nTime figures based on %d samples\n\n", collect_garbage_enters, 100*((float) collect_garbage_ticks)/total_ticks, total_ticks);
    }
    else {
      if(enter_percentages) {
	if(total_g_enters)
	  fprintf(fp, "GreenCard\tTime = %.1f%%\tEnters = %d (%.1f%%)\n\n", 100*((float) total_g_ticks)/total_ticks, total_g_enters, 100*((float) total_g_enters)/total_enters);
	fprintf(fp, "GarbageCollect\tTime = %.1f%%\tEnters = %d (%.1f%%)\n\nTime figures based on %d samples\n\n", 100*((float) collect_garbage_ticks)/total_ticks, collect_garbage_enters, 100*((float) collect_garbage_enters)/total_enters, total_ticks);
      }
      else {
	if(total_g_enters)
	  fprintf(fp, "GreenCard\tTime = %.1f%%\tEnters = %d\n\n", 100*((float) total_g_ticks)/total_ticks, total_g_enters);
	fprintf(fp, "GarbageCollect\tTime = %.1f%%\tEnters = %d\n\nTime figures based on %d samples\n\n", 100*((float) collect_garbage_ticks)/total_ticks, collect_garbage_enters, total_ticks);
      }
    }
  }
}

#if 0
void show_AVL_structure(AVLnode **tree, int level, FILE *fp)
{ 
  int i;                             /* V.useful for Phil's debugging, */
                                     /* but could be removed now       */
  if (*tree) {
    show_AVL_structure(&((*tree)->right), level+1, fp);

    for (i=0; i<level; i++)
      fprintf(fp, "  ");
    fprintf(fp, " %s\n", (*tree)->funname);
    
    show_AVL_structure(&((*tree)->left), level+1, fp);
  }
}
#endif

/****************************************************** tprof stuff */

void tprofStart(int argc, char **argv)
{
  int i;
  char fname[MAX_FILE_NAME];
#ifdef __arm
  strcpy(fname, argv[0]);
  strcat(fname, "_tp");
#else
  char *str;
  if(0 == (str = strrchr(argv[0], DIR_DEL))) {
    strcpy(fname, argv[0]);
  } else {
    strcpy(fname, str+1);
  }
  strcat(fname, ".tp");
#endif

  if(0 == (tpFILE = fopen(fname, "w"))) {
    fprintf(stderr, "%s can't open \"%s\" for profile data.\n", argv[0], fname);
    exit(-1);
  }
  fprintf(tpFILE, "JOB ");
  for(i=0; i<argc; ) {
    fputs(argv[i], tpFILE);
    i++;
    fputc(' ', tpFILE);
  }
  fprintf(tpFILE, ";\n");
  { time_t t;
  time(&t);
  fprintf(tpFILE, "DATE %s\n", asctime(localtime(&t)));
  }
  init_AVL(&address_AVL_root);

  if(!timeSample && !profileInterval) {
    timeSample=1;
    profileInterval = (double) 0.02;
  }
  if(timeSample) {
#if defined(__arm) || defined(__CYGWIN32__)
    fprintf(stderr,"No timed profiling availible on this machine.\n");
    exit(-1);
#else
    setuptimer();
#endif
  }
  if (tprofModDefaultsUse[0] && tprofModDefaultsUse[1] && (collapse_mode<2)) {
    tprofModules[tprofModules_size++].module = strdup("System");
    tprofModules[tprofModules_size++].module = strdup("IO");
    combine_System_IO = 1;
  }
  for (i=2*combine_System_IO;i<tprofModDefaults_size; i++) {
    if (tprofModDefaultsUse[i])
      tprofModules[tprofModules_size++].module = tprofModDefaults[i];
  }
  junk_enters = (int*) malloc(sizeof(int*));
  *junk_enters = 0;
}

void tprofInclude(char *module)
{
  int mod_len,i,j,k;
  char *args;

  mod_len = strlen(module);
  args = module;
  for(i=0;i<mod_len;i++) {
    switch (*(args+i)) {
    case 't': {
      listedby = 1;
    } break;
    case 'e': { 
      listedby = 2;
    } break;
    case 'p': { 
      enter_percentages = 1;
    } break;
    case 's': { 
      assign_subfn_enters = 1;
    } break;
    case 'f': { 
      assign_subfn_enters = 2;
    } break;
    case ' ': break;
    case '+': 
    case '-': {
      for(j=i;j<mod_len;j++) {
	if (*(args+j)==' ' || (j==mod_len-1)) {
	  if (j!=mod_len-1) *(args+j)='\0';
	  if (*(args+i)=='-')
	    tprofModules[tprofModules_size++].module = args+i+1;
	  for (k=0; k<tprofModDefaults_size; k++)
	    if (strcmp(tprofModDefaults[k],args+i+1)==0)
	      tprofModDefaultsUse[k] = 0;
	  collapse_mode=1;
	  if (tprofModules_size==MAX_MODULES) {
	    fprintf(stderr,"Array of Module names > MAX_MODULES");
	  }
	  if(strcmp(args+i,"-all")==0) {
	    collapse_mode=2;
	  }
	  if(strcmp(args+i,"+all")==0) {
	    collapse_mode=3;
	  }
	  i=j;
	  break;
	}
      }
    } break;
    default: {
      fprintf(stderr,"\n Unknown Flag.\tValid flags: -t, -tt, -te, -tp, -ttp, etc...\n");
      exit(-1);
    }
    }
  }
}

void tprofStop(void)
{
  timeSample++; /* No more timer exceptions */
  stoptimer();
  output_AVL_as_orderd_table(&address_AVL_root, tpFILE);
  /*  discard_AVL(&address_AVL_root);*/
  fclose(tpFILE);
}

void tprofRecordEnter(CodePtr *address, char *funname, int **enterPtr)
{
  char *funnamecolon;
  char *moduledot;
  int collapse_node_pos;
  int i;

  timeSample = FREEZE_TIME;
  last_right_taken = address_AVL_root;  

  if (**enterPtr==0) {  /*  First enter for this function */
    moduledot = strchr(funname,'.');
    *moduledot = '\0';
    collapse_node_pos = collapse_module(funname);
    *moduledot = '.';
    if(collapse_node_pos) { /* collapsing */
      if(tprofModules[collapse_node_pos-1].already_made_node) { 
	AVLnode *module_node; /* No need for new node */
	module_node = tprofModules[collapse_node_pos-1].node;
	(*(module_node->enters))++;
	penultimate_enter = last_enter;
	last_enter = module_node->enters;
	if (address < module_node->address)
	  module_node->address = address;
	*enterPtr = module_node->enters;
      }
      else { /* Really do need a new node, but store it for later savings */
	enterPtrTmp = enterPtr;
	insert_info_address_AVL(&address_AVL_root, address, funname, 1);
	tprofModules[collapse_node_pos-1].node = (AVLnode*) malloc(sizeof(AVLnode*));
	tprofModules[collapse_node_pos-1].node = last_entered_node;
	tprofModules[collapse_node_pos-1].already_made_node = 1;
      }
    }
    else { /* not collapsing */
      enterPtrTmp = enterPtr;
      funnamecolon = strchr(funname,':');
      if ((assign_subfn_enters==2) || (funnamecolon==NULL)) /* deal with : enters */
	insert_info_address_AVL(&address_AVL_root, address, funname, 1);
      else
	if(assign_subfn_enters)
	  insert_info_address_AVL(&address_AVL_root, address, funname, 3);
	else {
	  *enterPtr = junk_enters;
	  /*	  **enterPtr = -1;*/
	  penultimate_enter = last_enter;
	  last_enter = *enterPtr;
	}
    }
  }
  /*  else {
      if (**enterPtr<0) {  /* not counting enters here at all */
  /*   penultimate_enter = last_enter;
       last_enter = *enterPtr;    
       }*/
  else {  /* no need to search the tree :-) */
    (**enterPtr)++;
    penultimate_enter = last_enter;
    last_enter = *enterPtr;
  }
  timeSample = ACTIVE_TIME;
  return;
}

void tprofUnrecordEnter(void)
{
  timeSample = FREEZE_TIME;
  *last_enter--;
  last_enter = penultimate_enter;
  timeSample = ACTIVE_TIME;
}

void tprofEnterGreencard(CodePtr *address, char *funname)
{
  timeSample = FREEZE_TIME;
  in_greencard = 1;
  insert_info_address_AVL(&address_AVL_root, address, "greencard", 2);
  timeSample = ACTIVE_TIME;
}

void tprofExitGreencard(void)
{
  in_greencard = 0;
}

void tprofRecordGC(void)
{
  timeSample = FREEZE_TIME;
  if (collecting_garbage)
    collecting_garbage=0;
  else {
    collecting_garbage=1;
    collect_garbage_enters++;
  }
  timeSample = ACTIVE_TIME;
}

void tprofRecordTick(CodePtr *address)
{
  timeSample = FREEZE_TIME;
  if (in_greencard) {
    *in_greencard_ticks++;
  }
  else {
    if (collecting_garbage) {
      collect_garbage_ticks++;
    } 
    else {
      last_right_taken = address_AVL_root;
      insert_info_address_AVL(&address_AVL_root, address, "tick", 0);    
    }
  }  
  timeSample = ACTIVE_TIME;
  return;
}
