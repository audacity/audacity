/* -*- Mode: c; c-basic-offset: 2 -*-
 *
 * raptor_avltree.c - Balanced Binary Tree / AVL Tree
 *
 * This file is in the public domain.
 *
 * Based on public domain sources posted to comp.sources.misc in 1993
 *
 * From: p...@vix.com (Paul Vixie)
 * Newsgroups: comp.sources.unix
 * Subject: v27i034: REPOST AVL Tree subroutines (replaces v11i020 from 1987), Part01/01
 * Date: 6 Sep 1993 13:51:22 -0700
 * Message-ID: <1.747348668.4037@gw.home.vix.com>
 * 
 * ----------------------------------------------------------------------
 * Original headers below
 */

/* as_tree - tree library for as
 * vix 14dec85 [written]
 * vix 02feb86 [added tree balancing from wirth "a+ds=p" p. 220-221]
 * vix 06feb86 [added tree_mung()]
 * vix 20jun86 [added tree_delete per wirth a+ds (mod2 v.) p. 224]
 * vix 23jun86 [added delete uar to add for replaced nodes]
 * vix 22jan93 [revisited; uses RCS, ANSI, POSIX; has bug fixes]
 */


/* This program text was created by Paul Vixie using examples from the book:
 * "Algorithms & Data Structures," Niklaus Wirth, Prentice-Hall, 1986, ISBN
 * 0-13-022005-1.  This code and associated documentation is hereby placed
 * in the public domain.
 */


#ifdef HAVE_CONFIG_H
#include <raptor_config.h>
#endif

#ifdef WIN32
#include <win32_raptor_config.h>
#endif

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif


/* Raptor includes */
#include "raptor.h"
#include "raptor_internal.h"


#if RAPTOR_DEBUG > 1
#define RAPTOR_AVLTREE_DEBUG1(msg) RAPTOR_DEBUG1(msg)
#else
#define RAPTOR_AVLTREE_DEBUG1(msg)
#endif


#define RAPTOR_AVLTREE_ENOMEM -1
#define RAPTOR_AVLTREE_EXISTS 1


#ifndef STANDALONE
typedef struct raptor_avltree_node_s raptor_avltree_node;

/* AVL-tree node */
struct raptor_avltree_node_s {
  /* parent tree */
  struct raptor_avltree_node_s *parent;
  
  /* left child tree */
  struct raptor_avltree_node_s *left;

  /* right child tree */
  struct raptor_avltree_node_s *right;

  /* balance factor =
   *   height of the right tree minus the height of the left tree
   * i.e. equal: 0  left larger: -1  right larger: 1
   */
  signed char balance;

  /* actual data */
  void* data;
};


/* AVL-tree */
struct raptor_avltree_s {
  /* root node of tree */
  raptor_avltree_node* root;

  /* node comparison function (optional) */
  raptor_data_compare_function compare_fn;

  /* node deletion function (optional) */
  raptor_data_free_function free_fn;

  /* node print function (optional) */
  raptor_data_print_function print_fn;

  /* tree flags (none defined at present) */
  unsigned int flags;

  /* number of nodes in tree */
  unsigned int size;

  /* legacy iterator used for cursor methods */
  raptor_avltree_iterator* cursor_iterator;
};


#ifndef TRUE
#define	TRUE		1
#define	FALSE		0
#endif


/* local prototypes */
static int raptor_avltree_sprout(raptor_avltree* tree, raptor_avltree_node* parent, raptor_avltree_node** node_pp, void* p_data, int *rebalancing_p);
static void* raptor_avltree_delete_internal(raptor_avltree* tree, raptor_avltree_node** node_pp, void* p_data, int *rebalancing_p);
static void* raptor_avltree_delete_internal2(raptor_avltree* tree, raptor_avltree_node** ppr_r, int *rebalancing_p, raptor_avltree_node** ppr_q);
static void raptor_avltree_balance_left(raptor_avltree* tree, raptor_avltree_node** node_pp, int *rebalancing_p);
static void raptor_avltree_balance_right(raptor_avltree* tree, raptor_avltree_node** node_pp, int *rebalancing_p);
static raptor_avltree_node* raptor_avltree_search_internal(raptor_avltree* tree, raptor_avltree_node* node, const void* p_data);
static int raptor_avltree_visit_internal(raptor_avltree* tree, raptor_avltree_node* node, int depth, raptor_avltree_visit_function visit_fn, void* user_data);
static void raptor_free_avltree_internal(raptor_avltree* tree, raptor_avltree_node* node);



/*
 * raptor_new_avltree:
 * @compare_fn: item comparison function for ordering
 * @free_fn: item free function (or NULL)
 * @flags: AVLTree flags - bitmask of
 *    RAPTOR_AVLTREE_FLAG_REPLACE_DUPLICATES - raptor_avltree_add()
 *    will replace any duplicate items (default is to ignore them and
 *    return >0 on duplicates)
 *
 * INTERNAL - AVL Tree Constructor
 *
 * Return value: new AVL Tree or NULL on failure
 */
raptor_avltree*
raptor_new_avltree(raptor_data_compare_function compare_fn,
                   raptor_data_free_function free_fn,
                   unsigned int flags)
{
  raptor_avltree* tree;
  
  tree=(raptor_avltree*)RAPTOR_MALLOC(raptor_avltree, sizeof(*tree));
  if(!tree)
    return NULL;
  
  tree->root=NULL;
  tree->compare_fn=compare_fn;
  tree->free_fn=free_fn;
  tree->print_fn=NULL;
  tree->flags=flags;
  tree->size=0;
  tree->cursor_iterator=NULL;
  
  return tree;
}


/*
 * raptor_free_avltree:
 * @tree: AVLTree object
 *
 * INTERNAL - AVL Tree destructor
 */
void
raptor_free_avltree(raptor_avltree* tree)
{
  RAPTOR_ASSERT_OBJECT_POINTER_RETURN(tree, raptor_avltree);
  
  raptor_free_avltree_internal(tree, tree->root);

  if(tree->cursor_iterator)
    raptor_free_avltree_iterator(tree->cursor_iterator);

  RAPTOR_FREE(raptor_avltree, tree);
}


static void
raptor_free_avltree_internal(raptor_avltree* tree, raptor_avltree_node* node)
{
  if(node) {
    raptor_free_avltree_internal(tree, node->left);

    raptor_free_avltree_internal(tree, node->right);

    if(tree->free_fn)
      tree->free_fn(node->data);
    tree->size--;
    RAPTOR_FREE(raptor_avltree_node, node);
  }
}


/* methods */

static raptor_avltree_node*
raptor_avltree_search_internal(raptor_avltree* tree, raptor_avltree_node* node,
                               const void* p_data)
{
  if(node) {
    int cmp= tree->compare_fn(p_data, node->data);

    if(cmp > 0)
      return raptor_avltree_search_internal(tree, node->right, p_data);
    else if(cmp < 0)
      return raptor_avltree_search_internal(tree, node->left, p_data);

    /* found */
    return node;
  }

  /* otherwise not found */
  return NULL;
}


/*
 * raptor_avltree_search:
 * @tree: AVL Tree object
 * @p_data: pointer to data item
 *
 * INTERNAL - find an item in an AVL Tree
 *
 * Return value: shared pointer to item (still owned by AVL Tree) or NULL on failure or if not found
 */
void*
raptor_avltree_search(raptor_avltree* tree, const void* p_data)
{
  raptor_avltree_node* node;
  node=raptor_avltree_search_internal(tree, tree->root, p_data);
  return node ? node->data : NULL;
}


/*
 * raptor_avltree_add:
 * @tree: AVL Tree object
 * @p_data: pointer to data item
 *
 * INTERNAL - add an item to an AVL Tree
 *
 * The item added becomes owned by the AVL Tree, and will be freed by
 * the free_fn argument given to raptor_new_avltree().
 *
 * Return value: 0 on success, >0 if equivalent item exists (and the old element remains in the tree), <0 on failure
 */
int
raptor_avltree_add(raptor_avltree* tree, void* p_data)
{
  int rebalancing= FALSE;
  int rv;

  rv=raptor_avltree_sprout(tree, NULL, &tree->root, p_data,
                           &rebalancing);
#if RAPTOR_DEBUG > 1
  raptor_avltree_check(tree);
#endif

  return rv;
}


/*
 * raptor_avltree_remove:
 * @tree: AVL Tree object
 * @p_data: pointer to data item
 *
 * INTERNAL - remove an item from an AVL Tree and return it
 *
 * The item removed is  no longer owned by the AVL Tree and is
 * owned by the caller.
 *
 * Return value: object or NULL on failure or if not found
 */
void*
raptor_avltree_remove(raptor_avltree* tree, void* p_data)
{
  int rebalancing= FALSE;
  void* rdata;
  
  rdata=raptor_avltree_delete_internal(tree, &tree->root, p_data,
                                       &rebalancing);
  if(rdata)
    tree->size--;

#if RAPTOR_DEBUG > 1
  raptor_avltree_check(tree);
#endif
  
  return rdata;
}


/*
 * raptor_avltree_delete:
 * @tree: AVL Tree object
 * @p_data: pointer to data item
 *
 * INTERNAL - remove an item from an AVL Tree and free it
 */
int
raptor_avltree_delete(raptor_avltree* tree, void* p_data)
{
  void* rdata;

  rdata=raptor_avltree_remove(tree, p_data);
  if(rdata) {
    if(tree->free_fn)
      tree->free_fn(rdata);
  }

  return (rdata != NULL);
}


static int
raptor_avltree_visit_internal(raptor_avltree* tree, raptor_avltree_node* node,
                              int depth,
                              raptor_avltree_visit_function visit_fn,
                              void* user_data)
{
  if(!node)
    return TRUE;

  if(!raptor_avltree_visit_internal(tree, node->left, depth+1, 
                                    visit_fn, user_data))
    return FALSE;

  if(!visit_fn(depth, node->data, user_data))
    return FALSE;

  if(!raptor_avltree_visit_internal(tree, node->right, depth+1,
                                    visit_fn, user_data))
    return FALSE;

  return TRUE;
}


/*
 * raptor_avltree_visit:
 * @tree: AVL Tree object
 * @visit_fn: visit function to call at each item
 * @user_data: user data pointer fo visit function
 *
 * INTERNAL - perform an in-order visit of the items in the AVL Tree
 *
 * Return value: non-0 if traversal was terminated early by @visit_fn
*/
int
raptor_avltree_visit(raptor_avltree* tree,
                     raptor_avltree_visit_function visit_fn,
                     void* user_data)
{
  return raptor_avltree_visit_internal(tree, tree->root, 0,
                                       visit_fn, user_data);
}


#ifdef RAPTOR_DEBUG
static void
raptor_avltree_print_node(raptor_avltree_node* node) 
{
  fprintf(stderr, "%p: parent %p  left %p  right %p  data %p",
          node, node->parent, node->left, node->right, node->data);
}


static void
raptor_avltree_check_node(raptor_avltree* tree, raptor_avltree_node* node,
                          const char* fn, const char* where) 
{
  if(node->parent) {
    if((node->parent == node->left) || (node->parent == node->right)) {
      if(fn && where)
        fprintf(stderr, "%s (%s): ", fn, where);
      fputs("ERROR bad node ", stderr);
      raptor_avltree_print_node(node);
      fputc('\n', stderr);
      fflush(stderr);
      abort();
    }
    if(node->parent->left != node && node->parent->right != node) {
      if(fn && where)
        fprintf(stderr, "%s (%s): ", fn, where);
      fputs("ERROR parent node ", stderr);
      raptor_avltree_print_node(node->parent);
      fputs(" has no reference to child node ", stderr);
      raptor_avltree_print_node(node);
      fputc('\n', stderr);
      fflush(stderr);
      abort();
    }
  }
}
#endif


static int
raptor_avltree_sprout_left(raptor_avltree* tree, raptor_avltree_node** node_pp,
                           void* p_data, int *rebalancing_p)
{
  raptor_avltree_node *p1, *p2, *p_parent;
  int rc;

  RAPTOR_AVLTREE_DEBUG1("LESS. raptor_avltree_sprouting left.\n");

  p_parent=(*node_pp)->parent;
  
  rc=raptor_avltree_sprout(tree, *node_pp, &(*node_pp)->left, p_data,
                           rebalancing_p);
  if(rc)
    return rc;

  if(!*rebalancing_p)
    return FALSE;

  /* left branch has grown longer */
  RAPTOR_AVLTREE_DEBUG1("LESS: left branch has grown\n");
  switch((*node_pp)->balance) {
    case 1:
      /* right branch WAS longer; balance is ok now */
      RAPTOR_AVLTREE_DEBUG1("LESS: case 1.. balance restored implicitly\n");
      (*node_pp)->balance= 0;
      *rebalancing_p= FALSE;
      break;

    case 0:
      /* balance WAS okay; now left branch longer */
      RAPTOR_AVLTREE_DEBUG1("LESS: case 0.. balance bad but still ok\n");
      (*node_pp)->balance= -1;
      break;

    case -1:
      /* left branch was already too long. rebalance */
      RAPTOR_AVLTREE_DEBUG1("LESS: case -1: rebalancing\n");
      p1= (*node_pp)->left;

      if(p1->balance == -1) {
        /* LL */
        RAPTOR_AVLTREE_DEBUG1("LESS: single LL\n");
        (*node_pp)->left= p1->right;
        if((*node_pp)->left)
          (*node_pp)->left->parent=(*node_pp);
        p1->right = *node_pp;
        if(p1->right)
          p1->right->parent=p1;
        (*node_pp)->balance= 0;
        *node_pp= p1;
        (*node_pp)->parent=p_parent;
      } else {
        /* double LR */
        RAPTOR_AVLTREE_DEBUG1("LESS: double LR\n");
        p2= p1->right;
        p1->right= p2->left;
        if(p1->right)
          p1->right->parent=p1;
        p2->left= p1;
        if(p2->left)
          p2->left->parent=p2;

        (*node_pp)->left= p2->right;
        if((*node_pp)->left)
          (*node_pp)->left->parent= (*node_pp);
        p2->right= *node_pp;
        if(p2->right)
          p2->right->parent=p2;

        if(p2->balance == -1)
          (*node_pp)->balance= 1;
        else
          (*node_pp)->balance= 0;

        if(p2->balance == 1)
          p1->balance= -1;
        else
          p1->balance= 0;
        *node_pp = p2;
        (*node_pp)->parent=p_parent;
      } /* end else */

      (*node_pp)->balance= 0;
      *rebalancing_p= FALSE;
  } /* end switch */

  return FALSE;
}


static int
raptor_avltree_sprout_right(raptor_avltree* tree,
                            raptor_avltree_node** node_pp, 
                            void* p_data, int *rebalancing_p)
{
  raptor_avltree_node *p1, *p2, *p_parent;
  int rc;

  RAPTOR_AVLTREE_DEBUG1("MORE: raptor_avltree_sprouting to the right\n");

  p_parent=(*node_pp)->parent;
  
  rc=raptor_avltree_sprout(tree, *node_pp, &(*node_pp)->right, p_data,
                           rebalancing_p);
  if(rc)
    return rc;

  if(!*rebalancing_p)
    return FALSE;
  
  /* right branch has grown longer */
  RAPTOR_AVLTREE_DEBUG1("MORE: right branch has grown\n");
  
  switch((*node_pp)->balance) {
    case -1:
      RAPTOR_AVLTREE_DEBUG1("MORE: balance was off, fixed implicitly\n");
      (*node_pp)->balance= 0;
      *rebalancing_p= FALSE;
      break;
      
    case 0:
      RAPTOR_AVLTREE_DEBUG1("MORE: balance was okay, now off but ok\n");
      (*node_pp)->balance= 1;
      break;
      
    case 1:
      RAPTOR_AVLTREE_DEBUG1("MORE: balance was off, need to rebalance\n");
      p1= (*node_pp)->right;
      
      if(p1->balance == 1) {
        /* RR */
        RAPTOR_AVLTREE_DEBUG1("MORE: single RR\n");
        (*node_pp)->right= p1->left;
        if((*node_pp)->right)
          (*node_pp)->right->parent= (*node_pp);
        p1->left= *node_pp;
        if(p1->left)
          p1->left->parent= p1;
        (*node_pp)->balance= 0;
        *node_pp= p1;
        (*node_pp)->parent=p_parent;
      } else {
        /* double RL */
        RAPTOR_AVLTREE_DEBUG1("MORE: double RL\n");
        
        p2= p1->left;
        p1->left= p2->right;
        if(p1->left)
          p1->left->parent=p1;
        p2->right= p1;
        if(p2->right)
          p2->right->parent=p2;
        
        (*node_pp)->right= p2->left;
        if((*node_pp)->right)
          (*node_pp)->right->parent= (*node_pp);
        p2->left= *node_pp;
        if(p2->left)
          p2->left->parent=p2;
        
        if(p2->balance == 1)
          (*node_pp)->balance= -1;
        else
          (*node_pp)->balance= 0;
        
        if(p2->balance == -1)
          p1->balance= 1;
        else
          p1->balance= 0;
        
        *node_pp= p2;
        (*node_pp)->parent=p_parent;
      } /* end else */
      
      (*node_pp)->balance= 0;
      *rebalancing_p= FALSE;
  } /* end switch */

  return FALSE;
}


/* grow a tree by sprouting with a new node
 *
 * Return values:
 *   0 on success
 *   >0 if equivalent item exists (and the old element remains in the tree)
 *   <0 if memory is exhausted.
 */
static int
raptor_avltree_sprout(raptor_avltree* tree, raptor_avltree_node* parent,
                      raptor_avltree_node** node_pp, void* p_data,
                      int *rebalancing_p)
{
  int cmp;

  RAPTOR_AVLTREE_DEBUG1("Enter\n");

  /* If grounded, add the node here, set the rebalance flag and return */
  if(!*node_pp) {
    RAPTOR_AVLTREE_DEBUG1("grounded. adding new node, setting rebalancing flag true\n");
    *node_pp= (raptor_avltree_node*)RAPTOR_MALLOC(raptor_avltree_node, sizeof(**node_pp));
    if(!*node_pp)
      return RAPTOR_AVLTREE_ENOMEM;
    
    (*node_pp)->parent= parent;
    (*node_pp)->left= NULL;
    (*node_pp)->right= NULL;
    (*node_pp)->balance= 0;
    (*node_pp)->data= p_data;
    *rebalancing_p= TRUE;

    tree->size++;
    
    return FALSE;
  }

  /* compare the data */
  cmp= tree->compare_fn(p_data, (*node_pp)->data);
  if(cmp < 0)
    /* if LESS, prepare to move to the left. */
    return raptor_avltree_sprout_left(tree, node_pp, p_data, rebalancing_p);
  else if(cmp > 0)
    /* if MORE, prepare to move to the right. */
    return raptor_avltree_sprout_right(tree, node_pp, p_data, rebalancing_p);

  /* otherwise equivalent key */
  *rebalancing_p= FALSE;

  if(tree->flags & RAPTOR_AVLTREE_FLAG_REPLACE_DUPLICATES) {
    /* replace item with equivalent key */
    if(tree->free_fn)
      tree->free_fn((*node_pp)->data);
    (*node_pp)->data= p_data;

    return FALSE;
  } else {
    /* ignore item with equivalent key */
    tree->free_fn(p_data);
    return RAPTOR_AVLTREE_EXISTS;
  }
}


static void*
raptor_avltree_delete_internal(raptor_avltree* tree,
                               raptor_avltree_node** node_pp,
                               void* p_data,
                               int *rebalancing_p)
{
  int cmp;
  void* rdata=NULL;

  RAPTOR_AVLTREE_DEBUG1("Enter\n");

  if(*node_pp == NULL) {
    RAPTOR_AVLTREE_DEBUG1("key not in tree\n");
    return rdata;
  }

  cmp= tree->compare_fn((*node_pp)->data, p_data);

  if(cmp > 0) {
    RAPTOR_AVLTREE_DEBUG1("too high - scan left\n");
    rdata= raptor_avltree_delete_internal(tree, &(*node_pp)->left, p_data,
                                          rebalancing_p);
    if(*rebalancing_p)
      raptor_avltree_balance_left(tree, node_pp, rebalancing_p);

  } else if(cmp < 0) {
    RAPTOR_AVLTREE_DEBUG1("too low - scan right\n");
    rdata= raptor_avltree_delete_internal(tree, &(*node_pp)->right, p_data,
                                          rebalancing_p);
    if(*rebalancing_p)
      raptor_avltree_balance_right(tree, node_pp, rebalancing_p);

  } else {
    raptor_avltree_node *pr_q;

    RAPTOR_AVLTREE_DEBUG1("equal\n");
    pr_q= *node_pp;
    
    rdata=pr_q->data;
    
    if(pr_q->right == NULL) {
      RAPTOR_AVLTREE_DEBUG1("right subtree null\n");
      *node_pp= pr_q->left;
      *rebalancing_p= TRUE;
    } else if(pr_q->left == NULL) {
      RAPTOR_AVLTREE_DEBUG1("right subtree non-null, left subtree null\n");
      *node_pp= pr_q->right;
      *rebalancing_p= TRUE;
    } else {
      RAPTOR_AVLTREE_DEBUG1("neither subtree null\n");
      rdata=raptor_avltree_delete_internal2(tree, &pr_q->left, rebalancing_p,
                                            &pr_q);
      if(*rebalancing_p)
        raptor_avltree_balance_left(tree, node_pp, rebalancing_p);
    }

    RAPTOR_FREE(raptor_avltree_node, pr_q);
  }

  return rdata;
}


static void*
raptor_avltree_delete_internal2(raptor_avltree* tree,
                                raptor_avltree_node** ppr_r,
                                int *rebalancing_p,
                                raptor_avltree_node** ppr_q)
{
  void* rdata=NULL;
  
  RAPTOR_AVLTREE_DEBUG1("Enter\n");

  if((*ppr_r)->right != NULL) {
    rdata=raptor_avltree_delete_internal2(tree,
                                          &(*ppr_r)->right, 
                                          rebalancing_p,
                                          ppr_q);
    if(*rebalancing_p)
      raptor_avltree_balance_right(tree, ppr_r, rebalancing_p);

  } else {
    rdata=(*ppr_q)->data;

    (*ppr_q)->data= (*ppr_r)->data;
    *ppr_q= *ppr_r;
    *ppr_r= (*ppr_r)->left;
    *rebalancing_p= TRUE;
  }

  return rdata;
}


static void
raptor_avltree_balance_left(raptor_avltree* tree,
                            raptor_avltree_node** node_pp, int *rebalancing_p)
{
  raptor_avltree_node *p1, *p2, *p_parent;
  int b1, b2;

  RAPTOR_AVLTREE_DEBUG1("left branch has shrunk\n");

  p_parent=(*node_pp)->parent;
  
  switch((*node_pp)->balance) {
    case -1:
      RAPTOR_AVLTREE_DEBUG1("was imbalanced, fixed implicitly\n");
      (*node_pp)->balance= 0;
      break;

    case 0:
      RAPTOR_AVLTREE_DEBUG1("was okay, is now one off\n");
      (*node_pp)->balance= 1;
      *rebalancing_p= FALSE;
      break;

    case 1:
      RAPTOR_AVLTREE_DEBUG1("was already off, this is too much\n");
      p1= (*node_pp)->right;
      b1= p1->balance;

      if(b1 >= 0) {
        RAPTOR_AVLTREE_DEBUG1("single RR\n");
        (*node_pp)->right= p1->left;
        if((*node_pp)->right)
          (*node_pp)->right->parent= (*node_pp);
        p1->left= *node_pp;
        if(p1->left)
          p1->left->parent= p1;
        if(b1 == 0) {
          RAPTOR_AVLTREE_DEBUG1("b1 == 0\n");
          (*node_pp)->balance= 1;
          p1->balance= -1;
          *rebalancing_p= FALSE;
        } else {
          RAPTOR_AVLTREE_DEBUG1("b1 != 0\n");
          (*node_pp)->balance= 0;
          p1->balance= 0;
        }
        *node_pp= p1;
        (*node_pp)->parent=p_parent;
      } else {
        RAPTOR_AVLTREE_DEBUG1("double RL\n");
        p2= p1->left;
        b2= p2->balance;
        p1->left= p2->right;
        if(p1->left)
          p1->left->parent=p1;
        p2->right= p1;
        if(p2->right)
          p2->right->parent=p2;
        (*node_pp)->right= p2->left;
        if((*node_pp)->right)
          (*node_pp)->right->parent= (*node_pp);
        p2->left= *node_pp;
        if(p2->left)
          p2->left->parent= p2;
        if(b2 == 1)
          (*node_pp)->balance= -1;
        else
          (*node_pp)->balance= 0;
        if(b2 == -1)
          p1->balance= 1;
        else
          p1->balance= 0;
        *node_pp= p2;
        (*node_pp)->parent=p_parent;
        p2->balance= 0;
      }
      break;
  } /* end switch */

}


static void
raptor_avltree_balance_right(raptor_avltree* tree,
                             raptor_avltree_node** node_pp, int *rebalancing_p)
{
  raptor_avltree_node *p1, *p2, *p_parent;
  int b1, b2;

  RAPTOR_AVLTREE_DEBUG1("right branch has shrunk\n");

  p_parent=(*node_pp)->parent;

  switch((*node_pp)->balance) {
    case 1:
      RAPTOR_AVLTREE_DEBUG1("was imbalanced, fixed implicitly\n");
      (*node_pp)->balance= 0;
      break;

    case 0:
      RAPTOR_AVLTREE_DEBUG1("was okay, is now one off\n");
      (*node_pp)->balance= -1;
      *rebalancing_p= FALSE;
      break;

    case -1:
      RAPTOR_AVLTREE_DEBUG1("was already off, this is too much\n");
      p1= (*node_pp)->left;
      b1= p1->balance;

      if(b1 <= 0) {
        RAPTOR_AVLTREE_DEBUG1("single LL\n");
        (*node_pp)->left= p1->right;
        if((*node_pp)->left)
          (*node_pp)->left->parent= (*node_pp);
        p1->right= *node_pp;
        if(p1->right)
          p1->right->parent= p1;
        if(b1 == 0) {
          RAPTOR_AVLTREE_DEBUG1("b1 == 0\n");
          (*node_pp)->balance= -1;
          p1->balance= 1;
          *rebalancing_p= FALSE;
        } else {
          RAPTOR_AVLTREE_DEBUG1("b1 != 0\n");
          (*node_pp)->balance= 0;
          p1->balance= 0;
        }
        *node_pp= p1;
        (*node_pp)->parent=p_parent;
      } else {
        RAPTOR_AVLTREE_DEBUG1("double LR\n");
        p2= p1->right;
        b2= p2->balance;
        p1->right= p2->left;
        if(p1->right)
          p1->right->parent= p1;
        p2->left= p1;
        if(p2->left)
          p2->left->parent= p2;
        (*node_pp)->left= p2->right;
        if((*node_pp)->left)
          (*node_pp)->left->parent= (*node_pp);
        p2->right= *node_pp;
        if(p2->right)
          p2->right->parent= p2;
        if(b2 == -1)
          (*node_pp)->balance= 1;
        else
          (*node_pp)->balance= 0;
        if(b2 == 1)
          p1->balance= -1;
        else
          p1->balance= 0;
        *node_pp= p2;
        (*node_pp)->parent=p_parent;
        p2->balance= 0;
      }
  } /* end switch */

}


/*
 * raptor_avltree_size:
 * @tree: AVL Tree object
 *
 * INTERNAL - Get the number of items in the AVL Tree
 *
 * Return value: number of items in tree
 */
int
raptor_avltree_size(raptor_avltree* tree)
{
  return tree->size;
}


/*
 * raptor_avltree_set_print_handler:
 * @tree: AVL Tree object
 * @print_fn: print function
 *
 * INTERNAL - set the handler for printing an item in a tree
 *
 */
void
raptor_avltree_set_print_handler(raptor_avltree* tree,
                                 raptor_data_print_function print_fn)
{
  tree->print_fn=print_fn;
}


/* Follow left children until a match for range is found (if range not NULL) */
static raptor_avltree_node*
raptor_avltree_node_leftmost(raptor_avltree* tree, raptor_avltree_node* node,
                             void* range)
{
  /*assert(node);
  assert(!range || tree->compare_fn(range, node->data) == 0);*/
  if(range)
    while(node && node->left && 
          tree->compare_fn(range, node->left->data) == 0)
      node=node->left;
  else
    while(node && node->left)
      node=node->left;
  
  return node;
}


static raptor_avltree_node*
raptor_avltree_node_rightmost(raptor_avltree* tree, raptor_avltree_node* node,
                              void* range)
{
  /*assert(node);
  assert(!range || tree->compare_fn(range, node->data) == 0);*/
  if(range)
    while(node && node->right && 
          tree->compare_fn(range, node->right->data) == 0)
      node=node->right;
  else
    while(node && node->right)
      node=node->right;
  return node;
}


/* Follow right children until a match for range is found (range required) */
static raptor_avltree_node*
raptor_avltree_node_search_right(raptor_avltree* tree,
                                 raptor_avltree_node* node, void* range)
{
  raptor_avltree_node* result;
  
  if(node == NULL)
    return NULL;

  result=node->right;
  while(result) {
    if(tree->compare_fn(range, result->data) == 0) {
      return result;
    } else {
      result = result->right;
    }
  }

  return node;
}


/* Follow left children until a match for range is found (range required) */
static raptor_avltree_node*
raptor_avltree_node_search_left(raptor_avltree* tree,
                                raptor_avltree_node* node, void* range)
{
  raptor_avltree_node* result;
  
  if(node == NULL)
    return NULL;

  result=node->left;
  while(result) {
    if(tree->compare_fn(range, result->data) == 0) {
      return result;
    } else {
      result = result->left;
    }
  }

  return node;
}


static raptor_avltree_node*
raptor_avltree_node_prev(raptor_avltree* tree, raptor_avltree_node* node,
                         void* range)
{
  int up=0;

  /*assert(!range || tree->compare_fn(range, node->data) == 0);*/

  if(node->left) {
    /* Should never go left if the current node is already < range */
    raptor_avltree_node* prev;
    prev=raptor_avltree_node_rightmost(tree, node->left, NULL);
    /*assert(!range ||tree->compare_fn(range, node->data) <= 0);*/
    if(range) {
      if(tree->compare_fn(range, prev->data) == 0) {
        up = 0;
        node = prev;
      } else {
        up = 1;
      }
    } else {
      node = prev;
      up = 0;
    }
  } else {
    up = 1;
  }
  
  if(up) {
    raptor_avltree_node* last=node;
    /* Need to go up */
    node=node->parent;
    while(node) {
    
      /* moving from right subtree to this node */
      if(node->right && last == node->right) {
        break;
      }
      
      /* moved up to find an unvisited left subtree */
      if(node->left && last != node->left) {
        /* Should never go left if the current node is already > range */
        /*assert(!range ||tree->compare_fn(range, node->data) <= 0);*/
        node=raptor_avltree_node_rightmost(tree, node->left, range);
        break;
      }
      last=node;
      node=node->parent;
    }
  }

  if (node && range) {
    if (tree->compare_fn(range, node->data) == 0)
      return node;
    else
      return NULL;
  } else {
    return node;
  }
}


/* Follow right children until a match for range is found (if range not NULL) */
static raptor_avltree_node*
raptor_avltree_node_next(raptor_avltree* tree, raptor_avltree_node* node,
                         void* range)
{
  int up=0;

  /*assert(!range || tree->compare_fn(range, node->data) == 0);*/

  if(node->right) {
    /* Should never go right if the current node is already > range */
    raptor_avltree_node* next;
    next=raptor_avltree_node_leftmost(tree, node->right, NULL);
    /*assert(!range ||tree->compare_fn(range, node->data) <= 0);*/
    if(range) {
      if(tree->compare_fn(range, next->data) == 0) {
        up = 0;
        node = next;
      } else {
        up = 1;
      }
    } else {
      node = next;
      up = 0;
    }
  } else {
    up = 1;
  }
  
  if(up) {
    raptor_avltree_node* last=node;
    /* Need to go up */
    node=node->parent;
    while(node) {
    
      /* moving from left subtree to this node */
      if(node->left && last == node->left) {
        break;
      }
      
      /* moved up to find an unvisited right subtree */
      if(node->right && last != node->right) {
        /* Should never go right if the current node is already > range */
        /*assert(!range ||tree->compare_fn(range, node->data) <= 0);*/
        node=raptor_avltree_node_leftmost(tree, node->right, range);
        break;
      }
      last=node;
      node=node->parent;
    }
  }

  if (node && range) {
    if (tree->compare_fn(range, node->data) == 0)
      return node;
    else
      return NULL;
  } else {
    return node;
  }
}


struct raptor_avltree_iterator_s {
  raptor_avltree* tree;
  raptor_avltree_node* root;
  raptor_avltree_node* current;
  void* range;
  raptor_data_free_function range_free_fn;
  int direction;
  int is_finished;
};


/*
 * raptor_new_avltree_iterator:
 * @tree: #raptor_avltree object
 * @range: range
 * @range_free_fn: function to free @range object
 * @direction: <0 to go 'backwards' otherwise 'forwards'
 *
 * INTERNAL - Get an in-order iterator for the start of a range, or the entire contents
 *
 * If range is NULL, the entire tree is walked in order.  If range
 * specifies a range (i.e. the tree comparison function will 'match'
 * (return 0 for) range and /several/ nodes), the iterator will be
 * placed at the leftmost child matching range, and
 * raptor_avltree_iterator_next will iterate over all nodes (and only
 * nodes) that match range.
 * 
 * Return value: a new #raptor_avltree_iterator object or NULL on failure
 **/
raptor_avltree_iterator*
raptor_new_avltree_iterator(raptor_avltree* tree, void* range,
                            raptor_data_free_function range_free_fn,
                            int direction)
{
  raptor_avltree_iterator* iterator;

  iterator=(raptor_avltree_iterator*)RAPTOR_CALLOC(raptor_avltree_iterator,
                                                   1, sizeof(raptor_avltree_iterator));
  if(!iterator)
    return NULL;

  iterator->is_finished=0;
  iterator->current=NULL;

  iterator->tree = tree;
  iterator->range = range;
  iterator->range_free_fn = range_free_fn;
  iterator->direction = direction;

  if(range) {
    /* find the topmost match (range is contained entirely in tree
     * rooted here) 
     */
    iterator->current=raptor_avltree_search_internal(tree, tree->root, range);
  } else {
    iterator->current=tree->root;
  }

  iterator->root = iterator->current;
  
  if(iterator->current) {
    if(iterator->direction < 0) {
      /* go down to find END of range (or tree) */
      while (1) {
        raptor_avltree_node* pred;
        iterator->current=raptor_avltree_node_rightmost(tree, iterator->current,
                                                       range);
        /* move left until a match is found */
        pred=raptor_avltree_node_search_left(tree, iterator->current->right,
                                             range);
        
        if(pred && tree->compare_fn(range, pred->data) == 0)
          iterator->current = pred;
        else
          break;
      }
    } else {
      /* go down to find START of range (or tree) */
      while (1) {
        raptor_avltree_node* pred;
        iterator->current=raptor_avltree_node_leftmost(tree, iterator->current,
                                                       range);
        /* move right until a match is found */
        pred=raptor_avltree_node_search_right(tree, iterator->current->left,
                                              range);
        
        if(pred && tree->compare_fn(range, pred->data) == 0)
          iterator->current = pred;
        else
          break;
      }
    }
  }

  return iterator;
}


/*
 * raptor_free_avltree_iterator:
 * @iterator: AVL Tree iterator object
 *
 * INTERNAL - AVL Tree Iterator destructor
 */
void
raptor_free_avltree_iterator(raptor_avltree_iterator* iterator) 
{
  if(!iterator)
    return;
  
  if(iterator->range && iterator->range_free_fn)
    iterator->range_free_fn(iterator->range);

  RAPTOR_FREE(raptor_avltree_iterator, iterator);
}


/*
 * raptor_avltree_iterator_end:
 * @iterator: AVL Tree iterator object
 *
 * INTERNAL - test if an iteration is finished
 *
 * Return value: non-0 if iteration is finished
 */
int
raptor_avltree_iterator_end(raptor_avltree_iterator* iterator)
{
  raptor_avltree_node *node=iterator->current;
  
  if(iterator->is_finished)
    return 1;
  iterator->is_finished=(node == NULL);

  return iterator->is_finished;
}


/*
 * raptor_avltree_iterator_next:
 * @iterator: AVL Tree iterator object
 *
 * INTERNAL - move iteration to next/prev object
 *
 * Return value: non-0 if iteration is finished
 */
int
raptor_avltree_iterator_next(raptor_avltree_iterator* iterator)
{
  raptor_avltree_node *node=iterator->current;
  
  if(!node || iterator->is_finished)
    return 1;
  
  if(iterator->direction < 0)
    iterator->current=raptor_avltree_node_prev(iterator->tree, node,
                                               iterator->range);
  else
    iterator->current=raptor_avltree_node_next(iterator->tree, node,
                                               iterator->range);
  /* Stay within rooted subtree */
  if (iterator->root->parent == iterator->current)
    iterator->current = NULL;

  iterator->is_finished=(iterator->current == NULL);

  return iterator->is_finished;
}


/*
 * raptor_avltree_iterator_get:
 * @iterator: AVL Tree iterator object
 *
 * INTERNAL - get current iteration object
 *
 * Return value: object or NULL if iteration is finished
 */
void*
raptor_avltree_iterator_get(raptor_avltree_iterator* iterator)
{
  raptor_avltree_node *node=iterator->current;

  if(iterator->is_finished)
    return NULL;

  iterator->is_finished=(node == NULL);
  if(iterator->is_finished)
    return NULL;
  
  return node->data;
}


/* move the tree cursor to the first item by order */
int
raptor_avltree_cursor_first(raptor_avltree* tree)
{
  if(tree->cursor_iterator) {
    raptor_free_avltree_iterator(tree->cursor_iterator);
    tree->cursor_iterator=NULL;
  }

  if(!tree->size)
    return 1;
  
  tree->cursor_iterator=raptor_new_avltree_iterator(tree, NULL, NULL, 1);
  return (tree->cursor_iterator == NULL);
}


/* move the tree cursor to the last item by order */
int
raptor_avltree_cursor_last(raptor_avltree* tree)
{
  if(tree->cursor_iterator) {
    raptor_free_avltree_iterator(tree->cursor_iterator);
    tree->cursor_iterator=NULL;
  }
  
  if(!tree->size)
    return 1;
  
  tree->cursor_iterator=raptor_new_avltree_iterator(tree, NULL, NULL, -1);
  return (tree->cursor_iterator == NULL);
}


/* move the tree cursor to the previous item by order */
int
raptor_avltree_cursor_prev(raptor_avltree* tree)
{
  int rc;
  if(!tree->cursor_iterator)
    rc=raptor_avltree_cursor_last(tree);
  else
    rc=raptor_avltree_iterator_next(tree->cursor_iterator);
  return rc;
}


/* move the tree cursor to the next item by order */
int
raptor_avltree_cursor_next(raptor_avltree* tree)
{
  int rc;
  if(!tree->cursor_iterator)
    rc=raptor_avltree_cursor_first(tree);
  else
    rc=raptor_avltree_iterator_next(tree->cursor_iterator);
  return rc;
}


/* get the item at the tree cursor (or NULL if no cursor was set) */
void*
raptor_avltree_cursor_get(raptor_avltree* tree)
{
  if(tree->cursor_iterator)
    return raptor_avltree_iterator_get(tree->cursor_iterator);
  return NULL;
}


/* print the items in the tree in order (for debugging) */
void
raptor_avltree_print(raptor_avltree* tree, FILE* stream)
{
  int i;
  int rv=0;
  raptor_avltree_iterator* iter;

  fprintf(stream, "AVL Tree size %u\n", tree->size);
  for(i=0, (iter=raptor_new_avltree_iterator(tree, NULL, NULL, 1));
      iter && !rv;
      i++, (rv=raptor_avltree_iterator_next(iter))) {
    const void* data=raptor_avltree_iterator_get(iter);
    if(!data)
      continue;
    fprintf(stream, "%d) ", i);
    if(tree->print_fn)
      tree->print_fn(stream, data);
    else
      fprintf(stream, "Data Node %p\n", data);
  }
  /*assert(i == tree->size);*/
}


#ifdef RAPTOR_DEBUG

static int
raptor_avltree_dump_internal(raptor_avltree* tree, raptor_avltree_node* node,
                             int depth, FILE* stream)
{
  int i;
  if(!node)
    return TRUE;

  for(i=0; i < depth; i++)
    fputs("  ", stream);
  fprintf(stream, "Node %p: parent %p  left %p  right %p  data %p\n",
          node, node->parent, node->left, node->right, node->data);
  if(tree->print_fn) {
    for(i= 0; i < depth; i++)
      fputs("  ", stream);
    tree->print_fn(stream, node->data);
  }
  
  if(!raptor_avltree_dump_internal(tree, node->left, depth+1, stream))
    return FALSE;

  if(!raptor_avltree_dump_internal(tree, node->right, depth+1, stream))
    return FALSE;

  return TRUE;
}


/* debugging tree dump with pointers and depth indenting */
int
raptor_avltree_dump(raptor_avltree* tree, FILE* stream)
{
  fprintf(stream, "Dumping avltree %p size %u\n", tree, tree->size);
  return raptor_avltree_dump_internal(tree, tree->root, 0, stream);
}


static void
raptor_avltree_check_internal(raptor_avltree* tree, raptor_avltree_node* node,
                              unsigned int* count_p)
{
  if(!node)
    return;
  (*count_p)++;
  
  raptor_avltree_check_node(tree, node, NULL, NULL);

  raptor_avltree_check_internal(tree, node->left, count_p);

  raptor_avltree_check_internal(tree, node->right, count_p);
}


/* debugging tree check - parent/child pointers and counts */
void
raptor_avltree_check(raptor_avltree* tree)
{
  unsigned int count=0;
  
  raptor_avltree_check_internal(tree, tree->root, &count);
  if(count != tree->size) {
    fprintf(stderr, "Tree %p nodes count is %u.  actual count %d\n",
            tree, tree->size, count);
    abort();
  }
}

#endif

#endif


#ifdef STANDALONE

#include <string.h>

typedef struct 
{
  FILE *fh;
  int count;
  const char** results;
  int failed;
} visit_state;
  
#if RAPTOR_DEBUG > 1
static int
print_string(int depth, void* data, void *user_data) 
{
  visit_state* vs=(visit_state*)user_data;
  
  fprintf(vs->fh, "%3d: %s\n", vs->count, (char*) data);
  vs->count++;
  return 1;
}
#endif

static int
check_string(int depth, void* data, void *user_data) 
{
  visit_state* vs=(visit_state*)user_data;
  const char* result=vs->results[vs->count];
  
  if(strcmp((const char*)data, result)) {
    fprintf(vs->fh, "%3d: Expected '%s' but found '%s'\n", vs->count,
            result, (char*)data);
    vs->failed=1;
  }
  vs->count++;
  
  return 1;
}

static int
compare_strings(const void *l, const void *r)
{
  return strcmp((const char*)l, (const char*)r);
}


/* one more prototype */
int main(int argc, char *argv[]);

int
main(int argc, char *argv[])
{
  const char *program=raptor_basename(argv[0]);
#define ITEM_COUNT 8
  const char *items[ITEM_COUNT+1] = { "ron", "amy", "jen", "bij", "jib", "daj", "jim", "def", NULL };
#define DELETE_COUNT 2
  const char *delete_items[DELETE_COUNT+1] = { "jen", "jim", NULL };
#define RESULT_COUNT (ITEM_COUNT-DELETE_COUNT)
  const char *results[RESULT_COUNT+1] = { "amy", "bij", "daj", "def", "jib", "ron", NULL};

  raptor_avltree* tree;
  raptor_avltree_iterator* iter;
  visit_state vs;
  int i;
  
  tree=raptor_new_avltree(compare_strings,
                          NULL, /* no free as they are static pointers above */
                          0);
  if(!tree) {
    fprintf(stderr, "%s: Failed to create tree\n", program);
    exit(1);
  }
  for(i=0; items[i]; i++) {
    int rc;
    void* node;

#if RAPTOR_DEBUG > 1
    fprintf(stderr, "%s: Adding tree item '%s'\n", program, items[i]);
#endif
  
    rc=raptor_avltree_add(tree, (void*)items[i]);
    if(rc) {
      fprintf(stderr,
              "%s: Adding tree item %d '%s' failed, returning error %d\n",
              program, i, items[i], rc);
      exit(1);
    }

#ifdef RAPTOR_DEBUG
    raptor_avltree_check(tree);
#endif

    node=raptor_avltree_search(tree, (void*)items[i]);
    if(!node) {
      fprintf(stderr,
              "%s: Tree did NOT contain item %d '%s' as expected\n",
              program, i, items[i]);
      exit(1);
    }
  }

#if RAPTOR_DEBUG > 1
  fprintf(stderr, "%s: Printing tree\n", program);
  vs.fh=stderr;
  vs.count=0;
  raptor_avltree_visit(tree, print_string, &vs);

  fprintf(stderr, "%s: Dumping tree\n", program);
  raptor_avltree_dump(tree, stderr);
#endif



  for(i=0; delete_items[i]; i++) {
    int rc;

#if RAPTOR_DEBUG > 1
    fprintf(stderr, "%s: Deleting tree item '%s'\n", program, delete_items[i]);
#endif
  
    rc=raptor_avltree_delete(tree, (void*)delete_items[i]);
    if(!rc) {
      fprintf(stderr,
              "%s: Deleting tree item %d '%s' failed, returning error %d\n",
              program, i, delete_items[i], rc);
      exit(1);
    }

#ifdef RAPTOR_DEBUG
    raptor_avltree_check(tree);
#endif
  }


#if RAPTOR_DEBUG > 1
  fprintf(stderr, "%s: Walking tree forwards via iterator\n", program);
#endif
  iter=raptor_new_avltree_iterator(tree, NULL, NULL, 1);
  for(i=0; 1; i++) {
    const char* data=(const char*)raptor_avltree_iterator_get(iter);
    const char* result=results[i];
    if((!data && data != result) || (data && strcmp(data, result))) {
      fprintf(stderr, "%3d: Forwards iterator expected '%s' but found '%s'\n",
              i, result, data);
      exit(1);
    }
#if RAPTOR_DEBUG > 1
    fprintf(stderr, "%3d: Got '%s'\n", i, data);
#endif
    if(raptor_avltree_iterator_next(iter))
      break;
    if(i > RESULT_COUNT) {
      fprintf(stderr, "Forward iterator did not end on result %i as expected\n", i);
      exit(1);
    }
  }
  raptor_free_avltree_iterator(iter);


#if RAPTOR_DEBUG > 1
  fprintf(stderr, "%s: Walking tree backwards via cursor\n", program);
#endif
  raptor_avltree_cursor_last(tree);
  for(i=RESULT_COUNT-1; 1; i--) {
    const char* data=(const char*)raptor_avltree_cursor_get(tree);
    const char* result=results[i];
    if((!data && data != result) || (data && strcmp(data, result))) {
      fprintf(stderr, "%3d: Backwards cursoring expected '%s' but found '%s'\n",
              i, result, data);
      exit(1);
    }
#if RAPTOR_DEBUG > 1
    fprintf(stderr, "%3d: Got '%s'\n", i, data);
#endif
    if(raptor_avltree_cursor_prev(tree))
      break;
    if(i < 0) {
      fprintf(stderr, "Backwards cursor did not end on result %i as expected\n", i);
      exit(1);
    }
  }


#if RAPTOR_DEBUG > 1
  fprintf(stderr, "%s: Checking tree\n", program);
#endif
  vs.count=0;
  vs.results=results;
  vs.failed=0;
  raptor_avltree_visit(tree, check_string, &vs);
  if(vs.failed) {
    fprintf(stderr, "%s: Checking tree failed\n", program);
    exit(1);
  }

  
  for(i=0; results[i]; i++) {
    const char* result=results[i];
    char* data=(char*)raptor_avltree_remove(tree, (void*)result);
    if(!data) {
      fprintf(stderr, "%s: remove %i failed at item '%s'\n", program, i,
              result);
      exit(1);
    }
    if(strcmp(data, result)) {
      fprintf(stderr, "%s: remove %i returned %s not %s as expected\n", program,
              i, data, result);
      exit(1);
    }
  }
  

#if RAPTOR_DEBUG > 1
  fprintf(stderr, "%s: Freeing tree\n", program);
#endif
  raptor_free_avltree(tree);

  /* keep gcc -Wall happy */
  return(0);
}

#endif
