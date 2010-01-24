/* -*- Mode: c; c-basic-offset: 2 -*-
 *
 * librdf_avltree.c - Balanced Binary Tree / AVL Tree
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
#include <rdf_config.h>
#endif

#ifdef WIN32
#include <win32_rdf_config.h>
#endif

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#include <redland.h>
#include "rdf_avltree_internal.h"

#if LIBRDF_DEBUG > 1
#define LIBRDF_AVLTREE_DEBUG1(msg) LIBRDF_DEBUG1(msg)
#else
#define LIBRDF_AVLTREE_DEBUG1(msg)
#endif


#ifndef STANDALONE
typedef struct librdf_avltree_node_s librdf_avltree_node;

/* AVL-tree node */
struct librdf_avltree_node_s {
  /* parent tree */
  struct librdf_avltree_node_s *parent;
  
  /* left child tree */
  struct librdf_avltree_node_s *left;

  /* right child tree */
  struct librdf_avltree_node_s *right;
  
  /* actual data */
  void* data;

  /* balance factor =
   *   height of the right tree minus the height of the left tree
   * i.e. equal: 0  left larger: -1  right larger: 1
   */
  char balance;
};


/* AVL-tree */
struct librdf_avltree_s {
  /* root node of tree */
  librdf_avltree_node* root;

  /* node comparison function (optional) */
  librdf_avltree_data_compare_function compare_fn;

  /* node deletion function (optional) */
  librdf_avltree_data_free_function free_fn;

  /* number of nodes in tree */
  size_t size;
};


#ifndef TRUE
#define TRUE  1
#define FALSE 0
#endif


/* local prototypes */
static int librdf_avltree_sprout(librdf_avltree* tree, librdf_avltree_node* parent, librdf_avltree_node** node_pp, void* p_data, int *rebalancing_p);
static void* librdf_avltree_delete_internal(librdf_avltree* tree, librdf_avltree_node** node_pp, void* p_data, int *rebalancing_p);
static void* librdf_avltree_delete_internal2(librdf_avltree* tree, librdf_avltree_node** ppr_r, int *rebalancing_p, librdf_avltree_node** ppr_q);
static void librdf_avltree_balance_left(librdf_avltree* tree, librdf_avltree_node** node_pp, int *rebalancing_p);
static void librdf_avltree_balance_right(librdf_avltree* tree, librdf_avltree_node** node_pp, int *rebalancing_p);
static librdf_avltree_node* librdf_avltree_search_internal(librdf_avltree* tree, librdf_avltree_node* node, const void* p_data);
static int librdf_avltree_visit_internal(librdf_avltree* tree, librdf_avltree_node* node, int depth, librdf_avltree_visit_function visit_fn, void* user_data);
static void librdf_free_avltree_internal(librdf_avltree* tree, librdf_avltree_node* node);


/* avltree constructor */
librdf_avltree*
librdf_new_avltree(librdf_avltree_data_compare_function compare_fn,
                   librdf_avltree_data_free_function free_fn/*,
                   unsigned int flags*/)
{
  librdf_avltree* tree;
  
  tree=(librdf_avltree*)LIBRDF_MALLOC(librdf_avltree, sizeof(*tree));
  if(!tree)
    return NULL;
  
  tree->root=NULL;
  tree->compare_fn=compare_fn;
  tree->free_fn=free_fn;
  tree->size=0;
  
  return tree;
}


/* avltree destructor */
void
librdf_free_avltree(librdf_avltree* tree)
{
  librdf_free_avltree_internal(tree, tree->root);
  LIBRDF_FREE(librdf_avltree, tree);
}


static void
librdf_free_avltree_internal(librdf_avltree* tree, librdf_avltree_node* node)
{
  if(node) {
    librdf_free_avltree_internal(tree, node->left);

    librdf_free_avltree_internal(tree, node->right);

    if(tree->free_fn)
      tree->free_fn(node->data);
    tree->size--;
    LIBRDF_FREE(librdf_avltree_node, node);
  }
}


/* methods */

static librdf_avltree_node*
librdf_avltree_search_internal(librdf_avltree* tree, librdf_avltree_node* node,
                               const void* p_data)
{
  if(node) {
    int cmp= tree->compare_fn(p_data, node->data);

    if(cmp > 0)
      return librdf_avltree_search_internal(tree, node->right, p_data);
    else if(cmp < 0)
      return librdf_avltree_search_internal(tree, node->left, p_data);

    /* found */
    return node;
  }

  /* otherwise not found */
  return NULL;
}


/* find an item and return shared pointer to it (still owned by avltree) */
void*
librdf_avltree_search(librdf_avltree* tree, const void* p_data)
{
  librdf_avltree_node* node;
  node=librdf_avltree_search_internal(tree, tree->root, p_data);
  return node ? node->data : NULL;
}


/* add an item (becomes owned by avltree).
 * Return 0 on success.
 * Return LIBRDF_AVLTREE_EXISTS if equivalent item exists
 *   (and the old element remains in the tree).
 * Return LIBRDF_AVLTREE_ENOMEM if memory is exhausted.
 */
int
librdf_avltree_add(librdf_avltree* tree, void* p_data)
{
  int rebalancing= FALSE;
  int rv;

  rv=librdf_avltree_sprout(tree, NULL, &tree->root, p_data,
                           &rebalancing);
#if LIBRDF_DEBUG > 1
  librdf_avltree_check(tree);
#endif

  return rv;
}


/* remove an item and return it (no longer owned by avltree) */
void*
librdf_avltree_remove(librdf_avltree* tree, void* p_data)
{
  int rebalancing= FALSE;
  void* rdata;
  
  rdata=librdf_avltree_delete_internal(tree, &tree->root, p_data,
                                       &rebalancing);
  if(rdata)
    tree->size--;

#if LIBRDF_DEBUG > 1
  librdf_avltree_check(tree);
#endif
  
  return rdata;
}


/* remove an item and free it */
int
librdf_avltree_delete(librdf_avltree* tree, void* p_data)
{
  void* rdata;

  rdata=librdf_avltree_remove(tree, p_data);
  if(rdata) {
    if(tree->free_fn)
      tree->free_fn(rdata);
  }

  return (rdata != NULL);
}


static int
librdf_avltree_visit_internal(librdf_avltree* tree, librdf_avltree_node* node,
                              int depth,
                              librdf_avltree_visit_function visit_fn,
                              void* user_data)
{
  if(!node)
    return TRUE;

  if(!librdf_avltree_visit_internal(tree, node->left, depth+1, 
                                    visit_fn, user_data))
    return FALSE;

  if(!visit_fn(depth, node->data, user_data))
    return FALSE;

  if(!librdf_avltree_visit_internal(tree, node->right, depth+1,
                                    visit_fn, user_data))
    return FALSE;

  return TRUE;
}


/* perform an in-order visit of the items in the tree */
int
librdf_avltree_visit(librdf_avltree* tree,
                     librdf_avltree_visit_function visit_fn,
                     void* user_data)
{
  return librdf_avltree_visit_internal(tree, tree->root, 0,
                                       visit_fn, user_data);
}


#if defined(LIBRDF_DEBUG) && LIBRDF_DEBUG > 1
static void
librdf_avltree_print_node(librdf_avltree_node* node) 
{
  fprintf(stderr, "%p: parent %p  left %p  right %p  data %p",
          node, node->parent, node->left, node->right, node->data);
}

static void
librdf_avltree_check_node(librdf_avltree* tree, librdf_avltree_node* node,
                          const char* fn, const char* where) 
{
  if(node->parent) {
    if((node->parent == node->left) || (node->parent == node->right)) {
      if(fn && where)
        fprintf(stderr, "%s (%s): ", fn, where);
      fputs("ERROR bad node ", stderr);
      librdf_avltree_print_node(node);
      fputc('\n', stderr);
      fflush(stderr);
      abort();
    }
    if(node->parent->left != node && node->parent->right != node) {
      if(fn && where)
        fprintf(stderr, "%s (%s): ", fn, where);
      fputs("ERROR parent node ", stderr);
      librdf_avltree_print_node(node->parent);
      fputs(" has no reference to child node ", stderr);
      librdf_avltree_print_node(node);
      fputc('\n', stderr);
      fflush(stderr);
      abort();
    }
  }
}
#endif

#endif


static int
librdf_avltree_sprout_left(librdf_avltree* tree, librdf_avltree_node** node_pp,
                           void* p_data, int *rebalancing_p)
{
  librdf_avltree_node *p1, *p2, *p_parent;
  int rc;

  LIBRDF_AVLTREE_DEBUG1("LESS. librdf_avltree_sprouting left.\n");

  p_parent=(*node_pp)->parent;
  
  rc=librdf_avltree_sprout(tree, *node_pp, &(*node_pp)->left, p_data,
                           rebalancing_p);
  if(rc)
    return rc;

  if(!*rebalancing_p)
    return FALSE;

  /* left branch has grown longer */
  LIBRDF_AVLTREE_DEBUG1("LESS: left branch has grown\n");
  switch((*node_pp)->balance) {
    case 1:
      /* right branch WAS longer; balance is ok now */
      LIBRDF_AVLTREE_DEBUG1("LESS: case 1.. balance restored implicitly\n");
      (*node_pp)->balance= 0;
      *rebalancing_p= FALSE;
      break;

    case 0:
      /* balance WAS okay; now left branch longer */
      LIBRDF_AVLTREE_DEBUG1("LESS: case 0.. balance bad but still ok\n");
      (*node_pp)->balance= -1;
      break;

    case -1:
      /* left branch was already too long. rebalance */
      LIBRDF_AVLTREE_DEBUG1("LESS: case -1: rebalancing\n");
      p1= (*node_pp)->left;

      if(p1->balance == -1) {
        /* LL */
        LIBRDF_AVLTREE_DEBUG1("LESS: single LL\n");
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
        LIBRDF_AVLTREE_DEBUG1("LESS: double LR\n");
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
librdf_avltree_sprout_right(librdf_avltree* tree,
                            librdf_avltree_node** node_pp, 
                            void* p_data, int *rebalancing_p)
{
  librdf_avltree_node *p1, *p2, *p_parent;
  int rc;

  LIBRDF_AVLTREE_DEBUG1("MORE: librdf_avltree_sprouting to the right\n");

  p_parent=(*node_pp)->parent;
  
  rc=librdf_avltree_sprout(tree, *node_pp, &(*node_pp)->right, p_data,
                           rebalancing_p);
  if(rc)
    return rc;

  if(!*rebalancing_p)
    return FALSE;
  
  /* right branch has grown longer */
  LIBRDF_AVLTREE_DEBUG1("MORE: right branch has grown\n");
  
  switch((*node_pp)->balance) {
    case -1:
      LIBRDF_AVLTREE_DEBUG1("MORE: balance was off, fixed implicitly\n");
      (*node_pp)->balance= 0;
      *rebalancing_p= FALSE;
      break;
      
    case 0:
      LIBRDF_AVLTREE_DEBUG1("MORE: balance was okay, now off but ok\n");
      (*node_pp)->balance= 1;
      break;
      
    case 1:
      LIBRDF_AVLTREE_DEBUG1("MORE: balance was off, need to rebalance\n");
      p1= (*node_pp)->right;
      
      if(p1->balance == 1) {
        /* RR */
        LIBRDF_AVLTREE_DEBUG1("MORE: single RR\n");
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
        LIBRDF_AVLTREE_DEBUG1("MORE: double RL\n");
        
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


static int
librdf_avltree_sprout(librdf_avltree* tree, librdf_avltree_node* parent,
                      librdf_avltree_node** node_pp, void* p_data,
                      int *rebalancing_p)
{
  int cmp;

  LIBRDF_AVLTREE_DEBUG1("Enter\n");

  /* If grounded, add the node here, set the rebalance flag and return */
  if(!*node_pp) {
    LIBRDF_AVLTREE_DEBUG1("grounded. adding new node, setting rebalancing flag true\n");
    *node_pp= (librdf_avltree_node*)LIBRDF_MALLOC(librdf_avltree_node, sizeof(**node_pp));
    if(!*node_pp)
      return LIBRDF_AVLTREE_ENOMEM;
    
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
    return librdf_avltree_sprout_left(tree, node_pp, p_data, rebalancing_p);
  else if(cmp > 0)
    /* if MORE, prepare to move to the right. */
    return librdf_avltree_sprout_right(tree, node_pp, p_data, rebalancing_p);

  /* otherwise same key */
  *rebalancing_p= FALSE;

  /* replace */
  /*if(tree->free_fn)
    tree->free_fn((*node_pp)->data);
  (*node_pp)->data= p_data;

  return FALSE;*/

  /* ignore */
  tree->free_fn(p_data);
  return LIBRDF_AVLTREE_EXISTS;
}


static void*
librdf_avltree_delete_internal(librdf_avltree* tree,
                               librdf_avltree_node** node_pp,
                               void* p_data,
                               int *rebalancing_p)
{
  int cmp;
  void* rdata=NULL;

  LIBRDF_AVLTREE_DEBUG1("Enter\n");

  if(*node_pp == NULL) {
    LIBRDF_AVLTREE_DEBUG1("key not in tree\n");
    return rdata;
  }

  cmp= tree->compare_fn((*node_pp)->data, p_data);

  if(cmp > 0) {
    LIBRDF_AVLTREE_DEBUG1("too high - scan left\n");
    rdata= librdf_avltree_delete_internal(tree, &(*node_pp)->left, p_data,
                                          rebalancing_p);
    if(*rebalancing_p)
      librdf_avltree_balance_left(tree, node_pp, rebalancing_p);

  } else if(cmp < 0) {
    LIBRDF_AVLTREE_DEBUG1("too low - scan right\n");
    rdata= librdf_avltree_delete_internal(tree, &(*node_pp)->right, p_data,
                                          rebalancing_p);
    if(*rebalancing_p)
      librdf_avltree_balance_right(tree, node_pp, rebalancing_p);

  } else {
    librdf_avltree_node *pr_q;

    LIBRDF_AVLTREE_DEBUG1("equal\n");
    pr_q= *node_pp;
    
    rdata=pr_q->data;
    
    if(pr_q->right == NULL) {
      LIBRDF_AVLTREE_DEBUG1("right subtree null\n");
      *node_pp= pr_q->left;
      *rebalancing_p= TRUE;
    } else if(pr_q->left == NULL) {
      LIBRDF_AVLTREE_DEBUG1("right subtree non-null, left subtree null\n");
      *node_pp= pr_q->right;
      *rebalancing_p= TRUE;
    } else {
      LIBRDF_AVLTREE_DEBUG1("neither subtree null\n");
      rdata=librdf_avltree_delete_internal2(tree, &pr_q->left, rebalancing_p,
                                            &pr_q);
      if(*rebalancing_p)
        librdf_avltree_balance_left(tree, node_pp, rebalancing_p);
    }

    LIBRDF_FREE(librdf_avltree_node, pr_q);
  }

  return rdata;
}


static void*
librdf_avltree_delete_internal2(librdf_avltree* tree,
                                librdf_avltree_node** ppr_r,
                                int *rebalancing_p,
                                librdf_avltree_node** ppr_q)
{
  void* rdata=NULL;
  
  LIBRDF_AVLTREE_DEBUG1("Enter\n");

  if((*ppr_r)->right != NULL) {
    rdata=librdf_avltree_delete_internal2(tree,
                                          &(*ppr_r)->right, 
                                          rebalancing_p,
                                          ppr_q);
    if(*rebalancing_p)
      librdf_avltree_balance_right(tree, ppr_r, rebalancing_p);

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
librdf_avltree_balance_left(librdf_avltree* tree,
                            librdf_avltree_node** node_pp, int *rebalancing_p)
{
  librdf_avltree_node *p1, *p2, *p_parent;
  int b1, b2;

  LIBRDF_AVLTREE_DEBUG1("left branch has shrunk\n");

  p_parent=(*node_pp)->parent;

  switch((*node_pp)->balance) {
    case -1:
      LIBRDF_AVLTREE_DEBUG1("was imbalanced, fixed implicitly\n");
      (*node_pp)->balance= 0;
      break;

    case 0:
      LIBRDF_AVLTREE_DEBUG1("was okay, is now one off\n");
      (*node_pp)->balance= 1;
      *rebalancing_p= FALSE;
      break;

    case 1:
      LIBRDF_AVLTREE_DEBUG1("was already off, this is too much\n");
      p1= (*node_pp)->right;
      b1= p1->balance;

      if(b1 >= 0) {
        LIBRDF_AVLTREE_DEBUG1("single RR\n");
        (*node_pp)->right= p1->left;
        if((*node_pp)->right)
          (*node_pp)->right->parent= (*node_pp);
        p1->left= *node_pp;
        if(p1->left)
          p1->left->parent= p1;
        if(b1 == 0) {
          LIBRDF_AVLTREE_DEBUG1("b1 == 0\n");
          (*node_pp)->balance= 1;
          p1->balance= -1;
          *rebalancing_p= FALSE;
        } else {
          LIBRDF_AVLTREE_DEBUG1("b1 != 0\n");
          (*node_pp)->balance= 0;
          p1->balance= 0;
        }
        *node_pp= p1;
        (*node_pp)->parent=p_parent;
      } else {
        LIBRDF_AVLTREE_DEBUG1("double RL\n");
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
librdf_avltree_balance_right(librdf_avltree* tree,
                             librdf_avltree_node** node_pp, int *rebalancing_p)
{
  librdf_avltree_node *p1, *p2, *p_parent;
  int b1, b2;

  LIBRDF_AVLTREE_DEBUG1("right branch has shrunk\n");

  p_parent=(*node_pp)->parent;

  switch((*node_pp)->balance) {
    case 1:
      LIBRDF_AVLTREE_DEBUG1("was imbalanced, fixed implicitly\n");
      (*node_pp)->balance= 0;
      break;

    case 0:
      LIBRDF_AVLTREE_DEBUG1("was okay, is now one off\n");
      (*node_pp)->balance= -1;
      *rebalancing_p= FALSE;
      break;

    case -1:
      LIBRDF_AVLTREE_DEBUG1("was already off, this is too much\n");
      p1= (*node_pp)->left;
      b1= p1->balance;

      if(b1 <= 0) {
        LIBRDF_AVLTREE_DEBUG1("single LL\n");
        (*node_pp)->left= p1->right;
        if((*node_pp)->left)
          (*node_pp)->left->parent= (*node_pp);
        p1->right= *node_pp;
        if(p1->right)
          p1->right->parent= p1;
        if(b1 == 0) {
          LIBRDF_AVLTREE_DEBUG1("b1 == 0\n");
          (*node_pp)->balance= -1;
          p1->balance= 1;
          *rebalancing_p= FALSE;
        } else {
          LIBRDF_AVLTREE_DEBUG1("b1 != 0\n");
          (*node_pp)->balance= 0;
          p1->balance= 0;
        }
        *node_pp= p1;
        (*node_pp)->parent=p_parent;
      } else {
        LIBRDF_AVLTREE_DEBUG1("double LR\n");
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


/* get the number of items in the tree */
int
librdf_avltree_size(librdf_avltree* tree)
{
  return tree->size;
}


static librdf_avltree_node*
librdf_avltree_node_leftmost(librdf_avltree* tree, librdf_avltree_node* node, void* range)
{
  /*assert(node);
  assert(!range || tree->compare_fn(range, node->data) == 0);*/
  if (range)
    while(node && node->left && tree->compare_fn(range, node->left->data) == 0)
      node=node->left;
  else
    while(node && node->left)
      node=node->left;
  
  return node;
}


#if 0
static librdf_avltree_node*
librdf_avltree_node_rightmost(librdf_avltree* tree, librdf_avltree_node* node)
{
  while(node && node->right)
    node=node->right;
  return node;
}
#endif

/* Follow right children until a match for range is found */
static librdf_avltree_node*
librdf_avltree_node_search_right(librdf_avltree* tree, librdf_avltree_node* node, void* range)
{
  librdf_avltree_node* result;
  
  if (node == NULL)
    return NULL;

  result=node->right;
  while(result) {
    if (tree->compare_fn(range, result->data) == 0) {
      return result;
    } else {
      result = result->right;
    }
  }

  return node;
}


#if 0
static librdf_avltree_node*
librdf_avltree_node_prev(librdf_avltree* tree, librdf_avltree_node* node)
{
  if(node->left) {
    node=librdf_avltree_node_rightmost(tree, node->left);
  } else {
    librdf_avltree_node* last=node;
    /* Need to go up */
    node=node->parent;
    while(node) {
      /* moving from right subtree to this node */
      if(node->right && last == node->right)
        break;
      
      /* moved up to find an unvisited left subtree */
      if(node->left && last != node->left) {
        node=librdf_avltree_node_rightmost(tree, node->left);
        break;
      }
      last=node;
      node=node->parent;
    }
  }
    
  return node;
}
#endif


static librdf_avltree_node*
librdf_avltree_node_next(librdf_avltree* tree, librdf_avltree_node* node, void* range)
{
  int up=0;

  /*assert(!range || tree->compare_fn(range, node->data) == 0);*/

  if(node->right) {
    /* Should never go right if the current node is already > range */
    librdf_avltree_node* next=librdf_avltree_node_leftmost(tree, node->right, NULL);
    /*assert(!range ||tree->compare_fn(range, node->data) <= 0);*/
    if (range) {
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
  
  if (up) {
    librdf_avltree_node* last=node;
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
        node=librdf_avltree_node_leftmost(tree, node->right, range);
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


typedef struct {
  librdf_avltree* tree;
  librdf_avltree_node* root;
  librdf_avltree_node* current;
  void* range;
  librdf_avltree_data_free_function range_free_fn;
} librdf_avltree_iterator_context;


/* print the items in the tree in order (for debugging) */
void
librdf_avltree_print(librdf_world* world, librdf_avltree* tree, FILE* stream,
                     librdf_avltree_data_print_function print_fn)
{
  int i;
  int rv=0;
  librdf_iterator* iter;

  fprintf(stream, "AVL Tree size %zu\n", tree->size);
  for(i=0, (iter=librdf_avltree_get_iterator_start(world, tree, NULL, NULL));
      iter && !rv;
      i++, (rv=librdf_iterator_next(iter))) {
    const void* data=librdf_iterator_get_object(iter);
    if(!data)
      continue;
    fprintf(stream, "%d) ", i);
    if(print_fn)
      print_fn(stream, data);
    else
      fprintf(stream, "Data Node %p\n", data);
  }
  /*assert(i == tree->size);*/
}


static int
librdf_avltree_iterator_is_end(void* iterator) 
{
  librdf_avltree_iterator_context* context=(librdf_avltree_iterator_context*)iterator;
  librdf_avltree_node *node=context->current;
  
  return (node == NULL);
}

static int
librdf_avltree_iterator_next_method(void* iterator) 
{
  librdf_avltree_iterator_context* context=(librdf_avltree_iterator_context*)iterator;
  librdf_avltree_node *node=context->current;
  
  if(!node)
    return 1;
  
  context->current=librdf_avltree_node_next(context->tree, node, context->range);
  // Stay within rooted subtree
  if (context->root->parent == context->current)
    context->current = NULL;

  return (context->current == NULL);
}


static void*
librdf_avltree_iterator_get_method(void* iterator, int flags) 
{
  librdf_avltree_iterator_context* context=(librdf_avltree_iterator_context*)iterator;
  librdf_avltree_node *node=context->current;
  
  switch(flags) {
  case LIBRDF_ITERATOR_GET_METHOD_GET_OBJECT:
    return node->data;
  default:
    return NULL;
  }
}


static void
librdf_avltree_iterator_finished(void* iterator)
{
  librdf_avltree_iterator_context* context=(librdf_avltree_iterator_context*)iterator;

  if(!context)
    return;

  if(context->range && context->range_free_fn)
    context->range_free_fn(context->range);

  LIBRDF_FREE(librdf_avltree_iterator_context, context);
}

/**
 * librdf_avltree_get_iterator_start:
 * @list: #librdf_avltree object
 *
 * Get an (in-order) iterator for the start of a range, or the entire tree
 * (if range is NULL).  If range specifies a range (i.e. the tree comparison
 * function will 'match' (return 0 for) range and /several/ nodes), the
 * iterator will be placed at the leftmost child matching range, and
 * librdf_avltree_iterator_next will iterate over all nodes
 * (and only nodes) that match range.
 * 
 * Return value: a new #librdf_iterator object or NULL on failure
 **/
librdf_iterator*
librdf_avltree_get_iterator_start(librdf_world* world, librdf_avltree* tree, void* range,
    librdf_avltree_data_free_function range_free_fn)
{
  librdf_avltree_iterator_context* context;
  librdf_iterator* iterator;

  context=(librdf_avltree_iterator_context*)LIBRDF_CALLOC(librdf_avltree_iterator_context,
      1, sizeof(librdf_avltree_iterator_context));
  if(!context)
    return NULL;

  context->tree = tree;
  context->range = range;
  context->range_free_fn = range_free_fn;

  if (range != NULL) {
    /* find the topmost match (range is contained entirely in tree rooted here) */
    context->current=librdf_avltree_search_internal(tree, tree->root, range);
  } else {
    context->current=tree->root;
  }

  context->root = context->current;
  
  /* go down to find start of range (or tree) */
  if (context->current) {
    while (1) {
      librdf_avltree_node* pred;
      context->current=librdf_avltree_node_leftmost(tree, context->current, range);
      /* right until we find a match */
      pred=librdf_avltree_node_search_right(tree, context->current->left, range);

      if(pred && tree->compare_fn(range, pred->data) == 0)
        context->current = pred;
      else
        break;
    }
  }

  iterator=librdf_new_iterator(world, 
                               (void*)context,
                               librdf_avltree_iterator_is_end,
                               librdf_avltree_iterator_next_method,
                               librdf_avltree_iterator_get_method,
                               librdf_avltree_iterator_finished);
  
  if(!iterator) {
    librdf_avltree_iterator_finished(context);
  }

  return iterator;
}

//#if defined(LIBRDF_DEBUG) && LIBRDF_DEBUG > 1

static int
librdf_avltree_dump_internal(librdf_avltree* tree, librdf_avltree_node* node,
                             int depth, FILE* stream,
                             librdf_avltree_data_print_function print_fn)
{
  int i;
  if(!node)
    return TRUE;

  for(i=0; i < depth; i++)
    fputs("  ", stream);
  if(print_fn) {
    for(i= 0; i < depth; i++)
      fputs("  ", stream);
    print_fn(stream, node->data);
  }
  
  if(!librdf_avltree_dump_internal(tree, node->left, depth+1, stream, print_fn))
    return FALSE;

  if(!librdf_avltree_dump_internal(tree, node->right, depth+1, stream, print_fn))
    return FALSE;

  return TRUE;
}


/* debugging tree dump with pointers and depth indenting */
int
librdf_avltree_dump(librdf_avltree* tree, FILE* stream, librdf_avltree_data_print_function print_fn)
{
  fprintf(stream, "Dumping avltree %p  size %zd\n", tree, tree->size);
  return librdf_avltree_dump_internal(tree, tree->root, 0, stream, print_fn);
}

#if defined(LIBRDF_DEBUG) && LIBRDF_DEBUG > 1


static void
librdf_avltree_check_internal(librdf_avltree* tree, librdf_avltree_node* node,
                              int* count_p)
{
  if(!node)
    return;
  (*count_p)++;
  
  librdf_avltree_check_node(tree, node, NULL, NULL);

  librdf_avltree_check_internal(tree, node->left, count_p);

  librdf_avltree_check_internal(tree, node->right, count_p);
}


/* debugging tree check - parent/child pointers and counts */
void
librdf_avltree_check(librdf_avltree* tree)
{
  int count=0;
  
  librdf_avltree_check_internal(tree, tree->root, &count);
  if(count != tree->size) {
    fprintf(stderr, "Tree %p nodes count is %d.  actual count %d\n",
            tree, tree->size, count);
    abort();
  }
}

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
  
#if LIBRDF_DEBUG > 1
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
  const char *program=librdf_basename(argv[0]);
#define ITEM_COUNT 8
  const char *items[ITEM_COUNT+1] = { "ron", "amy", "jen", "bij", "jib", "daj", "jim", "def", NULL };
#define DELETE_COUNT 2
  const char *delete_items[DELETE_COUNT+1] = { "jen", "jim", NULL };
#define RESULT_COUNT (ITEM_COUNT-DELETE_COUNT)
  const char *results[RESULT_COUNT+1] = { "amy", "bij", "daj", "def", "jib", "ron", NULL};

  librdf_world* world;
  librdf_avltree* tree;
  librdf_iterator* iter;
  visit_state vs;
  int i;
  
  world=librdf_new_world();
  tree=librdf_new_avltree(compare_strings,
                          NULL); /* no free as they are static pointers above */
  
  if(!tree) {
    fprintf(stderr, "%s: Failed to create tree\n", program);
    exit(1);
  }
  for(i=0; items[i]; i++) {
    int rc;
    void* node;

#if LIBRDF_DEBUG > 1
    fprintf(stderr, "%s: Adding tree item '%s'\n", program, items[i]);
#endif
  
    rc=librdf_avltree_add(tree, (void*)items[i]);
    if(rc) {
      fprintf(stderr,
              "%s: Adding tree item %d '%s' failed, returning error %d\n",
              program, i, items[i], rc);
      exit(1);
    }

#ifdef LIBRDF_DEBUG
    librdf_avltree_check(tree);
#endif

    node=librdf_avltree_search(tree, (void*)items[i]);
    if(!node) {
      fprintf(stderr,
              "%s: Tree did NOT contain item %d '%s' as expected\n",
              program, i, items[i]);
      exit(1);
    }
  }

#if LIBRDF_DEBUG > 1
  fprintf(stderr, "%s: Printing tree\n", program);
  vs.fh=stderr;
  vs.count=0;
  librdf_avltree_visit(tree, print_string, &vs);

  fprintf(stderr, "%s: Dumping tree\n", program);
  librdf_avltree_dump(tree, stderr);
#endif


  for(i=0; delete_items[i]; i++) {
    int rc;

#if LIBRDF_DEBUG > 1
    fprintf(stderr, "%s: Deleting tree item '%s'\n", program, delete_items[i]);
#endif
  
    rc=librdf_avltree_delete(tree, (void*)delete_items[i]);
    if(!rc) {
      fprintf(stderr,
              "%s: Deleting tree item %d '%s' failed, returning error %d\n",
              program, i, delete_items[i], rc);
      exit(1);
    }

#ifdef LIBRDF_DEBUG
    librdf_avltree_check(tree);
#endif
  }


#if LIBRDF_DEBUG > 1
  fprintf(stderr, "%s: Walking tree forwards via iterator\n", program);
#endif
  iter=librdf_avltree_get_iterator_start(world, tree, NULL, NULL);
  for(i=0; 1; i++) {
    const char* data=(const char*)librdf_iterator_get_object(iter);
    const char* result=results[i];
    if((!data && data != result) || (data && strcmp(data, result))) {
      fprintf(stderr, "%3d: Forwards iterator expected '%s' but found '%s'\n",
              i, result, data);
      exit(1);
    }
#if LIBRDF_DEBUG > 1
    fprintf(stderr, "%3d: Got '%s'\n", i, data);
#endif
    if(librdf_iterator_next(iter))
      break;
    if(i > RESULT_COUNT) {
      fprintf(stderr, "Forward iterator did not end on result %i as expected\n", i);
      exit(1);
    }
  }


#if LIBRDF_DEBUG > 1
  fprintf(stderr, "%s: Checking tree\n", program);
#endif
  vs.count=0;
  vs.results=results;
  vs.failed=0;
  librdf_avltree_visit(tree, check_string, &vs);
  if(vs.failed) {
    fprintf(stderr, "%s: Checking tree failed\n", program);
    exit(1);
  }

  
  for(i=0; results[i]; i++) {
    const char* result=results[i];
    char* data=(char*)librdf_avltree_remove(tree, (void*)result);
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
  

#if LIBRDF_DEBUG > 1
  fprintf(stderr, "%s: Freeing tree\n", program);
#endif
  librdf_free_avltree(tree);
  librdf_free_world(world);

  /* keep gcc -Wall happy */
  return(0);
}

#endif
