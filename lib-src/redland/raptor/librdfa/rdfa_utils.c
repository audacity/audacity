/*
 * Copyright 2008 Digital Bazaar, Inc.
 *
 * This file is part of librdfa.
 * 
 * librdfa is Free Software, and can be licensed under any of the
 * following three licenses:
 * 
 *   1. GNU Lesser General Public License (LGPL) V2.1 or any 
 *      newer version
 *   2. GNU General Public License (GPL) V2 or any newer version
 *   3. Apache License, V2.0 or any newer version
 * 
 * You may not use this file except in compliance with at least one of
 * the above three licenses.
 * 
 * See LICENSE-* at the top of this software distribution for more
 * information regarding the details of each license.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with librdfa. If not, see <http://www.gnu.org/licenses/>.
 */
#include "stdlib.h"
#include "string.h"
#include "stdio.h"
#include "rdfa_utils.h"
#include "rdfa.h"

#define RDFA_WHITESPACE_CHARACTERS " \a\b\t\n\v\f\r"

char* rdfa_join_string(const char* prefix, const char* suffix)
{
   char* rval = NULL;
   size_t prefix_size = strlen(prefix);
   size_t suffix_size = strlen(suffix);
   rval = (char*)malloc(prefix_size + suffix_size + 1);
   
   memcpy(rval, prefix, prefix_size);
   memcpy(rval+prefix_size, suffix, suffix_size + 1);
   

   return rval;
}

char* rdfa_n_append_string(
   char* old_string, size_t* string_size,
   const char* suffix, size_t suffix_size)
{
   char* rval = NULL;
   rval = (char*)realloc(old_string, *string_size + suffix_size + 1);
   memcpy(rval + *string_size, suffix, suffix_size + 1);
   *string_size = *string_size + suffix_size;
   return rval;
}

char* rdfa_replace_string(char* old_string, const char* new_string)
{
   char* rval = NULL;
   
   if(new_string != NULL)
   {
      // free the memory associated with the old string if it exists.
      if(old_string != NULL)
      {
         free(old_string);
      }

      // copy the new string
      rval = strdup(new_string);
   }
   
   return rval;
}

char* rdfa_canonicalize_string(const char* str)
{
   char* rval = (char*)malloc(sizeof(char) * (strlen(str) + 2));
   char* working_string = NULL;
   char* token = NULL;
   char* wptr = NULL;
   char* offset = rval;
   
   working_string = rdfa_replace_string(working_string, str);

   // split on any whitespace character that we may find
   token = strtok_r(working_string, RDFA_WHITESPACE_CHARACTERS, &wptr);
   while(token != NULL)
   {
      size_t token_length = strlen(token);
      memcpy(offset, token, token_length);
      offset += token_length;
      *offset++ = ' ';
      *offset = '\0';
      
      token = strtok_r(NULL, RDFA_WHITESPACE_CHARACTERS, &wptr);
   }

   if(offset != rval)
   {      
      offset--;
      *offset = '\0';
   }

   free(working_string);

   return rval;
}

rdfalist* rdfa_create_list(size_t size)
{
   rdfalist* rval = (rdfalist*)malloc(sizeof(rdfalist));

   rval->max_items = size;
   rval->num_items = 0;
   rval->items = (rdfalistitem**)malloc(sizeof(rdfalistitem) * rval->max_items);

   return rval;
}

rdfalist* rdfa_copy_list(rdfalist* list)
{
   rdfalist* rval = (rdfalist*)malloc(sizeof(rdfalist));
   unsigned int i;

   // copy the base list variables over
   rval->max_items = list->max_items;
   rval->num_items = list->num_items;
   rval->items = NULL;
   rval->items = (rdfalistitem**)realloc(rval->items, sizeof(void*) * rval->max_items);

   // copy the data of every list member along with all of the flags
   // for each list member.
   //
   // TODO: Implement the copy for context, if it is needed.
   for(i = 0; i < list->max_items; i++)
   {
      if(i < rval->num_items)
      {
         if(list->items[i]->flags & RDFALIST_FLAG_TEXT)
         {
            rval->items[i] = (rdfalistitem*)malloc(sizeof(rdfalistitem));
            rval->items[i]->data = NULL;
            rval->items[i]->data =  (char*)
               rdfa_replace_string((char*)rval->items[i]->data, (const char*)list->items[i]->data);
            rval->items[i]->flags = list->items[i]->flags;
         }
      }
      else
      {
         rval->items[i] = NULL;
      }
   }

   return rval;
}

void rdfa_print_list(rdfalist* list)
{
   unsigned int i;

   printf("[ ");

   for(i = 0; i < list->num_items; i++)
   {
      if(i != 0)
      {
         printf(", ");
      }
      
      puts((const char*)list->items[i]->data);
   }

   printf(" ]\n");
}

void rdfa_free_list(rdfalist* list)
{
   if(list != NULL)
   {
      unsigned int i;
      for(i = 0; i < list->num_items; i++)
      {
         free(list->items[i]->data);
         free(list->items[i]);
      }
      
      free(list->items);
      free(list);
   }
}

void rdfa_push_item(rdfalist* stack, void* data, liflag_t flags)
{
   rdfa_add_item(stack, data, flags);
}

void* rdfa_pop_item(rdfalist* stack)
{
   void* rval = NULL;

   if(stack->num_items > 0)
   {
      rval = stack->items[stack->num_items - 1]->data;
      free(stack->items[stack->num_items - 1]);
      stack->items[stack->num_items - 1] = NULL;
      stack->num_items--;
   }

   return rval;
}

void rdfa_add_item(rdfalist* list, void* data, liflag_t flags)
{
   rdfalistitem* item = (rdfalistitem*)malloc(sizeof(rdfalistitem));

   item->data = NULL;

   if(flags & RDFALIST_FLAG_CONTEXT)
   {
      item->data = data;
   }
   else
   {
      item->data = (char*)rdfa_replace_string((char*)item->data, (const char*)data);
   }   
   
   item->flags = flags;

   if(list->num_items == list->max_items)
   {
      list->max_items = 1 + (list->max_items * 2);
      list->items = (rdfalistitem**)
         realloc(list->items, sizeof(rdfalistitem) * list->max_items);
   }

   list->items[list->num_items] = item;
   list->num_items++;
}

#ifndef LIBRDFA_IN_RAPTOR
char** rdfa_create_mapping(size_t elements)
{
   size_t mapping_size = sizeof(char*) * MAX_URI_MAPPINGS * 2;
   char** mapping = malloc(mapping_size);

   // only initialize the mapping if it is null.
   if(mapping != NULL)
   {
      memset(mapping, 0, mapping_size);
   }
   
   return mapping;
}

char** rdfa_copy_mapping(char** mapping)
{
   size_t mapping_size = sizeof(char*) * MAX_URI_MAPPINGS * 2;
   char** rval = malloc(mapping_size);
   char** mptr = mapping;
   char** rptr = rval;

   // initialize the mapping
   memset(rval, 0, mapping_size);
   
   // copy each element of the old mapping to the new mapping.
   while(*mptr != NULL)
   {
      *rptr = rdfa_replace_string(*rptr, *mptr);
      rptr++;
      mptr++;
   }
   
   return rval;
}

void rdfa_update_mapping(char** mapping, const char* key, const char* value)
{
   int found = 0;
   char** mptr = mapping;
   
   // search the current mapping to see if the namespace
   // prefix exists in the mapping
   while(*mptr != NULL)
   {
      if(strcmp(*mptr, key) == 0)
      {
         mptr++;
         *mptr = rdfa_replace_string(*mptr, value);
         found = 1;
      }
      else
      {
         mptr++;
      }
      mptr++;
   }

   // if we made it through the entire URI mapping and the key was not
   // found, create a new key-value pair.
   if(!found)
   {
      *mptr = rdfa_replace_string(*mptr, key);
      mptr++;
      *mptr = rdfa_replace_string(*mptr, value);
   }
}

const char* rdfa_get_mapping(char** mapping, const char* key)
{
   const char* rval = NULL;
   char** mptr = mapping;
   
   // search the current mapping to see if the key exists in the mapping.
   while(*mptr != NULL)
   {
      if(strcmp(*mptr, key) == 0)
      {
         mptr++;
         rval = *mptr;
      }
      else
      {
         mptr++;
      }
      mptr++;
   }
   
   return rval;
}

void rdfa_next_mapping(char** mapping, char** key, char** value)
{
   *key = NULL;
   *value = NULL;
   
   if(*mapping != NULL)
   {
      *key = *mapping++;
      *value = *mapping++;
   }
}

void rdfa_print_mapping(char** mapping)
{
   char** mptr = mapping;
   printf("{\n");
   while(*mptr != NULL)
   {
      char* key;
      char* value;
      key = *mptr++;
      value = *mptr++;

      printf("   %s : %s", key, value);
      if(*mptr != NULL)
      {
         printf(",\n");
      }
      else
      {
         printf("\n");
      }
   }
   printf("}\n");
}

void rdfa_free_mapping(char** mapping)
{
   char** mptr = mapping;

   if(mapping != NULL)
   {
      // free all of the memory in the mapping
      while(*mptr != NULL)
      {
         free(*mptr);
         mptr++;
      }

      free(mapping);
   }
}
#endif
