/*
  libcore: library for basic data structures and algorithms.
  Copyright (C) 2005  IPD Goos, Universit"at Karlsruhe, Germany

  This library is free software; you can redistribute it and/or
  modify it under the terms of the GNU Lesser General Public
  License as published by the Free Software Foundation; either
  version 2.1 of the License, or (at your option) any later version.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  Lesser General Public License for more details.

  You should have received a copy of the GNU Lesser General Public
  License along with this library; if not, write to the Free Software
  Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
*/


/**
 * Linked lists. 
 * Shamelessly adapted from the linux kernel.
 */

#ifndef _LC_LIST_H
#define _LC_LIST_H

#include <libcore/lc_config.h>
#include <libcore/lc_defines.h>

/*
 * Simple doubly linked list implementation.
 *
 * Some of the internal functions ("_xxx") are useful when
 * manipulating whole lists rather than single entries, as
 * sometimes we already know the next/prev entries and we can
 * generate better code by using them directly rather than
 * using the generic single-entry routines.
 */

struct lc_list_head {
	struct lc_list_head *next, *prev;
};

typedef struct lc_list_head lc_list_t;

#define LC_LIST_HEAD_INIT(name) { &(name), &(name) }

#define LC_LIST_HEAD(name) \
	struct lc_list_head name = LC_LIST_HEAD_INIT(name)

#define LC_INIT_LIST_HEAD(ptr) do { \
	(ptr)->next = (ptr); (ptr)->prev = (ptr); \
} while (0)

#define _lc_list_offsetof(type,member) \
  LC_OFFSETOF(type, member)

#define _lc_list_container_of(ptr, type, member) \
	((type *) ((char *) (ptr) - _lc_list_offsetof(type, member)))

/*
 * Insert a new entry between two known consecutive entries. 
 *
 * This is only for internal list manipulation where we know
 * the prev/next entries already!
 */
static LC_INLINE void _lc_list_add(struct lc_list_head *new_node,
			      struct lc_list_head *prev,
			      struct lc_list_head *next)
{
	next->prev = new_node;
	new_node->next = next;
	new_node->prev = prev;
	prev->next = new_node;
}

/**
 * lc_list_add - add a new entry
 * @new: new entry to be added
 * @head: list head to add it after
 *
 * Insert a new entry after the specified head.
 * This is good for implementing stacks.
 */
static LC_INLINE void lc_list_add(struct lc_list_head *new_node, struct lc_list_head *head)
{
	_lc_list_add(new_node, head, head->next);
}

/**
 * lc_list_add_tail - add a new entry
 * @new: new entry to be added
 * @head: list head to add it before
 *
 * Insert a new entry before the specified head.
 * This is useful for implementing queues.
 */
static LC_INLINE void lc_list_add_tail(struct lc_list_head *new_node, struct lc_list_head *head)
{
	_lc_list_add(new_node, head->prev, head);
}

/*
 * Delete a list entry by making the prev/next entries
 * point to each other.
 *
 * This is only for internal list manipulation where we know
 * the prev/next entries already!
 */
static LC_INLINE void _lc_list_del(struct lc_list_head * prev, struct lc_list_head * next)
{
	next->prev = prev;
	prev->next = next;
}

/**
 * lc_list_del - deletes entry from list.
 * @entry: the element to delete from the list.
 * Note: lc_list_empty on entry does not return true after this, the entry is
 * in an undefined state.
 */
static LC_INLINE void lc_list_del(struct lc_list_head *entry)
{
	_lc_list_del(entry->prev, entry->next);
	entry->next = NULL;
	entry->prev = NULL;
}


/**
 * lc_list_del_init - deletes entry from list and reinitialize it.
 * @entry: the element to delete from the list.
 */
static LC_INLINE void lc_list_del_init(struct lc_list_head *entry)
{
	_lc_list_del(entry->prev, entry->next);
	LC_INIT_LIST_HEAD(entry); 
}

/**
 * lc_list_move - delete from one list and add as another's head
 * @list: the entry to move
 * @head: the head that will precede our entry
 */
static LC_INLINE void lc_list_move(struct lc_list_head *list, struct lc_list_head *head)
{
        _lc_list_del(list->prev, list->next);
        lc_list_add(list, head);
}

/**
 * lc_list_move_tail - delete from one list and add as another's tail
 * @list: the entry to move
 * @head: the head that will follow our entry
 */
static LC_INLINE void lc_list_move_tail(struct lc_list_head *list,
				  struct lc_list_head *head)
{
        _lc_list_del(list->prev, list->next);
        lc_list_add_tail(list, head);
}

/**
 * lc_list_empty - tests whether a list is empty
 * @head: the list to test.
 */
static LC_INLINE int lc_list_empty(const struct lc_list_head *head)
{
	return head->next == head;
}

static LC_INLINE void _lc_list_splice(struct lc_list_head *list,
				 struct lc_list_head *head)
{
	struct lc_list_head *first = list->next;
	struct lc_list_head *last = list->prev;
	struct lc_list_head *at = head->next;

	first->prev = head;
	head->next = first;

	last->next = at;
	at->prev = last;
}

/**
 * lc_list_splice - join two lists
 * @list: the new list to add.
 * @head: the place to add it in the first list.
 */
static LC_INLINE void lc_list_splice(struct lc_list_head *list, struct lc_list_head *head)
{
	if (!lc_list_empty(list))
		_lc_list_splice(list, head);
}

/**
 * lc_list_splice_init - join two lists and reinitialise the emptied list.
 * @list: the new list to add.
 * @head: the place to add it in the first list.
 *
 * The list at @list is reinitialised
 */
static LC_INLINE void lc_list_splice_init(struct lc_list_head *list,
				    struct lc_list_head *head)
{
	if (!lc_list_empty(list)) {
		_lc_list_splice(list, head);
		LC_INIT_LIST_HEAD(list);
	}
}

/**
 * lc_list_entry - get the struct for this entry
 * @ptr:	the &struct lc_list_head pointer.
 * @type:	the type of the struct this is embedded in.
 * @member:	the name of the lc_list_struct within the struct.
 */
#define lc_list_entry(ptr, type, member) \
	_lc_list_container_of(ptr, type, member)

/**
 * lc_list_for_each	-	iterate over a list
 * @pos:	the &struct lc_list_head to use as a loop counter.
 * @head:	the head for your list.
 */
#define lc_list_for_each(pos, head) \
	for (pos = (head)->next, (pos->next); pos != (head); \
        	pos = pos->next, (pos->next))

/**
 * _lc_list_for_each	-	iterate over a list
 * @pos:	the &struct lc_list_head to use as a loop counter.
 * @head:	the head for your list.
 *
 * This variant differs from lc_list_for_each() in that it's the
 * simplest possible list iteration code, no ing is done.
 * Use this for code that knows the list to be very short (empty
 * or 1 entry) most of the time.
 */
#define _lc_list_for_each(pos, head) \
	for (pos = (head)->next; pos != (head); pos = pos->next)

/**
 * lc_list_for_each_prev	-	iterate over a list backwards
 * @pos:	the &struct lc_list_head to use as a loop counter.
 * @head:	the head for your list.
 */
#define lc_list_for_each_prev(pos, head) \
	for (pos = (head)->prev, (pos->prev); pos != (head); \
        	pos = pos->prev, (pos->prev))
        	
/**
 * lc_list_for_each_safe	-	iterate over a list safe against removal of list entry
 * @pos:	the &struct lc_list_head to use as a loop counter.
 * @n:		another &struct lc_list_head to use as temporary storage
 * @head:	the head for your list.
 */
#define lc_list_for_each_safe(pos, n, head) \
	for (pos = (head)->next, n = pos->next; pos != (head); \
		pos = n, n = pos->next)

/**
 * lc_list_for_each_entry	-	iterate over list of given type
 * @pos:	the type * to use as a loop counter.
 * @head:	the head for your list.
 * @member:	the name of the lc_list_struct within the struct.
 */
#define lc_list_for_each_entry(type, pos, head, member)				\
	for (pos = lc_list_entry((head)->next, type, member);	\
	     &pos->member != (head); 					\
	     pos = lc_list_entry(pos->member.next, type, member))	

/**
 * lc_list_for_each_entry_reverse - iterate backwards over list of given type.
 * @pos:	the type * to use as a loop counter.
 * @head:	the head for your list.
 * @member:	the name of the lc_list_struct within the struct.
 */
#define lc_list_for_each_entry_reverse(type, pos, head, member)			\
	for (pos = lc_list_entry((head)->prev, type, member);	\
	     &pos->member != (head); 					\
	     pos = lc_list_entry(pos->member.prev, type, member))	


/**
 * lc_list_for_each_entry_safe - iterate over list of given type safe against removal of list entry
 * @pos:	the type * to use as a loop counter.
 * @n:		another type * to use as temporary storage
 * @head:	the head for your list.
 * @member:	the name of the lc_list_struct within the struct.
 */
#define lc_list_for_each_entry_safe(type, pos, n, head, member)			\
	for (pos = lc_list_entry((head)->next, type, member),	\
		n = lc_list_entry(pos->member.next, type, member);	\
	     &pos->member != (head); 					\
	     pos = n, n = lc_list_entry(n->member.next, type, member))


#endif /* _LC_LIST_H */
