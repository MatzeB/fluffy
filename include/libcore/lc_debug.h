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
 * Debug facility.
 * @author Michael Beck, Sebastian Hack
 * @date 15.12.2004
 */

#ifndef _LIBCORE_DEBUG_H
#define _LIBCORE_DEBUG_H

#include <stdio.h>
#include <stdarg.h>
#include <obstack.h>

enum lc_dbg_level_t {
	LC_LEVEL_DEFAULT = 0,		/**< Prints always. Use with DBG(). */
	LC_LEVEL_1 = 1,
	LC_LEVEL_2 = 2,
	LC_LEVEL_3 = 4,
	LC_LEVEL_4 = 8,
	LC_LEVEL_5 = 16,

	LC_SET_LEVEL_0 = 0,			/**< use with lc_dbg_set_mask(). */
	LC_SET_LEVEL_1 = 1,
	LC_SET_LEVEL_2 = 3,
	LC_SET_LEVEL_3 = 7,
	LC_SET_LEVEL_4 = 15,
	LC_SET_LEVEL_5 = 31
};

enum _lc_dbg_flags_t {
	LC_DF_NONE      = 0,
	LC_DF_WITH_FILE = 1,
	LC_DF_WITH_LINE = 2,
	LC_DF_WITH_FUNC = 4,
	LC_DF_WITH_DATE = 8
};

typedef struct _lc_dbg_module_t lc_dbg_module_t;

/**
 * A dumper for a debug string.
 * The debug format string and arguments are passed to such a function. 
 * @param obst An obstack on which the message shall be printed.
 * @param fmt The format string.
 * @param args The arguments.
 */
typedef void (lc_dbg_printer_t)(struct obstack *obst, const char *fmt, va_list args);

/* Internal function to the debug module. */
void *_lc_dbg_make_msg(const lc_dbg_module_t *mod, unsigned mask, const char *fmt, ...);

/* Internal function to the debug module. */
void _lc_dbg_print_msg(const char *filename, int line, const char *func, void *data);

/* Internal function to the debug module. */
void _lc_dbg_print(const lc_dbg_module_t *mod, unsigned mask, const char *fmt, ...);

/**
 * Register a module to the firm debug facility.
 * If the module has already been registered, no new module is allocated
 * but the handle is returned. By default, all messages go to @c stderr
 * and the debug mask is set to 0, i.e. the module is muted.
 * @param name The name of the module to register.
 * @param printer The debug printer. If @c NULL is passed, the default
 * printer is chosen.
 * @return The module handle.
 */
lc_dbg_module_t *lc_dbg_register_with_printer(const char *name, lc_dbg_printer_t *printer);

/**
 * The same as lc_dbg_register_with_printer().
 * @c NULL is passed as the printer which selects the default printer.
 * @param name The name of the module.
 * @return The module.
 */
lc_dbg_module_t *lc_dbg_register(const char *name);

/** 
 * Set the mask of a module.
 * @param module The module.
 * @param mask The new mask for the module.
 */
void lc_dbg_set_mask(lc_dbg_module_t *module, unsigned mask);

/**
 * Get the mask of a module.
 * @param module The module handle.
 * @return The mask currently used by the module.
 */
unsigned lc_dbg_get_mask(const lc_dbg_module_t *module);

/**
 * Set the flags of the debug module.
 * @param module The debug module.
 * @param flags  The flags.
 */
void lc_dbg_set_flags(lc_dbg_module_t *module, unsigned flags);

/**
 * Get the flags of the debug module.
 * @param module The debug module.
 * @return The flags. 
 */
unsigned lc_dbg_get_flags(const lc_dbg_module_t *module);

/**
 * Set the output file of a module.
 * @param module The module handle.
 * @param file The new file to use by this handle.
 */
void lc_dbg_set_file(lc_dbg_module_t *module, FILE *file);

/**
 * Get the flags of the debug module.
 * @param module The debug module.
 * @return The flags. 
 */
FILE *lc_dbg_get_file(const lc_dbg_module_t *module);

#define _LC_DBG_MAIN(func,args) \
	_lc_dbg_print_msg(__FILE__, __LINE__, func, _lc_dbg_make_msg args)	

/* If we have C99 use the __func__ variable for calling functions name. */
#if defined(__STD_VERSION__) && __STD_VERSION >= 199901L
#define _LC_DBG(args) 	_LC_DBG_MAIN(__func__, args)
#else

/* Else, check for gcc and use the proprietary __FUNCTION__ macro. */
#ifdef __GNUC__
#define _LC_DBG(args)	_LC_DBG_MAIN(__FUNCTION__, args) 
#else

/* Else go without the name of the calling function. */
#define _LC_DBG(args) 	_LC_DBG_MAIN("", args)
#endif  /** __GNUC__ */
#endif

#define _LC_DB(args) _lc_dbg_print args

/**
 * Debug messages issued with this macro are always printed, even in
 * retail versions.
 * @see LC_DBG()
 */
#define LC_DBG_RETAIL(args)		_LC_DBG(args)

/**
 * Issue a debug message.
 * @param args The arguments.
 *
 * The arguments is a list surrounded by parentheses. The items
 * of the list are:
 * - The module handle as returned by lc_dbg_register().
 * - The debug mask that you want associate with this message.
 * - A format string for the message to pass to ir_printf().
 * - Further optional arguments are passed to ir_printf().
 *
 * The mask is anded against the module's mask. If both have some bits
 * in common, the message is issued. If the given mask is 0, the message
 * is always dumped regardless of the module's mask. You can also use
 * the mask in a level based manner, see lc_dbg_level_t.
 *
 * Here is an example:
 * @code
 * LC_DBG((my_mod, MASK_ERR, "ir node %n is not green", node))
 * ...
 * LC_DBG((my_mod, LEVEL_DEFAULT, "entity %e has type %t", ent, type))
 * @endcode
 */
#define LC_DBG(args) 					_LC_DBG(args)

/**
 * Issue a debug message.
 * @param args The arguments.
 *
 * Same as LC_DBG() but did not write the function, file and line number
 * where this macro was called.
 */
#define LC_DB(args) 					_LC_DB(args)

/**
 * The assert macro takes following parameters:
 * 1) The condition
 * 2) A format string
 * 3) The arguments for the format string
 *
 * Here's and example:
 * @code
 * LC_ASSERT((var > 0, "var must be greater than 0 and not %d", var));
 * @endcode
 */
#ifdef NDEBUG
#define LC_ASSERT(args)
#else
#define LC_ASSERT(args)                 _lc_assert args
#endif

#endif /* _LIBCORE_DEBUG_H */

