/*
 * Project:     libFIRM
 * File name:   ir/common/ident_t.h
 * Purpose:     Data type for unique names.
 * Author:      Goetz Lindenmaier
 * Modified by:
 * Created:
 * CVS-ID:      $Id$
 * Copyright:   (c) 1999-2003 Universit�t Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */
/**
 * @file ident.h
 *
 * Declarations for identifiers in the firm library
 *
 * Identifiers are used in the firm library. This is the interface to it.
 */
#ifndef _IDENT_H_
#define _IDENT_H_

#include "firm_config.h"

#ifdef FIRM_ENABLE_WCHAR
#include <wchar.h>
#endif

#ifdef __cplusplus
extern "C" {
#endif

/* Identifiers */

/**
 *  The abstract data type ident.
 *
 *  An ident represents an unique string. The == operator
 *  is sufficient to compare two idents.
 */
#ifndef _IDENT_TYPEDEF_
#define _IDENT_TYPEDEF_
typedef const struct _ident ident;
#endif 

/**
 * The ident module interface.
 */
typedef struct _ident_if_t {
  /** The handle. */
  void *handle;
 
  /**
   * Store a string and create an ident.
   * This function may be NULL, new_id_from_chars()
   * is then used to emulate it's behavior.
   * 
   * @param str - the string which shall be stored
   */
  ident *(*new_id_from_str)(void *handle, const char *str);

  /**
   * Store a string and create an ident.
   *
   * @param str - the string (or whatever) which shall be stored
   * @param len - the length of the data in bytes
   */
  ident *(*new_id_from_chars)(void *handle, const char *str, int len);

  /**
   * Returns a string represented by an ident.
   */
  const char *(*get_id_str)(void *handle, ident *id);

  /**
   * Returns the length of the string represented by an ident.
   * This function may be NULL, get_id_str() is then used
   * to emulate it's behavior.
   *
   * @param id - the ident
   */
  int  (*get_id_strlen)(void *handle, ident *id);

  /**
   * Finish the ident module and frees all idents, may be NULL.
   */
  void (*finish_ident)(void *handle);

#ifdef FIRM_ENABLE_WCHAR
  /**
   * Store a wide character string and create an ident.
   * This function may be NULL, new_id_from_wchars()
   * is then used to emulate it's behavior.
   * 
   * @param wstr - the string which shall be stored
   */
  ident *(*new_id_from_wcs)(void *handle, const wchar_t *wstr);

  /**
   * Store a wide character string and create an ident.
   * This function may be NULL, new_id_from_chars() is then used appropriate.
   * Beware: the string might not be stored at a right alignment!
   *
   * @param wstr - the wide character string which shall be stored
   * @param len  - the length of the string
   */
  ident *(*new_id_from_wchars)(void *handle, const wchar_t *wstr, int len);

  /**
   * Returns a wide character string represented by an ident.
   * This function may be NULL, get_id_str() is then used.
   * This assume that the strings are stored at an address aligned
   * for wchar_t, so beware!
   */
  const wchar_t *(*get_id_wcs)(void *handle, ident *id);

  /**
   * Returns the length of the string represented by an ident.
   * This function may be NULL, get_id_wcs() is then used
   * to emulate it's behavior.
   *
   * @param id - the ident
   */
  int  (*get_id_wcslen)(void *handle, ident *id);
#endif
} ident_if_t;

/**
 *  Store a string and create an ident.
 *
 *  Stores a string in the ident module and returns a handle for the string.
 *
 *  Copies the string. @p str must be zero terminated
 *
 * @param str - the string which shall be stored
 *
 * @return id - a handle for the generated ident
 *
 * @see get_id_str(), get_id_strlen()
 */
ident *new_id_from_str (const char *str);

/** Store a string and create an ident.
 *
 * Stores a string in the ident module and returns a handle for the string.
 * Copies the string. This version takes non-zero-terminated strings. 
 *
 * @param str - the string (or whatever) which shall be stored
 * @param len - the length of the data in bytes
 *
 * @return id - a handle for the generated ident
 *
 * @see new_id_from_str(), get_id_strlen()
 */
ident *new_id_from_chars (const char *str, int len);

/**
 * Returns a string represented by an ident.
 *
 * Returns the string represented by id. This string is
 * NULL terminated. The string may not be changed.
 *
 * @param id - the ident
 *
 * @return cp - a string
 *
 * @see new_id_from_str(), new_id_from_chars(), get_id_strlen()
 */
const char *get_id_str  (ident *id);

/**
 * Returns the length of the string represented by an ident.
 *
 * @param id - the ident
 *
 * @return len - the length of the string
 *
 * @see new_id_from_str(), new_id_from_chars(), get_id_str()
 */
int  get_id_strlen(ident *id);

/**
 * Returns true if prefix is a prefix of an ident.
 *
 * @param prefix - the prefix
 * @param id     - the ident
 *
 * @see new_id_from_str(), new_id_from_chars(), get_id_str(), id_is_prefix()
 */
int id_is_prefix (ident *prefix, ident *id);

/**
 * Returns true if suffix is a suffix of an ident.
 *
 * @param suffix - the suffix
 * @param id     - the ident
 *
 * @see new_id_from_str(), new_id_from_chars(), get_id_str(), id_is_prefix()
 */
int id_is_suffix (ident *suffix, ident *id);

/**
 * Returns true if infix is contained in id.  (Can be suffix or prefix)
 *
 * @param infix  - the infix
 * @param id     - the ident to search in
 *
 * @see new_id_from_str(), new_id_from_chars(), get_id_str(), id_is_prefix()
 */
/* int id_contains(ident *infix, ident *id); */

/**
 * Return true if an ident contains a given character.
 *
 * @param id     - the ident
 * @param c      - the character
 *
 * @see new_id_from_str(), new_id_from_chars(), get_id_str()
 */
int id_contains_char (ident *id, char c);

#ifdef FIRM_ENABLE_WCHAR
/**
 *  Store a wide character string and create an ident.
 *
 *  Stores a string in the ident module and returns a handle for the string.
 *
 *  Copies the string. @p str must be zero terminated
 *
 * @param str - the wide character string which shall be stored
 *
 * @return id - a handle for the generated ident
 *
 * @see get_id_wcs(), get_id_wcs()
 */
ident *new_id_from_wcs (const wchar_t *str);

/** Store a wide character string and create an ident.
 *
 * Stores a string in the ident module and returns a handle for the string.
 * Copies the string. This version takes non-zero-terminated strings. 
 *
 * @param wstr - the wide character string (or whatever) which shall be stored
 * @param len  - the length of string
 *
 * @return id - a handle for the generated ident
 *
 * @see new_id_from_str(), get_id_strlen()
 */
ident *new_id_from_wchars (const wchar_t *str, int len);

/**
 * Returns a wide character string represented by an ident.
 *
 * Returns the string represented by id. This string is
 * NULL terminated. The string may not be changed.
 *
 * @param id - the ident
 *
 * @return cp - a string
 *
 * @see new_id_from_wcs(), new_id_from_wchars(), get_id_wcslen()
 */
const wchar_t *get_id_wcs(ident *id);

/**
 * Returns the length of the wide character string represented by an ident.
 *
 * @param id - the ident
 *
 * @return len - the length of the string
 *
 * @see new_id_from_wcs(), new_id_from_wchars(), get_id_wcs()
 */
int  get_id_wcslen(ident *id);

/**
 * Return true if an ident contains a given character.
 *
 * @param id     - the ident
 * @param c      - the character
 *
 * @see new_id_from_wcs(), new_id_from_chars(), get_id_str()
 */
int id_contains_wchar (ident *id, wchar_t c);

#endif /* FIRM_ENABLE_WCHAR */

#ifdef __cplusplus
}
#endif

# endif /* _IDENT_H_ */

