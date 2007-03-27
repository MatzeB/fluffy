/*
 * Project:     firmlower
 * File name:   src/lower_cast.h
 * Purpose:     lower cast nodes according to varying semantics. 
 * Author:      Goetz Lindenmaier
 * Modified by:
 * Created:     12.08.2003
 * CVS-ID:      $Id$
 * Copyright:   (c) 2003 Universität Karlsruhe. All rights reserved.
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

#ifndef __LOWER_CAST_H__
#define __LOWER_CAST_H__


/**
 * Simply removes all cast nodes. 
 * 
 * Sets type field in all constants (in the code) to unknown_type to increase the 
 * number of cse's. 
 */
void remove_casts(void);


#endif /* __LOWER_CAST_H__ */
