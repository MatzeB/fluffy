/*
 * Project:     firmlower
 * File name:   src/dispatch.h
 * Purpose:     handle all the dispatch stuff
 * Author:      Boris Boesler
 * Modified by:
 * Created:     10.07.2001
 * CVS-ID:      $Id$
 * Copyright:   (c) 2001 Universität Karlsruhe. All rights reserved.
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */


#ifndef __DISPATCH_H__
#define __DISPATCH_H__

typedef void (firmlower_BEGIN_OFFSET_FUNC) (ir_type*);
typedef void (firmlower_MEMBER_OFFSET_FUNC) (ir_entity*, ir_entity*);
typedef void (firmlower_END_OFFSET_FUNC) (ir_type*);


void lower_types_and_entities(const char *ObjectDescriptionPrefix,
			      const char *DispatchTableDescriptionPrefix,
			      const char *DispatchTablePrefix,
			      firmlower_BEGIN_OFFSET_FUNC begin,
			      firmlower_MEMBER_OFFSET_FUNC member,
			      firmlower_END_OFFSET_FUNC end);

#endif
