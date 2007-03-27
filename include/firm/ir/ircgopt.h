/*
 * Project:     libFIRM
 * File name:   ir/ir/ircgopt.h
 * Purpose:     Removal of unreachable methods. 
 * Author:      Hubert Schmid
 * Modified by:
 * Created:     09.06.2002
 * CVS-ID:      $Id$
 * Copyright:   (c) 2002-2003 Universit�t Karlsruhe
 * Licence:     This file protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 */

/**
 * @file irgopt.h
 *
 * Entfernen von nicht erreichbaren (aufrufbaren) Methoden. Die Menge
 * der nicht erreichbaren Methoden wird aus der Absch�tzung der
 * Aufrufrelation bestimmt.
 */
#ifndef _FIRM_IR_ICGOPT_H_
#define _FIRM_IR_ICGOPT_H_

#include "firm_types.h"

/* Entfernt alle Methoden, die von den Methoden aus "keep_arr"
 * (bezgl. der Abschaetzung get_Call_callee) nicht erreichbar sind. Die
 * Abschaetzung der Aufrufrelation muss entsprechend an den
 * Call-Operationen gespeichert sein. Die "entity->link"s werden dabei
 * ueberschrieben. 
 *
 * Frees all interprocedural loop information. */
void gc_irgs(int n_keep, ir_entity *keep_arr[]);

#endif /* _FIRM_IR_ICGOPT_H_ */
