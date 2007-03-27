/**
 * Author:      Matthias Braun
 * Date:		05.05.2006
 * Copyright:   (c) Universitaet Karlsruhe
 * License:     This file is protected by GPL -  GNU GENERAL PUBLIC LICENSE.
 *
 * Backend irg - a ir_graph with additional analysis information
 */
#ifndef BEIRG_H_
#define BEIRG_H_

#include "belive.h"
#include "beirgmod.h"

typedef struct _be_irg_t be_irg_t;

ir_graph *be_get_birg_irg(const be_irg_t *birg);

void be_assure_liveness(be_irg_t *birg);
void be_invalidate_liveness(be_irg_t *birg);
be_lv_t *be_get_birg_liveness(const be_irg_t *birg);

void be_assure_dom_front(be_irg_t *birg);
void be_invalidate_dom_front(be_irg_t *birg);
be_dom_front_info_t *be_get_birg_dom_front(const be_irg_t *birg);

ir_exec_freq *be_get_birg_exec_freq(const be_irg_t *birg);

/**
 * frees all memory allocated by birg structures (liveness, dom_front, ...).
 * The memory of the birg structure itself is not freed.
 */
void be_free_birg(be_irg_t *birg);

#endif

