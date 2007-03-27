#include <config.h>

#include <firm/common/firm.h>
#include <firm/be/be.h>

#include "ast_t.h"

static
ir_node *uninitialized_local_var(ir_graph *irg, ir_mode *mode, int pos)
{
	(void) irg;
	(void) pos;
	/* TODO generate warning */
	return new_Unknown(mode);
}

void initialize_firm(void)
{
	const backend_params *be_params;
	firm_parameter_t params;
	memset(&params, 0, sizeof(params));

#if 0
	/* read firm options */
	firm_init_options("mlang", 0, NULL);
#endif

	params.size = sizeof(params);
	params.enable_statistics = 0;
	params.initialize_local_func = uninitialized_local_var;
	params.cc_mask = 0;
	params.builtin_dbg = NULL;

	/* initialize backend */
	be_params = be_init();
	params.arch_op_settings = be_params->arch_op_settings;

	/* intialize firm itself */
	init_firm(&params);
}

void exit_firm(void)
{
}

/**
 * Build a firm representation of an AST programm
 */
void ast2firm(compilation_unit_t *unit)
{
	/* scan compilation unit for functions */
	environment_entry_t *entry = unit->environment.entries;
	for( ; entry != NULL; entry = entry->next) {
		if(entry->type == ENTRY_FUNCTION) {
			/* first create an entity */
			ir_type *glob_type = get_glob_type();
			ident *id = new_id_from_str(entry->symbol->string);
			ir_type *methodtype = new_type_method(id, 0, 0);
			/*TODO
			 * set_method_res_type(methodtype, 0, int_type); */
			ir_entity *entity = new_entity(glob_type, id, methodtype);
			set_entity_ld_ident(entity, id);
			set_entity_visibility(entity, visibility_external_visible);

			ir_graph *irg = new_ir_graph(entity, 0);

			ir_node *ret = new_Return(get_store(), 0, NULL);

			mature_immBlock(get_irg_current_block(irg));
			add_immBlock_pred(get_irg_end_block(irg), ret);
			mature_immBlock(get_irg_end_block(irg));

			irg_finalize_cons(irg);

			irg_vrfy(irg);

			dump_ir_block_graph(irg, "-test");
		}
	}
}

