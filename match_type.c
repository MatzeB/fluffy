#include <config.h>

#include "match_type.h"

#include <assert.h>

#include "ast_t.h"
#include "semantic_t.h"

static inline
void match_error(semantic_env_t *env, type_t *variant, type_t *concrete,
                 const source_position_t source_position)
{
	print_error_prefix(env, source_position);
	fprintf(stderr, "can't match variant type ");
	print_type(stderr, variant);
	fprintf(stderr, " against ");
	print_type(stderr, concrete);
	fprintf(stderr, "\n");
}

void match_variant_to_concrete_type(semantic_env_t *env,
                                    type_t *variant_type,
                                    type_t *concrete_type,
                                    const source_position_t source_position)
{
	type_reference_t *type_ref;
	type_variable_t  *type_var;
	type_t           *current_type;
	pointer_type_t   *pointer_type_1;
	pointer_type_t   *pointer_type_2;
	method_type_t    *method_type_1;
	method_type_t    *method_type_2;

	assert(type_valid(variant_type));
	assert(type_valid(concrete_type));

	switch(variant_type->type) {
	case TYPE_REFERENCE_TYPE_VARIABLE:
		type_ref     = (type_reference_t*) variant_type;
		type_var     = type_ref->r.type_variable;
		current_type = type_var->current_type;
		if(current_type == NULL) {
			type_var->current_type = concrete_type;
		} else if(current_type != concrete_type) {
			print_error_prefix(env, source_position);
			fprintf(stderr, "ambiguous matches found for type variable '%s': ",
			        type_var->symbol->string);
			print_type(stderr, current_type);
			fprintf(stderr, ", ");
			print_type(stderr, concrete_type);
			fprintf(stderr, "\n");
			return;
		}
		break;

	case TYPE_VOID:
	case TYPE_STRUCT:
	case TYPE_ATOMIC:
		if(concrete_type != variant_type) {
			match_error(env, variant_type, concrete_type, source_position);
		}
		break;

	case TYPE_POINTER:
		if(concrete_type->type != TYPE_POINTER) {
			match_error(env, variant_type, concrete_type, source_position);
			break;
		}
		pointer_type_1 = (pointer_type_t*) variant_type;
		pointer_type_2 = (pointer_type_t*) concrete_type;
		match_variant_to_concrete_type(env, pointer_type_1->points_to,
		                               pointer_type_2->points_to,
		                               source_position);
		break;

	case TYPE_METHOD:
		if(concrete_type->type != TYPE_METHOD) {
			match_error(env, variant_type, concrete_type, source_position);
			break;
		}
		method_type_1 = (method_type_t*) variant_type;
		method_type_2 = (method_type_t*) concrete_type;
		match_variant_to_concrete_type(env, method_type_1->result_type,
		                               method_type_2->result_type,
		                               source_position);

		method_parameter_type_t *param1 = method_type_1->parameter_types;
		method_parameter_type_t *param2 = method_type_2->parameter_types;
		while(param1 != NULL && param2 != NULL) {
			match_variant_to_concrete_type(env, param1->type, param2->type,
			                               source_position);

			param1 = param1->next;
			param2 = param2->next;
		}
		if(param1 != NULL || param2 != NULL) {
			match_error(env, variant_type, concrete_type, source_position);
			break;
		}

		break;
		
	default:
		abort();
	}
}

