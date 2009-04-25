#include <config.h>

#include "match_type.h"

#include <assert.h>

#include "type_t.h"
#include "ast_t.h"
#include "semantic_t.h"
#include "type_hash.h"
#include "adt/error.h"

static inline void match_error(type_t *variant, type_t *concrete,
                               const source_position_t source_position)
{
	print_error_prefix(source_position);
	fprintf(stderr, "can't match variant type ");
	print_type(variant);
	fprintf(stderr, " against ");
	print_type(concrete);
	fprintf(stderr, "\n");
}

static bool matched_type_variable(type_variable_t *type_variable, type_t *type,
                                  const source_position_t source_position,
								  bool report_errors)
{
	type_t *current_type = type_variable->current_type;
	if (current_type != NULL && current_type != type) {
		if (report_errors) {
			print_error_prefix(source_position);
			fprintf(stderr, "ambiguous matches found for type variable '%s': ",
					type_variable->declaration.symbol->string);
			print_type(current_type);
			fprintf(stderr, ", ");
			print_type(type);
			fprintf(stderr, "\n");
		}
		/* are both types normalized? */
		assert(typehash_contains(current_type));
		assert(typehash_contains(type));
		return false;
	}
	type_variable->current_type = type;

	return true;
}

static bool match_compound_type(compound_type_t *variant_type,
                                type_t *concrete_type,
                                const source_position_t source_position,
								bool report_errors)
{
	type_variable_t *type_parameters = variant_type->type_parameters;
	if (type_parameters == NULL) {
		if (concrete_type != (type_t*) variant_type) {
			if (report_errors)
				match_error((type_t*) variant_type, concrete_type,
				            source_position);
			return false;
		}
		return true;
	}

	if (concrete_type->type != TYPE_BIND_TYPEVARIABLES) {
		if (report_errors)
			match_error((type_t*) variant_type, concrete_type, source_position);
		return false;
	}
	bind_typevariables_type_t *bind_typevariables 
		= (bind_typevariables_type_t*) concrete_type;
	compound_type_t           *polymorphic_type 
		= bind_typevariables->polymorphic_type;
	if (polymorphic_type != variant_type) {
		if (report_errors)
			match_error((type_t*) variant_type, concrete_type, source_position);
		return false;
	}

	type_variable_t *type_parameter = type_parameters;
	type_argument_t *type_argument  = bind_typevariables->type_arguments;
	bool             result         = true;
	while (type_parameter != NULL) {
		assert(type_argument != NULL);

		if (!matched_type_variable(type_parameter, type_argument->type,
		                          source_position, true))
			result = false;

		type_parameter = type_parameter->next;
		type_argument  = type_argument->next;
	}
	return result;
}

static bool match_bind_typevariables(bind_typevariables_type_t *variant_type,
                                     type_t *concrete_type,
                                     const source_position_t source_position,
									 bool report_errors)
{
	if (concrete_type->type != TYPE_BIND_TYPEVARIABLES) {
		if (report_errors)
			match_error((type_t*) variant_type, concrete_type, source_position);
		return false;
	}

	bind_typevariables_type_t *bind_typevariables
		= (bind_typevariables_type_t*) concrete_type;
	compound_type_t *polymorphic_type
		= bind_typevariables->polymorphic_type;
	if (polymorphic_type != variant_type->polymorphic_type) {
		if (report_errors)
			match_error((type_t*) variant_type, concrete_type, source_position);
		return false;
	}

	type_argument_t *argument1 = variant_type->type_arguments;
	type_argument_t *argument2 = bind_typevariables->type_arguments;
	bool             result    = true;
	while (argument1 != NULL) {
		assert(argument2 != NULL);

		if (!match_variant_to_concrete_type(argument1->type,
		                                   argument2->type, source_position,
										   report_errors))
			result = false;

		argument1 = argument1->next;
		argument2 = argument2->next;
	}
	assert(argument2 == NULL);
	return result;
}

bool match_variant_to_concrete_type(type_t *variant_type,
                                    type_t *concrete_type,
                                    const source_position_t source_position,
									bool report_errors)
{
	type_reference_t *type_ref;
	type_variable_t  *type_var;
	pointer_type_t   *pointer_type_1;
	pointer_type_t   *pointer_type_2;
	method_type_t    *method_type_1;
	method_type_t    *method_type_2;

	assert(type_valid(variant_type));
	assert(type_valid(concrete_type));

	variant_type = skip_typeref(variant_type);
	concrete_type = skip_typeref(concrete_type);

	switch(variant_type->type) {
	case TYPE_REFERENCE_TYPE_VARIABLE:
		type_ref     = (type_reference_t*) variant_type;
		type_var     = type_ref->type_variable;
		return matched_type_variable(type_var, concrete_type, source_position,
		                             report_errors);

	case TYPE_VOID:
	case TYPE_ATOMIC:
		if (concrete_type != variant_type) {
			if (report_errors)
				match_error(variant_type, concrete_type, source_position);
			return false;
		}
		return true;

	case TYPE_COMPOUND_CLASS:
	case TYPE_COMPOUND_STRUCT:
	case TYPE_COMPOUND_UNION:
		return match_compound_type((compound_type_t*) variant_type,
		                           concrete_type, source_position,
								   report_errors);

	case TYPE_POINTER:
		if (concrete_type->type != TYPE_POINTER) {
			if (report_errors)
				match_error(variant_type, concrete_type, source_position);
			return false;
		}
		pointer_type_1 = (pointer_type_t*) variant_type;
		pointer_type_2 = (pointer_type_t*) concrete_type;
		return match_variant_to_concrete_type(pointer_type_1->points_to,
		                                      pointer_type_2->points_to,
		                                      source_position,
					       				   report_errors);

	case TYPE_METHOD:
		if (concrete_type->type != TYPE_METHOD) {
			if (report_errors)
				match_error(variant_type, concrete_type, source_position);
			return false;
		}
		method_type_1 = (method_type_t*) variant_type;
		method_type_2 = (method_type_t*) concrete_type;
		bool result = match_variant_to_concrete_type(method_type_1->result_type,
		                               method_type_2->result_type,
		                               source_position,
									   report_errors);

		method_parameter_type_t *param1 = method_type_1->parameter_types;
		method_parameter_type_t *param2 = method_type_2->parameter_types;
		while (param1 != NULL && param2 != NULL) {
			if (!match_variant_to_concrete_type(param1->type, param2->type,
			                               source_position, report_errors))
				result = false;

			param1 = param1->next;
			param2 = param2->next;
		}
		if (param1 != NULL || param2 != NULL) {
			if (report_errors)
				match_error(variant_type, concrete_type, source_position);
			return false;
		}
		return result;
	case TYPE_BIND_TYPEVARIABLES:
		return match_bind_typevariables(
		        (bind_typevariables_type_t*) variant_type,
		        concrete_type, source_position, report_errors);
	case TYPE_ARRAY:
		panic("TODO");
	case TYPE_ERROR:
		return false;
	case TYPE_TYPEOF:
	case TYPE_REFERENCE:
		panic("type reference not resolved in match variant to concrete type");
	case TYPE_INVALID:
		panic("invalid type in match variant to concrete type");
	}
	panic("unknown type in match variant to concrete type");
}

