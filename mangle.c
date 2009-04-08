#include <config.h>

#include <stdbool.h>
#include "mangle.h"
#include "ast_t.h"
#include <libfirm/firm.h>

static struct obstack obst;

static ident *make_id_from_obst(void)
{
	size_t  size = obstack_object_size(&obst);
	char   *str  = obstack_finish(&obst);
	ident  *id   = new_id_from_chars(str, size);
	obstack_free(&obst, str);
	return id;
}

/**
 * Mangles an entity linker (ld) name for win32 usage.
 *
 * @param ent          the entity to be mangled
 * @param declaration  the declaration
 */
ident *create_name_win32(declaration_t *declaration)
{
	struct obstack *o = &obst;

#if 0
	if (declaration->type == DECLARATION_METHOD) {
		if (declaration->declaration.modifiers & DM_DLLIMPORT)
			/* add prefix for imported symbols */
			obstack_printf(o, "__imp_");

		cc_kind_t cc = declaration->declaration.type->function.calling_convention;

		/* calling convention prefix */
		switch (cc) {
			case CC_DEFAULT:
			case CC_CDECL:
			case CC_STDCALL:  obstack_1grow(o, '_'); break;
			case CC_FASTCALL: obstack_1grow(o, '@'); break;
			default:          panic("unhandled calling convention");
		}

		switch (declaration->declaration.type->function.linkage) {
			case LINKAGE_INVALID:
				panic("linkage type of function is invalid");

			case LINKAGE_C:
				obstack_printf(o, "%s", declaration->base.symbol->string);
				break;

			case LINKAGE_CXX:
				mangle_entity(declaration);
				break;
		}

		/* calling convention suffix */
		switch (cc) {
			case CC_DEFAULT:
			case CC_CDECL:
				break;

			case CC_STDCALL:
			case CC_FASTCALL: {
				ir_type  *irtype = get_ir_type(declaration->declaration.type);
				unsigned size    = 0;
				for (int i = get_method_n_params(irtype) - 1; i >= 0; --i) {
					size += get_type_size_bytes(get_method_param_type(irtype, i));
				}
				obstack_printf(o, "@%u", size);
				break;
			}

			default:
				panic("unhandled calling convention");
		}
	} else {
		obstack_printf(o, "_%s", declaration->base.symbol->string);
	}
#endif
	obstack_printf(o, "_%s", declaration->symbol->string);

	return make_id_from_obst();
}

/**
 * Mangles an entity linker (ld) name for Linux ELF usage.
 *
 * @param ent          the entity to be mangled
 * @param declaration  the declaration
 */
ident *create_name_linux_elf(declaration_t *declaration)
{
#if 0
	bool needs_mangling = false;

	if (declaration->kind == ENTITY_FUNCTION) {
		switch (declaration->declaration.type->function.linkage) {
			case LINKAGE_INVALID:
				panic("linkage type of function is invalid");

			case LINKAGE_C:       break;
			case LINKAGE_CXX:     needs_mangling = true; break;
		}
	}

	if (needs_mangling) {
		mangle_entity(declaration);
		return make_id_from_obst();
	}
#endif

	return new_id_from_str(declaration->symbol->string);
}

/**
 * Mangles an entity linker (ld) name for Mach-O usage.
 *
 * @param ent          the entity to be mangled
 * @param declaration  the declaration
 */
ident *create_name_macho(declaration_t *declaration)
{
#if 0
	bool needs_mangling = false;
	if (declaration->kind == ENTITY_FUNCTION) {
		switch (declaration->declaration.type->function.linkage) {
			case LINKAGE_INVALID:
				panic("linkage type of function is invalid");

			case LINKAGE_C:       break;
			case LINKAGE_CXX:     needs_mangling = true; break;
		}
	}

	if (needs_mangling) {
		obstack_1grow(&obst, '_');
		mangle_entity(declaration);
		return make_id_from_obst();
	}
#endif

	obstack_printf(&obst, "_%s", declaration->symbol->string);
	return make_id_from_obst();
}

void init_mangle(void)
{
	obstack_init(&obst);
}

void exit_mangle(void)
{
	obstack_free(&obst, NULL);
}
