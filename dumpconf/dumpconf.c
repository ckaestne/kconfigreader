/*
 * Copyright (C) 2009 Reinhard Tartler
 * Released under the terms of the GNU GPL v2.0.
 */

#include <locale.h>
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <unistd.h>
#include <sys/stat.h>
#include <sys/time.h>

#define LKC_DIRECT_LINK
#include "lkc.h"

#if 0

void my_expr_print(struct expr *e, void (*fn)(void *, struct symbol *, const char *), void *data, int prevtoken)
{
	static char buf[20];
	snprintf(buf, sizeof buf, "CHOICE_%d", choice_count);
	choicestring = buf;
	if (!e) {
		fn(data, NULL, "y");
		return;
	}

	if (expr_compare_type(prevtoken, e->type) > 0)
	fn(data, NULL, "(");
	switch (e->type) {
	case E_SYMBOL:
		if (e->left.sym->name){
			if (isConstant(e->left.sym))
				fn(data, NULL, "'");
			fn(data, e->left.sym, e->left.sym->name);
			if (isConstant(e->left.sym))
				fn(data, NULL, "'");
		}else if (e->left.sym == modules_sym)
                        fn(data, NULL, "MODULES");
                else if (sym_is_choice(e->left.sym))
			fn(data, NULL, choicestring);
                else
			fn(data, NULL, "UNKNOWN_NODE");
		break;
	case E_NOT:
		fn(data, NULL, "!");
		my_expr_print(e->left.expr, fn, data, E_NOT);
		break;
	case E_EQUAL:
		if (e->left.sym->name){
			if (isConstant(e->left.sym))
				fn(data, NULL, "'");
			fn(data, e->left.sym, e->left.sym->name);
			if (isConstant(e->left.sym))
				fn(data, NULL, "'");
		}else
			fn(data, NULL, "<choice>");
		fn(data, NULL, "=");
		if (isConstant(e->right.sym))
			fn(data, NULL, "'");
		fn(data, e->right.sym, e->right.sym->name);
		if (isConstant(e->right.sym))
			fn(data, NULL, "'");
		break;
	case E_UNEQUAL:
		if (e->left.sym->name){
			if (isConstant(e->left.sym))
				fn(data, NULL, "'");
			fn(data, e->left.sym, e->left.sym->name);
			if (isConstant(e->left.sym))
				fn(data, NULL, "'");
		}else
			fn(data, NULL, "<choice>");
		fn(data, NULL, "!=");
		if (isConstant(e->right.sym))
			fn(data, NULL, "'");
		fn(data, e->right.sym, e->right.sym->name);
		if (isConstant(e->right.sym))
			fn(data, NULL, "'");
		break;
	case E_OR:
		my_expr_print(e->left.expr, fn, data, E_OR);
		fn(data, NULL, " || ");
		my_expr_print(e->right.expr, fn, data, E_OR);
		break;
	case E_AND:
		my_expr_print(e->left.expr, fn, data, E_AND);
		fn(data, NULL, " && ");
		my_expr_print(e->right.expr, fn, data, E_AND);
		break;
	case E_LIST:
		fn(data, e->right.sym, e->right.sym->name);
		if (e->left.expr) {
		fn(data, NULL, " ^ ");
		my_expr_print(e->left.expr, fn, data, E_LIST);
		}
		break;
	case E_RANGE:
		fn(data, NULL, "[");
		fn(data, e->left.sym, e->left.sym->name);
		fn(data, NULL, " ");
		fn(data, e->right.sym, e->right.sym->name);
		fn(data, NULL, "]");
		break;
	default:
		{
		char buf[32];
		sprintf(buf, "<unknown type %d>", e->type);
		fn(data, NULL, buf);
		break;
		}
	}
	if (expr_compare_type(prevtoken, e->type) > 0)
	fn(data, NULL, ")");
}

static void my_expr_print_file_helper(void *data, struct symbol *sym, const char *str)
{
	xfwrite(str, strlen(str), 1, data);
}

void my_expr_fprint(struct expr *e, FILE *out)
{
	my_expr_print(e, my_expr_print_file_helper, out, E_NONE);
}

void my_print_symbol(FILE *out, struct menu *menu)
{
	struct symbol *sym = menu->sym;
	struct property *prop;
	static char buf[50];
	tristate is_tristate = no;
        int isChoiceItem = 0;

	for (prop = sym->prop; prop; prop = prop->next) {
		if (prop->menu != menu)
			continue;
		switch (prop->type) {
		case P_CHOICE:
			fputs("#choice value\n", out);
                        isChoiceItem = 1;
			break;
#if 0
                case P_DEFAULT:
                    fprintf(out, "#default\t%s\t%s\t\"", sym->name, prop->text);
                    expr_fprint(prop->visible.expr, out);
                    fprintf(out, "\"\n");
                    break;
                case P_SELECT:
                    fprintf(out, "#select\t%s\t\"", sym->name);
                    expr_fprint(prop->expr, out);
                    fprintf(out, "\"\t\"");
                    expr_fprint(prop->visible.expr, out);
                    fprintf(out, "\"\n");
                    break;
                case P_PROMPT:
		    fprintf(out, "#prompt\t%s\t", prop->sym->name);
		    expr_fprint(prop->visible.expr, out);
		    fprintf(out, "\n");
		    break;
#endif
		default:
//			fprintf(out, "  unknown prop %d!\n", prop->type);
			break;
		}
	}


	if (sym_is_choice(sym)) {
		char itemname[50];
		fprintf(out, "#startchoice\n");
		current_choice = menu;
		choice_count++;

		//unnamed choices get a generic id
                if (sym->name)
                        snprintf(itemname, sizeof itemname, "%s", sym->name);
                else {
                        snprintf(itemname, sizeof itemname, "CHOICE_%d", choice_count);
                }


		fprintf(out, "Choice\t%s", itemname);
		snprintf(buf, sizeof buf, itemname);

		// optional, i.e. all items can be deselected
		if (current_choice->sym->flags & SYMBOL_OPTIONAL)
			fprintf(out, "\toptional");
		else
			fprintf(out, "\trequired");

		if (current_choice->sym->type & S_TRISTATE)
			fprintf(out, "\ttristate");
		else
			fprintf(out, "\tboolean");

		fprintf(out, "\n");

	} else {
		if (isChoiceItem)
			fprintf(out, "ChoiceItem\t%s\t%s\n", sym->name, buf);

		fprintf(out, "Item\t%s", sym->name);
		switch (sym->type) {
		case S_BOOLEAN:
			fputs("\tboolean\n", out);
			is_tristate = yes;
			break;
		case S_TRISTATE:
			fputs("\ttristate\n", out);
			is_tristate = mod;
			break;
		case S_STRING:
			fputs("\tstring\n", out);
			break;
		case S_INT:
			fputs("\tinteger\n", out);
			break;
		case S_HEX:
			fputs("\thex\n", out);
			break;
		default:
			fputs("\t???\n", out);
			break;
		}
	}

	//if (menu->dep || is_tristate != no) {
		char itemname[50];
		int has_prompts = 0;

		if (sym->name)
			snprintf(itemname, sizeof itemname, "%s", sym->name);
		else if (sym_is_choice(sym)) {
			snprintf(itemname, sizeof itemname, "CHOICE_%d", choice_count);
			choicestring = buf;
		} else {
			snprintf(itemname, sizeof itemname, "%s", "UNKNOWN_NODE");
                }
		if (menu->dep) {
		    fprintf(out, "Depends\t%s\t\"", itemname);
		    my_expr_fprint(menu->dep, out);
		    fprintf(out, "\"\n");
		}

		for_all_prompts(sym, prop) {
			has_prompts++;
			fprintf(out, "Prompt\t%s\t\"",itemname);
			my_expr_fprint(prop->visible.expr, out);
			fprintf(out, "\"\n");
		}

		fprintf(out, "HasPrompts\t%s\t%d\n", itemname, has_prompts);

		for_all_properties(sym, prop, P_DEFAULT) {
			fprintf(out, "Default\t%s\t\"", itemname);
			my_expr_fprint(prop->expr, out);
			fprintf(out, "\"\t\"");
			my_expr_fprint(prop->visible.expr, out);
			fprintf(out, "\"\n");
		}
		for_all_properties(sym, prop, P_SELECT) {
			fprintf(out, "ItemSelects\t%s\t\"", itemname);
			my_expr_fprint(prop->expr, out);
			fprintf(out, "\"\t\"");
			my_expr_fprint(prop->visible.expr, out);
			fprintf(out, "\"\n");
		}
		for_all_properties(sym, prop, P_RANGE) {
			fprintf(out, "Range\t%s\t\"", itemname);
			my_expr_fprint(prop->expr, out);
			fprintf(out, "\"\t\"");
			my_expr_fprint(prop->visible.expr, out);
			fprintf(out, "\"\n");
		}
	//}

	for (prop = sym->prop; prop; prop = prop->next) {
		if (prop->menu != menu)
			continue;
		switch (prop->type) {
		case P_CHOICE:
			fputs("#choice value\n", out);
			break;
#if 0
                case P_DEFAULT:
                    fprintf(out, "#default\t%s\t%s\t\"", sym->name, prop->text);
                    expr_fprint(prop->visible.expr, out);
                    fprintf(out, "\"\n");
                    break;
                case P_SELECT:
                    fprintf(out, "#select\t%s\t\"", sym->name);
                    expr_fprint(prop->expr, out);
                    fprintf(out, "\"\t\"");
                    expr_fprint(prop->visible.expr, out);
                    fprintf(out, "\"\n");
                    break;
                case P_PROMPT:
		    fprintf(out, "#prompt\t%s\t", prop->sym->name);
		    expr_fprint(prop->visible.expr, out);
		    fprintf(out, "\n");
		    break;
#endif
		default:
//			fprintf(out, "  unknown prop %d!\n", prop->type);
			break;
		}
	}
}

#endif

char* getSymType(enum symbol_type t) {
	switch (t) {
		case S_UNKNOWN: return "unknown";
		case S_BOOLEAN: return "boolean";
		case S_TRISTATE: return "tristate";
		case S_INT: return "int";
		case S_HEX: return "hex";
		case S_STRING: return "string";
		case S_OTHER: return "other";
	}
	return "?";
}

char* getPropType(enum prop_type t) {
	switch (t) {
		case P_UNKNOWN: return "unknown";
   		case P_PROMPT: return "prompt";
        case P_COMMENT: return "comment";
        case P_MENU: return "menu";
        case P_DEFAULT: return "default";
        case P_CHOICE: return "choice";
        case P_SELECT: return "select";
        case P_RANGE: return "range";
        case P_ENV: return "env";
        case P_SYMBOL: return "symbol";
	}
	return "?";
}


void dumpsymref(FILE *out, struct symbol *s) {
	if (s==&symbol_mod) 
		fprintf(out, "m");
	else if (s==&symbol_yes) 
		fprintf(out, "y");
	else if (s==&symbol_no) 
		fprintf(out, "n");
	else if ((s->flags & SYMBOL_CONST)) 
		fprintf(out, "\"%s\"", s->name);
	else
		fprintf(out, "S@%d", s);
}

void dumpexpr(FILE *out, struct expr *e) {


	switch (e->type) {
	case E_SYMBOL:
		dumpsymref(out, e->left.sym);
		// if (e->left.sym->name){
		// 	if (isConstant(e->left.sym))
		// 		fn(data, NULL, "'");
		// 	fn(data, e->left.sym, e->left.sym->name);
		// 	if (isConstant(e->left.sym))
		// 		fn(data, NULL, "'");
		// }else if (e->left.sym == modules_sym)
  //                       fn(data, NULL, "MODULES");
  //               else if (sym_is_choice(e->left.sym))
		// 	fn(data, NULL, choicestring);
  //               else
		// 	fn(data, NULL, "UNKNOWN_NODE");
		break;
	case E_NOT:
		fprintf(out, "!");
		dumpexpr(out, e->left.expr);
		break;
	case E_EQUAL:
		fprintf(out, "(");
		dumpsymref(out, e->left.sym);
		fprintf(out, "=");
		dumpsymref(out, e->right.sym);
		fprintf(out, ")");
		break;
	case E_UNEQUAL:
		fprintf(out, "(");
		dumpsymref(out, e->left.sym);
		fprintf(out, "!=");
		dumpsymref(out, e->right.sym);
		fprintf(out, ")");
		break;
	case E_OR:
		fprintf(out, "(");
		dumpexpr(out, e->left.expr);
		fprintf(out, " || ");
		dumpexpr(out, e->right.expr);
		fprintf(out, ")");
		break;
	case E_AND:
		fprintf(out, "(");
		dumpexpr(out, e->left.expr);
		fprintf(out, " &amp;&amp; ");
		dumpexpr(out, e->right.expr);
		fprintf(out, ")");
		break;
	case E_LIST:
		fprintf(out, "(");
		dumpsymref(out, e->right.sym);
		fprintf(out, " ^ ");
		dumpexpr(out, e->left.expr);
		fprintf(out, ")");
		break;
	case E_RANGE:
		fprintf(out, "[");
		dumpsymref(out, e->left.sym);
		fprintf(out, "!=");
		dumpsymref(out, e->right.sym);
		fprintf(out, "]");
		break;
	default:
		{
		fprintf(out, "<unknown expr type %d>", e->type);
		break;
		}
	}
}


void dumpprop(FILE *out, struct property *prop) {
	fprintf(out, "<property type=\"%s\">",getPropType(prop->type));
	if (prop->text)	
    	fprintf(out, "<text>%s</text>", prop->text);
	if (prop->expr)	{
       	fprintf(out, "<expr>");
       	dumpexpr(out, prop->expr);
     	fprintf(out, "</expr>");
    }
	if (prop->visible.expr)	{
       	fprintf(out, "<visible><expr>");
       	dumpexpr(out, prop->visible.expr);
     	fprintf(out, "</expr></visible>");
    }

	fprintf(out, "</property>\n");
}


void dumpsymbol(FILE *out, struct symbol *sym) {
	struct property *prop;
	while (sym) {
		fprintf(out, "<symbol type=\"%s\" flags=\"%d\" id=\"%d\">\n", getSymType(sym->type), sym->flags, sym);

		if (sym->name)	
       		fprintf(out, "<name>%s</name>\n", sym->name);

       	for (prop = sym->prop; prop; prop = prop->next) {
       		dumpprop(out, prop);
       	}

		fprintf(out, "</symbol>\n");
		sym = sym->next;
	}
}

void dumpmenu(FILE *out, struct menu *menu) {
	struct property *prop;
	struct symbol *sym;

	fprintf(out, "<menu>\n");
	if ((sym = menu->sym))
			dumpsymbol(out, sym);
	if ((prop = menu->prompt)) {
			dumpprop(out, prop);
	}
	if (menu->dep) {
		fprintf(out, "<dep>");
		dumpexpr(out, menu->dep);
		fprintf(out, "</dep>");
	}

	fprintf(out, "</menu>\n");
}

void myconfdump(FILE *out)
{
	struct menu *menu;

	menu = rootmenu.list;
	fprintf(out, "<submenu>\n");
	while (menu) {
		dumpmenu(out, menu);

		if (menu->list) {
			fprintf(out, "<submenu>\n");
			menu = menu->list;
		}
		else if (menu->next) {
			menu = menu->next;
		}
		else while ((menu = menu->parent)) {
			fprintf(out, "</submenu>\n");
			if (menu->next) {
				menu = menu->next;
				break;
			}
		}
	}
}

int main(int ac, char **av)
{
	struct stat tmpstat;
	char *arch = getenv("ARCH");

	setlocale(LC_ALL, "");
	bindtextdomain(PACKAGE, LOCALEDIR);
	textdomain(PACKAGE);

	if (stat(av[1], &tmpstat) != 0) {
		fprintf(stderr, "could not open %s\n", av[1]);
		exit(EXIT_FAILURE);
	}

	if (!arch) {
		fputs("setting arch", stderr);
		arch = strdup ("x86");
	}
	fprintf(stderr, "using arch %s\n", arch);
	setenv("ARCH", arch, 1);
	setenv("KERNELVERSION", "2.6.30-vamos", 1);
	conf_parse(av[1]);
	myconfdump(stdout);
	return 0;
}
