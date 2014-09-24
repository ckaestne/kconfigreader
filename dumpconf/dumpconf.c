/*
 * Copyright (C) 2014 Christian Kaestner
 * Inspired by prior version by Reinhart Tartler
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


char* getSymType(enum symbol_type t) {
	switch (t) {
		case S_UNKNOWN: return "unknown";
		case S_BOOLEAN: return "boolean";
		case S_TRISTATE: return "tristate";
		case S_INT: return "integer";
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
//        case P_SYMBOL: return "symbol";
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
		fprintf(out, "'%s'", s->name);
	else if (s->type==S_UNKNOWN)
		fprintf(out, "'%s'", s->name);
	else if (s->flags & SYMBOL_AUTO && !(s->flags & SYMBOL_CHOICE) && !(s->name)) //IGNORE
		fprintf(out, "IGNORE");
	else
		fprintf(out, "S@%d", s);
}

void dumpexpr(FILE *out, struct expr *e) {

if (!e) {fprintf(out, "ERROR"); return;}
	switch (e->type) {
	case E_SYMBOL:
		dumpsymref(out, e->left.sym);
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
		if (e->left.expr) {
			fprintf(out, " ^ ");
			dumpexpr(out, e->left.expr);
		}
		fprintf(out, ")");
		break;
	case E_RANGE:
		fprintf(out, "[");
		dumpsymref(out, e->left.sym);
		fprintf(out, ",");
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
    	fprintf(out, "<text><![CDATA[%s]]></text>", prop->text);
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
	//while (sym) {
		fprintf(out, "<symbol type=\"%s\" flags=\"%d\" id=\"%d\">\n", getSymType(sym->type), sym->flags, sym);

		if (sym->name)	
       		fprintf(out, "<name>%s</name>\n", sym->name);

       	for (prop = sym->prop; prop; prop = prop->next) {
       		dumpprop(out, prop);
       	}

		fprintf(out, "</symbol>\n");
		//sym = sym->next;
	//}
}

void dumpmenu(FILE *out, struct menu *menu) {
//	struct property *prop;
	struct symbol *sym;

	fprintf(out, "<menu flags=\"%d\">\n", menu->flags);
	if ((sym = menu->sym))
			dumpsymbol(out, sym);
//	if ((prop = menu->prompt)) {
//			dumpprop(out, prop);
//	}
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
	fprintf(stdout, "\n.\n");
	myconfdump(stdout);
	return 0;
}
