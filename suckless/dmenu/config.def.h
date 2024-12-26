/* See LICENSE file for copyright and license details. */
/* Default settings; can be overriden by command line. */

static int topbar = 1;                      /* -b  option; if 0, dmenu appears at bottom */
static int fuzzy  = 1;                      /* -F  option; if 0, dmenu doesn't use fuzzy matching */
static const unsigned int alpha = 0xff;
static const char *prompt = NULL;
static const char *fonts[] = { "Monocraft:size=18:antialias=false:autohint=false" };
// static const char *fonts[] = { "ShureTechMono Nerd Font:size=18:antialias=true:autohint=true" };
/* static const char *fonts[] = { "CozetteVector:size=18:antialias=false:autohint=true" }; */
// static const char *fonts[] = { "Monaspace Neon Light:size=18:antialias=false:autohint=true" };
/* static const char *fonts[] = { "Inconsolata:size=18:antialias=false:autohint=true" }; */
/* static const char *fonts[] = { "Liberation Mono:size=14:antialias=true:autohint=true" }; */
static const char *colors[SchemeLast][2] = {
    /* Rosé Pine https://rosepinetheme.com/palette */
    /*                         fg         bg       */
    [SchemeNorm]          = { "#ebbcba", "#1f1d2e" }, /* rose, surface */
    [SchemeSel]           = { "#9ccfd8", "#1f1d2e" }, /* foam, surface */
    [SchemeSelHighlight]  = { "#9ccfd8", "#1f1d2e" }, /* foam, surface */
    [SchemeNormHighlight] = { "#ebbcba", "#1f1d2e" }, /* rose, surface */
};

static const unsigned int alphas[SchemeLast][2] = {
    [SchemeNorm]          = { OPAQUE, alpha  },
    [SchemeSel]           = { OPAQUE, alpha  },
    [SchemeSelHighlight]  = { OPAQUE, OPAQUE },
    [SchemeNormHighlight] = { OPAQUE, OPAQUE },
};

static unsigned int lines          =  0;
static unsigned int lineheight     = 48;
static unsigned int min_lineheight =  8;

/*
 * Characters not considered part of a word while deleting words
 * for example: " /?\"&[]"
 */
static const char worddelimiters[] = "/[]{}()\"\'  ";

/* Size of the window border */
static unsigned int border_width = 4;
