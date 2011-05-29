%------------------------------------------------------------------------------%
% mexpat.m
% Peter Ross <pro@missioncriticalit.com>
%
% An interface to the expat parser.
%
%------------------------------------------------------------------------------%

:- module mexpat.

:- interface.

:- import_module sax_xml_parse.

:- import_module io.

:- type mexpat.

:- instance sax_xml_parser(mexpat.mexpat).

:- func init = mexpat.

:- pred xml_parse(string::in, more_xml_to_parse::in, parse_status::out,
    State::in, State::out, mexpat::in, mexpat::out) is det <= sax(State).

:- pred use_mexpat_as_default_parser(io::di, io::uo) is det.

%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%

:- implementation.

:- import_module char.
:- import_module int.
:- import_module list.
:- import_module require.
:- import_module string.

:- import_module xml.

:- type mexpat
        --->    mexpat(
                    parser  :: expat_parser
                ).

:- type user_data
        --->    some [T] user_data(T) => sax(T).

:- type expat_parser.
:- pragma foreign_type(c, expat_parser, "XML_Parser").
:- pragma foreign_type(erlang, expat_parser, "").       % stub only
:- pragma foreign_type(java, expat_parser, "Object").   % stub only
        
%------------------------------------------------------------------------------%

:- instance sax_xml_parser(mexpat.mexpat) where [
    (parse(S, M, R, !S, !P) :-
        mexpat.xml_parse(S, M, R, !S, !P)
    )
].

%------------------------------------------------------------------------------%
% We tell expat to use the nop function MEXPAT_free to free memory instead of
% GC_free, because I (iml) observed that expat was calling GC_free on memory
% not allocated by GC_malloc.  This caused the assertion on line 413 of
% malloc.c in boehm to be violated.  Any memory allocated with GC_malloc
% should be freed by boehm automatically anyway, so not using GC_free for
% expat is no great loss.
%

:- pragma foreign_decl("C",
"
extern void MEXPAT_free(void *ptr);
").

:- pragma foreign_code("C",
"
extern void MEXPAT_free(void *ptr) { return; }
").

%------------------------------------------------------------------------------%

init = mexpat(parser_create).

    %
    % XXX need to handle the encoding
    %
:- func parser_create = expat_parser.
:- pragma no_inline(parser_create/0).

:- pragma foreign_proc(c, parser_create = (Parser::out),
                [will_not_call_mercury, thread_safe, promise_pure], "
    mexpat_user_data    *user_data;

#ifdef MR_CONSERVATIVE_GC
    XML_Memory_Handling_Suite *suite;

    suite = MR_GC_NEW(XML_Memory_Handling_Suite);
    suite->malloc_fcn = MR_GC_malloc;
    suite->realloc_fcn = MR_GC_realloc;
    suite->free_fcn = MEXPAT_free;

    user_data = MR_GC_NEW(mexpat_user_data);

    Parser = XML_ParserCreate_MM(NULL, suite, NULL);
#else
    user_data = MR_NEW(mexpat_user_data);

    Parser = XML_ParserCreate(NULL);
#endif

    user_data->parser = Parser;
    user_data->user_data = (MR_Word) NULL;

    XML_SetUserData(Parser, user_data); 
    XML_SetElementHandler(Parser, startElement, endElement);
    XML_SetCharacterDataHandler(Parser, cdata);

    XML_SetUnknownEncodingHandler(Parser, mexpat_UnknownEncodingHandler, NULL);
").

:- pragma foreign_proc("Java", parser_create = (_Parser::out),
                [will_not_call_mercury, thread_safe, promise_pure], "
    if (1 == 1) throw new RuntimeException(\"not supported in Java grade\");
").
%------------------------------------------------------------------------------%

:- func call_start(string, list(xml.attribute), int, int, user_data) = user_data.
:- pragma export(call_start(in, in, in, in, in) = out, "MEXPAT_call_start").
call_start(S, As, Line, Col, user_data(Data)) = 'new user_data'(start(S, As, l(Line), c(Col), Data)).

:- func call_end(string, int, int, user_data) = user_data.
:- pragma export(call_end(in, in, in, in) = out, "MEXPAT_call_end").
call_end(S, Line, Col, user_data(Data)) = 'new user_data'(end(S, l(Line), c(Col), Data)).

:- func call_cdata(string, int, int, user_data) = user_data.
:- pragma export(call_cdata(in, in, in, in) = out, "MEXPAT_call_cdata").
call_cdata(S, Line, Col, user_data(Data)) = 'new user_data'(cdata(S, l(Line), c(Col), Data)).

:- func empty_attribute_list = list(xml.attribute).
:- pragma export(empty_attribute_list = out, "MEXPAT_empty_attribute_list").
empty_attribute_list = [].

:- func add_attribute(string, string, list(xml.attribute)) = list(xml.attribute).
:- pragma export(add_attribute(in, in, in) = out, "MEXPAT_add_attribute").
add_attribute(N, V, List) = [xml.attr(N, V) | List].

%------------------------------------------------------------------------------%

:- pragma foreign_decl(c, "
#define XML_STATIC
#include <expat.h>

typedef struct s_mexpat_user_data {
    XML_Parser  parser;
    MR_Word     user_data;
} mexpat_user_data;

").

:- pragma foreign_decl(c, local, "
#include ""mexpat.mh""

extern int  mexpat_windows1252_map[];

static void startElement(void *userData, const char *name, const char **atts);
static void endElement(void *userData, const char *name);
static void cdata(void *user_data, const char *s, int len);
static int  mexpat_UnknownEncodingHandler(void *,
                    const XML_Char *, XML_Encoding *);
").

:- pragma foreign_code(c, "
static void
startElement(void *userData, const char *name, const char **atts)
{
    int i;
    int line, col;
    mexpat_user_data *user_data = (mexpat_user_data *) userData;
    MR_Word list;
    MR_String str, str2;

    /* find the end of the attribute list */
    i = 0;
    while(atts[i]) {
        i += 2;
    }

    /* build the list of attributes starting from the end */
    list = MEXPAT_empty_attribute_list();
    for(; i > 0; i -= 2) {
        MR_make_aligned_string_copy(str, atts[i-2]);
        MR_make_aligned_string_copy(str2, atts[i-1]);
        list = MEXPAT_add_attribute(str, str2, list);
    }

    line = XML_GetCurrentLineNumber(user_data->parser);
    col = XML_GetCurrentColumnNumber(user_data->parser);

    MR_make_aligned_string_copy(str, name);
    user_data->user_data = MEXPAT_call_start(str, list, line, col, user_data->user_data);
}

static void
endElement(void *userData, const char *name)
{
    int line, col;
    mexpat_user_data *user_data = (mexpat_user_data *) userData;
    MR_String str;
    MR_make_aligned_string_copy(str, name);

    line = XML_GetCurrentLineNumber(user_data->parser);
    col = XML_GetCurrentColumnNumber(user_data->parser);

    user_data->user_data = MEXPAT_call_end(str, line, col, user_data->user_data);
}

static void cdata(void *userData, const char *s, int len)
{
    int line, col;
    mexpat_user_data *user_data = (mexpat_user_data *) userData;
    MR_Word	make_aligned_string_tmp;
    char	*make_aligned_string_ptr;

    MR_offset_incr_hp_atomic(make_aligned_string_tmp, 0,
            (len + sizeof(MR_Word)) / sizeof(MR_Word));
    make_aligned_string_ptr = (char *) make_aligned_string_tmp;	
    strncpy(make_aligned_string_ptr, s, len);
    make_aligned_string_ptr[len] = '\\0';

    line = XML_GetCurrentLineNumber(user_data->parser);
    col = XML_GetCurrentColumnNumber(user_data->parser);

    user_data->user_data = MEXPAT_call_cdata(make_aligned_string_ptr, line, col, user_data->user_data);
}

int mexpat_UnknownEncodingHandler(void *data,
        const XML_Char *name, XML_Encoding *info)
{
    int i;
    if (strcmp(name, ""Windows-1252"") != 0) {
        return 0;
    }

    for (i = 0; i < 256; i++)
    {
        info->map[i] = mexpat_windows1252_map[i];
    }
    info->data = NULL;
    info->convert = NULL;
    info->release = NULL;

    return 1;
}

int mexpat_windows1252_map[256] = {
    0x0000,	/* NULL */
    0x0001,	/* START OF HEADING */
    0x0002,	/* START OF TEXT */
    0x0003,	/* END OF TEXT */
    0x0004,	/* END OF TRANSMISSION */
    0x0005,	/* ENQUIRY */
    0x0006,	/* ACKNOWLEDGE */
    0x0007,	/* BELL */
    0x0008,	/* BACKSPACE */
    0x0009,	/* HORIZONTAL TABULATION */
    0x000A,	/* LINE FEED */
    0x000B,	/* VERTICAL TABULATION */
    0x000C,	/* FORM FEED */
    0x000D,	/* CARRIAGE RETURN */
    0x000E,	/* SHIFT OUT */
    0x000F,	/* SHIFT IN */
    0x0010,	/* DATA LINK ESCAPE */
    0x0011,	/* DEVICE CONTROL ONE */
    0x0012,	/* DEVICE CONTROL TWO */
    0x0013,	/* DEVICE CONTROL THREE */
    0x0014,	/* DEVICE CONTROL FOUR */
    0x0015,	/* NEGATIVE ACKNOWLEDGE */
    0x0016,	/* SYNCHRONOUS IDLE */
    0x0017,	/* END OF TRANSMISSION BLOCK */
    0x0018,	/* CANCEL */
    0x0019,	/* END OF MEDIUM */
    0x001A,	/* SUBSTITUTE */
    0x001B,	/* ESCAPE */
    0x001C,	/* FILE SEPARATOR */
    0x001D,	/* GROUP SEPARATOR */
    0x001E,	/* RECORD SEPARATOR */
    0x001F,	/* UNIT SEPARATOR */
    0x0020,	/* SPACE */
    0x0021,	/* EXCLAMATION MARK */
    0x0022,	/* QUOTATION MARK */
    0x0023,	/* NUMBER SIGN */
    0x0024,	/* DOLLAR SIGN */
    0x0025,	/* PERCENT SIGN */
    0x0026,	/* AMPERSAND */
    0x0027,	/* APOSTROPHE */
    0x0028,	/* LEFT PARENTHESIS */
    0x0029,	/* RIGHT PARENTHESIS */
    0x002A,	/* ASTERISK */
    0x002B,	/* PLUS SIGN */
    0x002C,	/* COMMA */
    0x002D,	/* HYPHEN-MINUS */
    0x002E,	/* FULL STOP */
    0x002F,	/* SOLIDUS */
    0x0030,	/* DIGIT ZERO */
    0x0031,	/* DIGIT ONE */
    0x0032,	/* DIGIT TWO */
    0x0033,	/* DIGIT THREE */
    0x0034,	/* DIGIT FOUR */
    0x0035,	/* DIGIT FIVE */
    0x0036,	/* DIGIT SIX */
    0x0037,	/* DIGIT SEVEN */
    0x0038,	/* DIGIT EIGHT */
    0x0039,	/* DIGIT NINE */
    0x003A,	/* COLON */
    0x003B,	/* SEMICOLON */
    0x003C,	/* LESS-THAN SIGN */
    0x003D,	/* EQUALS SIGN */
    0x003E,	/* GREATER-THAN SIGN */
    0x003F,	/* QUESTION MARK */
    0x0040,	/* COMMERCIAL AT */
    0x0041,	/* LATIN CAPITAL LETTER A */
    0x0042,	/* LATIN CAPITAL LETTER B */
    0x0043,	/* LATIN CAPITAL LETTER C */
    0x0044,	/* LATIN CAPITAL LETTER D */
    0x0045,	/* LATIN CAPITAL LETTER E */
    0x0046,	/* LATIN CAPITAL LETTER F */
    0x0047,	/* LATIN CAPITAL LETTER G */
    0x0048,	/* LATIN CAPITAL LETTER H */
    0x0049,	/* LATIN CAPITAL LETTER I */
    0x004A,	/* LATIN CAPITAL LETTER J */
    0x004B,	/* LATIN CAPITAL LETTER K */
    0x004C,	/* LATIN CAPITAL LETTER L */
    0x004D,	/* LATIN CAPITAL LETTER M */
    0x004E,	/* LATIN CAPITAL LETTER N */
    0x004F,	/* LATIN CAPITAL LETTER O */
    0x0050,	/* LATIN CAPITAL LETTER P */
    0x0051,	/* LATIN CAPITAL LETTER Q */
    0x0052,	/* LATIN CAPITAL LETTER R */
    0x0053,	/* LATIN CAPITAL LETTER S */
    0x0054,	/* LATIN CAPITAL LETTER T */
    0x0055,	/* LATIN CAPITAL LETTER U */
    0x0056,	/* LATIN CAPITAL LETTER V */
    0x0057,	/* LATIN CAPITAL LETTER W */
    0x0058,	/* LATIN CAPITAL LETTER X */
    0x0059,	/* LATIN CAPITAL LETTER Y */
    0x005A,	/* LATIN CAPITAL LETTER Z */
    0x005B,	/* LEFT SQUARE BRACKET */
    0x005C,	/* REVERSE SOLIDUS */
    0x005D,	/* RIGHT SQUARE BRACKET */
    0x005E,	/* CIRCUMFLEX ACCENT */
    0x005F,	/* LOW LINE */
    0x0060,	/* GRAVE ACCENT */
    0x0061,	/* LATIN SMALL LETTER A */
    0x0062,	/* LATIN SMALL LETTER B */
    0x0063,	/* LATIN SMALL LETTER C */
    0x0064,	/* LATIN SMALL LETTER D */
    0x0065,	/* LATIN SMALL LETTER E */
    0x0066,	/* LATIN SMALL LETTER F */
    0x0067,	/* LATIN SMALL LETTER G */
    0x0068,	/* LATIN SMALL LETTER H */
    0x0069,	/* LATIN SMALL LETTER I */
    0x006A,	/* LATIN SMALL LETTER J */
    0x006B,	/* LATIN SMALL LETTER K */
    0x006C,	/* LATIN SMALL LETTER L */
    0x006D,	/* LATIN SMALL LETTER M */
    0x006E,	/* LATIN SMALL LETTER N */
    0x006F,	/* LATIN SMALL LETTER O */
    0x0070,	/* LATIN SMALL LETTER P */
    0x0071,	/* LATIN SMALL LETTER Q */
    0x0072,	/* LATIN SMALL LETTER R */
    0x0073,	/* LATIN SMALL LETTER S */
    0x0074,	/* LATIN SMALL LETTER T */
    0x0075,	/* LATIN SMALL LETTER U */
    0x0076,	/* LATIN SMALL LETTER V */
    0x0077,	/* LATIN SMALL LETTER W */
    0x0078,	/* LATIN SMALL LETTER X */
    0x0079,	/* LATIN SMALL LETTER Y */
    0x007A,	/* LATIN SMALL LETTER Z */
    0x007B,	/* LEFT CURLY BRACKET */
    0x007C,	/* VERTICAL LINE */
    0x007D,	/* RIGHT CURLY BRACKET */
    0x007E,	/* TILDE */
    0x007F,	/* DELETE */
    0x20AC,	/* EURO SIGN */
    -1,     /* 0x81	UNDEFINED */
    0x201A,	/* SINGLE LOW-9 QUOTATION MARK */
    0x0192,	/* LATIN SMALL LETTER F WITH HOOK */
    0x201E,	/* DOUBLE LOW-9 QUOTATION MARK */
    0x2026,	/* HORIZONTAL ELLIPSIS */
    0x2020,	/* DAGGER */
    0x2021,	/* DOUBLE DAGGER */
    0x02C6,	/* MODIFIER LETTER CIRCUMFLEX ACCENT */
    0x2030,	/* PER MILLE SIGN */
    0x0160,	/* LATIN CAPITAL LETTER S WITH CARON */
    0x2039,	/* SINGLE LEFT-POINTING ANGLE QUOTATION MARK */
    0x0152,	/* LATIN CAPITAL LIGATURE OE */
    -1,     /* 0x8D	UNDEFINED */
    0x017D,	/* LATIN CAPITAL LETTER Z WITH CARON */
    -1,     /* 0x8F	UNDEFINED */
    -1,     /* 0x90	UNDEFINED */
    0x2018,	/* LEFT SINGLE QUOTATION MARK */
    0x2019,	/* RIGHT SINGLE QUOTATION MARK */
    0x201C,	/* LEFT DOUBLE QUOTATION MARK */
    0x201D,	/* RIGHT DOUBLE QUOTATION MARK */
    0x2022,	/* BULLET */
    0x2013,	/* EN DASH */
    0x2014,	/* EM DASH */
    0x02DC,	/* SMALL TILDE */
    0x2122,	/* TRADE MARK SIGN */
    0x0161,	/* LATIN SMALL LETTER S WITH CARON */
    0x203A,	/* SINGLE RIGHT-POINTING ANGLE QUOTATION MARK */
    0x0153,	/* LATIN SMALL LIGATURE OE */
    -1,     /* 0x9D UNDEFINED */
    0x017E,	/* LATIN SMALL LETTER Z WITH CARON */
    0x0178,	/* LATIN CAPITAL LETTER Y WITH DIAERESIS */
    0x00A0,	/* NO-BREAK SPACE */
    0x00A1,	/* INVERTED EXCLAMATION MARK */
    0x00A2,	/* CENT SIGN */
    0x00A3,	/* POUND SIGN */
    0x00A4,	/* CURRENCY SIGN */
    0x00A5,	/* YEN SIGN */
    0x00A6,	/* BROKEN BAR */
    0x00A7,	/* SECTION SIGN */
    0x00A8,	/* DIAERESIS */
    0x00A9,	/* COPYRIGHT SIGN */
    0x00AA,	/* FEMININE ORDINAL INDICATOR */
    0x00AB,	/* LEFT-POINTING DOUBLE ANGLE QUOTATION MARK */
    0x00AC,	/* NOT SIGN */
    0x00AD,	/* SOFT HYPHEN */
    0x00AE,	/* REGISTERED SIGN */
    0x00AF,	/* MACRON */
    0x00B0,	/* DEGREE SIGN */
    0x00B1,	/* PLUS-MINUS SIGN */
    0x00B2,	/* SUPERSCRIPT TWO */
    0x00B3,	/* SUPERSCRIPT THREE */
    0x00B4,	/* ACUTE ACCENT */
    0x00B5,	/* MICRO SIGN */
    0x00B6,	/* PILCROW SIGN */
    0x00B7,	/* MIDDLE DOT */
    0x00B8,	/* CEDILLA */
    0x00B9,	/* SUPERSCRIPT ONE */
    0x00BA,	/* MASCULINE ORDINAL INDICATOR */
    0x00BB,	/* RIGHT-POINTING DOUBLE ANGLE QUOTATION MARK */
    0x00BC,	/* VULGAR FRACTION ONE QUARTER */
    0x00BD,	/* VULGAR FRACTION ONE HALF */
    0x00BE,	/* VULGAR FRACTION THREE QUARTERS */
    0x00BF,	/* INVERTED QUESTION MARK */
    0x00C0,	/* LATIN CAPITAL LETTER A WITH GRAVE */
    0x00C1,	/* LATIN CAPITAL LETTER A WITH ACUTE */
    0x00C2,	/* LATIN CAPITAL LETTER A WITH CIRCUMFLEX */
    0x00C3,	/* LATIN CAPITAL LETTER A WITH TILDE */
    0x00C4,	/* LATIN CAPITAL LETTER A WITH DIAERESIS */
    0x00C5,	/* LATIN CAPITAL LETTER A WITH RING ABOVE */
    0x00C6,	/* LATIN CAPITAL LETTER AE */
    0x00C7,	/* LATIN CAPITAL LETTER C WITH CEDILLA */
    0x00C8,	/* LATIN CAPITAL LETTER E WITH GRAVE */
    0x00C9,	/* LATIN CAPITAL LETTER E WITH ACUTE */
    0x00CA,	/* LATIN CAPITAL LETTER E WITH CIRCUMFLEX */
    0x00CB,	/* LATIN CAPITAL LETTER E WITH DIAERESIS */
    0x00CC,	/* LATIN CAPITAL LETTER I WITH GRAVE */
    0x00CD,	/* LATIN CAPITAL LETTER I WITH ACUTE */
    0x00CE,	/* LATIN CAPITAL LETTER I WITH CIRCUMFLEX */
    0x00CF,	/* LATIN CAPITAL LETTER I WITH DIAERESIS */
    0x00D0,	/* LATIN CAPITAL LETTER ETH */
    0x00D1,	/* LATIN CAPITAL LETTER N WITH TILDE */
    0x00D2,	/* LATIN CAPITAL LETTER O WITH GRAVE */
    0x00D3,	/* LATIN CAPITAL LETTER O WITH ACUTE */
    0x00D4,	/* LATIN CAPITAL LETTER O WITH CIRCUMFLEX */
    0x00D5,	/* LATIN CAPITAL LETTER O WITH TILDE */
    0x00D6,	/* LATIN CAPITAL LETTER O WITH DIAERESIS */
    0x00D7,	/* MULTIPLICATION SIGN */
    0x00D8,	/* LATIN CAPITAL LETTER O WITH STROKE */
    0x00D9,	/* LATIN CAPITAL LETTER U WITH GRAVE */
    0x00DA,	/* LATIN CAPITAL LETTER U WITH ACUTE */
    0x00DB,	/* LATIN CAPITAL LETTER U WITH CIRCUMFLEX */
    0x00DC,	/* LATIN CAPITAL LETTER U WITH DIAERESIS */
    0x00DD,	/* LATIN CAPITAL LETTER Y WITH ACUTE */
    0x00DE,	/* LATIN CAPITAL LETTER THORN */
    0x00DF,	/* LATIN SMALL LETTER SHARP S */
    0x00E0,	/* LATIN SMALL LETTER A WITH GRAVE */
    0x00E1,	/* LATIN SMALL LETTER A WITH ACUTE */
    0x00E2,	/* LATIN SMALL LETTER A WITH CIRCUMFLEX */
    0x00E3,	/* LATIN SMALL LETTER A WITH TILDE */
    0x00E4,	/* LATIN SMALL LETTER A WITH DIAERESIS */
    0x00E5,	/* LATIN SMALL LETTER A WITH RING ABOVE */
    0x00E6,	/* LATIN SMALL LETTER AE */
    0x00E7,	/* LATIN SMALL LETTER C WITH CEDILLA */
    0x00E8,	/* LATIN SMALL LETTER E WITH GRAVE */
    0x00E9,	/* LATIN SMALL LETTER E WITH ACUTE */
    0x00EA,	/* LATIN SMALL LETTER E WITH CIRCUMFLEX */
    0x00EB,	/* LATIN SMALL LETTER E WITH DIAERESIS */
    0x00EC,	/* LATIN SMALL LETTER I WITH GRAVE */
    0x00ED,	/* LATIN SMALL LETTER I WITH ACUTE */
    0x00EE,	/* LATIN SMALL LETTER I WITH CIRCUMFLEX */
    0x00EF,	/* LATIN SMALL LETTER I WITH DIAERESIS */
    0x00F0,	/* LATIN SMALL LETTER ETH */
    0x00F1,	/* LATIN SMALL LETTER N WITH TILDE */
    0x00F2,	/* LATIN SMALL LETTER O WITH GRAVE */
    0x00F3,	/* LATIN SMALL LETTER O WITH ACUTE */
    0x00F4,	/* LATIN SMALL LETTER O WITH CIRCUMFLEX */
    0x00F5,	/* LATIN SMALL LETTER O WITH TILDE */
    0x00F6,	/* LATIN SMALL LETTER O WITH DIAERESIS */
    0x00F7,	/* DIVISION SIGN */
    0x00F8,	/* LATIN SMALL LETTER O WITH STROKE */
    0x00F9,	/* LATIN SMALL LETTER U WITH GRAVE */
    0x00FA,	/* LATIN SMALL LETTER U WITH ACUTE */
    0x00FB,	/* LATIN SMALL LETTER U WITH CIRCUMFLEX */
    0x00FC,	/* LATIN SMALL LETTER U WITH DIAERESIS */
    0x00FD,	/* LATIN SMALL LETTER Y WITH ACUTE */
    0x00FE,	/* LATIN SMALL LETTER THORN */
    0x00FF	/* LATIN SMALL LETTER Y WITH DIAERESIS */
};
").

%------------------------------------------------------------------------------%

xml_parse(Buffer, MoreXml, Result, !State, !MExpat) :-
    Length = string.length(Buffer),
    ( MoreXml = more_xml_to_parse,
        IsDoneInt = 0
    ; MoreXml = no_more_xml,
        IsDoneInt = 1
    ),
    UserData0 = 'new user_data'(!.State),
    xml_parse_c(!.MExpat ^ parser, Buffer, Length, IsDoneInt,
            IntResult, Line, Col, Error, UserData0, UserData, !MExpat),
    (
        UserData = user_data(State0),
        private_builtin.typed_unify(State0, State)
    ->
        !:State = State
    ;
        error("mexpat: unable to find out type of state")
    ),
    ( IntResult = 0 ->
        Result = error(l(Line), c(Col), Error)
    ;
        Result = ok
    ).

:- pred xml_parse_c(expat_parser::in, string::in, int::in, int::in,
                int::out, int::out, int::out, string::out,
                user_data::in, user_data::out,
                mexpat::in, mexpat::out) is det.
:- pragma foreign_proc(c, xml_parse_c(Parser::in, Buf::in,
                        Len::in, IsDone::in, Result::out,
                        LineNumber::out, ColNumber::out,
                        ErrorStr::out,
                        UserData0::in, UserData::out,
                        M0::in, M::out),
                [may_call_mercury, thread_safe, promise_pure], "
    mexpat_user_data *user_data;
    user_data = (mexpat_user_data *) XML_GetUserData(Parser);

    user_data->user_data = UserData0;

    Result = (XML_Parse(Parser, Buf, Len, IsDone) == XML_STATUS_OK);

    M = M0;

    if (Result) {
        LineNumber = -1;
        ColNumber = -1;
        ErrorStr = (MR_String) """";
    } else {
        LineNumber = XML_GetCurrentLineNumber(Parser);
        ColNumber = XML_GetCurrentColumnNumber(Parser);
        MR_make_aligned_string_copy(ErrorStr,
                XML_ErrorString(XML_GetErrorCode(Parser)));
    }

    UserData = user_data->user_data;
").

:- pragma foreign_proc("Java", xml_parse_c(_Parser::in, _Buf::in,
                        _Len::in, _IsDone::in, _Result::out,
                        _LineNumber::out, _ColNumber::out,
                        _ErrorStr::out,
                        _UserData0::in, _UserData::out,
                        _M0::in, _M::out),
                [may_call_mercury, thread_safe, promise_pure], "
    if (1 == 1) throw new RuntimeException(\"not supported in Java grade\");
").

%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%

use_mexpat_as_default_parser(!IO) :-
    set_default_sax_xml_parser((func) = mexpat.init, !IO).
    
%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et tw=0 wm=0
