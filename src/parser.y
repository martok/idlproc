%{
(*
Vorspann
 ****************************************************************************)

unit parser;

interface

uses
  SysUtils, pnode, lexlib, yacclib;

type
  YYSType = pnode.TPNode;

             
function yyparse : Integer;

var
  yyfilename: AnsiString;
  yyparseresult: YYSType;

  yinclude_expand: function (fn: AnsiString): AnsiString = nil;

implementation

const
   newline = #10;

var
  st_in_attrib_list: boolean = false;
  st_try_property: boolean = false;
  yycapture: AnsiString;

procedure yyerror ( msg : String );
begin
  writeln(ErrOutput, yyfilename, '(',yylineno,':',yycolno,'): at "',yytext,'"');
  WriteLn(ErrOutput, '  ',msg);
end(*yyerrmsg*);

procedure AcceptTree(PR: YYSType);
begin
  yyparseresult:= PR
end;

%}

%token ILLEGAL
%token COLON SEMICOLON COMMA EQUAL ASTERISK
%token LKLAMMER RKLAMMER LECKKLAMMER RECKKLAMMER LGKLAMMER RGKLAMMER
%token _LIBRARY _INTERFACE _MODULE _TYPEDEF _STRUCT _UNION _ENUM _CONST _CALLBACK
%token _IN _OUT _INOUT _ATTRIBUTE _READONLY
%token ID NUMBER CSTRING IID
%token EXTENSION INCLUDE


%right EQUAL
%right R_AND

%left UNEQUAL GT LT GTE LTE
%left QUESTIONMARK COLON
%%

idlfile
    : declaration_list               { AcceptTree($1); }
    ;

declaration_list
    : /* empty */                    { $$:= TPNode.CreateList(ntDocument); }
    | declaration_list declaration   { $$:= $1; $1.Append($2); }
    ;

attributes_maybe
    : /* empty */                             { $$:= nil; }
    | LECKKLAMMER attribute_list RECKKLAMMER  { $$:= TPNode.CreateList(ntAttributes); $$.AppendList($2); st_in_attrib_list:= false; }
    ;

attribute_list
    : attribute_list COMMA attribute          { $$:= TPNode.CreateList(ntTemporary); $$.AppendList($1); $$.Append($3); }
    | attribute                               { $$:= TPNode.CreateList(ntTemporary); $$.Append($1); }
    ;

attribute
    : attribute_tag ident LKLAMMER immediate RKLAMMER          { $$:= TPNode.Create(ntAttribute); $$.Name:= $2.Name; $$.Value:= $4; }
    | attribute_tag ident                                      { $$:= TPNode.Create(ntAttribute); $$.Name:= $2.Name; $$.Value:= nil; }
    ;

attribute_tag
    :                                         { st_in_attrib_list:= true; }
    ;

declaration
    : library                                 {  }
    | type_declaration                        {  }
    ;

library
    : attributes_maybe _LIBRARY ident LGKLAMMER type_declaration_list RGKLAMMER SEMICOLON   { $$:= TPNode.CreateList(ntLibrary); $$.Name:= $3.Name; $$.Attribs:= $1; $$.AppendList($5); }
    ;

type_declaration_list
    : /* Empty */                             { $$:= TPNode.CreateList(ntTemporary); }
    | type_declaration_list type_declaration  { $$:= $1; $1.Append($2); }
    ;

type_declaration
    : interface                               {  }
    | module                                  {  }      
    | const                                   {  }
    | directive                               {  }
    | typedef                                 {  }
    | EXTENSION                               { $$:= TPNode.Create(ntExtension); $$.Name:= yycapture; }
    ;

interface
    : attributes_maybe _INTERFACE ident interface_base LGKLAMMER interface_member_list RGKLAMMER SEMICOLON { $$:= TPNode.CreateList(ntInterface); $$.Name:= $3.Name; $$.Attribs:= $1; $$.Parent:= $4; $$.AppendList($6); }
    ;

module
    : attributes_maybe _MODULE ident LGKLAMMER module_member_list RGKLAMMER SEMICOLON { $$:= TPNode.CreateList(ntModule); $$.Name:= $3.Name; $$.Attribs:= $1; $$.AppendList($5); }
    ;

interface_base
    : /* empty */                             { $$:= nil; }
    | COLON interfaceparents                  { $$:= TPNode.CreateList(ntIntfParents); $$.AppendList($2); }
    ;

interfaceparents       
    : interfaceparents COMMA ident               { $$:= TPNode.CreateList(ntIntfParents); $$.AppendList($1); $$.Append($3); }
    | ident                                      { $$:= TPNode.CreateList(ntIntfParents); $$.Append($1); }
    ;

interface_member_list
    : /* Empty */                                { $$:= TPNode.CreateList(ntTemporary); }
    | interface_member_list interface_member     { $$:= $1; $$.Append($2); }
    ;

interface_member
    : property                                   { $$:= $1; }
    | method                                     { $$:= $1; }
    ;

module_member_list                                    
    : /* empty */                                { $$:= TPNode.CreateList(ntTemporary); }
    | module_member_list module_member           { $$:= $1; $1.Append($2); }
    ;

module_member
    : const                                   {  }
    | method                                  {  }
    ;

directive
    : attribute_tag ident LKLAMMER immediate RKLAMMER SEMICOLON                 { st_in_attrib_list:= false; $$:= TPNode.Create(ntDirective); $$.Name:= $2.Name; $$.Value:= $4; }

typedef
    : attributes_maybe _TYPEDEF enum ident SEMICOLON                            { $$:= TPNode.CreateList(ntEnum); $$.Name:= $4.Name; $$.Attribs:= $1; $$.AppendList($3); }
    | attributes_maybe _TYPEDEF struct ident SEMICOLON                          { $$:= TPNode.CreateList(ntStruct); $$.Name:= $4.Name; $$.Attribs:= $1; $$.AppendList($3); }
    | attributes_maybe _TYPEDEF typespec ident_array SEMICOLON                  { $$:= TPNode.Create(ntTypeAlias); $$.Name:= $4.Name; $$.Attribs:= $1; $$.typ:= $3; $$.typ.SetArraySpec($4); }
    | attributes_maybe _TYPEDEF _CALLBACK method                                { $$:= TPNode.Create(ntTypeAlias); $$.Name:= $4.Name; $$.typ:= $4; $4.Name:= ''; }
  /*  | _TYPEDEF union ident SEMICOLON          {  }       */
    ;


const
    : attributes_maybe _CONST typespec ident EQUAL immediate SEMICOLON          { $$:= TPNode.Create(ntConst); $$.Name:= $4.Name; $$.Attribs:= $1; $$.Value:= $6; $$.typ:= $3; }
    ;
        
property
    : attributes_maybe readonly_maybe _ATTRIBUTE typespec ident SEMICOLON       { $$:= TPNode.Create(ntProperty); $$.Name:= $5.Name; $$.Attribs:= $1; $$.Readonly:= Assigned($2); $$.typ:= $4; }
    ;

readonly_maybe
    : _READONLY                               { $$:= TPNode.Create(ntTemporary); }
    | /* Empty */                             { $$:= nil; }
    ;

method
    : attributes_maybe typespec ident LKLAMMER paramlist RKLAMMER SEMICOLON     { $$:= TPNode.CreateList(ntMethod); $$.Name:= $3.Name; $$.Attribs:= $1; $$.typ:= $2; if Assigned($5) then $$.AppendList($5); }
    ;

paramlist
    : paramlist COMMA param                   { $$:= TPNode.CreateList(ntTemporary); $$.AppendList($1); $$.Append($3); }
    | param                                   { $$:= TPNode.CreateList(ntTemporary); $$.Append($1); }
    |                                         { $$:= nil; }
    ;

param
    : attributes_maybe inoutspec typespec ident  { $$:= TPNode.Create(ntParam); $$.Name:= $4.Name; $$.Attribs:= $1; $$.typ:= $3; $$.inoutspec:= PtrUint($2); }
    ;

/* TODO: configure matches */
inoutspec
    : _IN                                     { $$:= TPNode(Pointer(PARAM_IN)); }
    | _OUT                                    { $$:= TPNode(Pointer(PARAM_OUT)); }
    | _INOUT                                  { $$:= TPNode(Pointer(PARAM_INOUT)); }
    | /* MSIDL: as attrib! */                 { $$:= TPNode(Pointer(PARAM_DEFAULT)); }
    ;

typespec
    : typespec ident                          { $$:= TPNode.Create(ntIdentifier); $$.Name:= $1.Name + ' ' + $2.Name; }
    | typespec ASTERISK                       { $$:= TPNode.Create(ntIdentifier); $$.Name:= $1.Name + ' *'; }
    | ident                                   { $$:= $1; }
    ;

enum
    : _ENUM LGKLAMMER enum_body RGKLAMMER     { $$:= $3; }
    ;

enum_body
    : enum_body COMMA enumitem                   { $$:= TPNode.CreateList(ntTemporary); $$.AppendList($1); $$.Append($3); }
    | enumitem                                   { $$:= TPNode.CreateList(ntTemporary); $$.Append($1); }
    ;

enumitem
    : ident EQUAL immediate                      { $$:= TPNode.Create(ntConst); $$.Name:= $1.Name; $$.Value:= $3; }
    | ident                                      { $$:= TPNode.Create(ntConst); $$.Name:= $1.Name; $$.Value:= nil; }
    ;

struct
    : _STRUCT LGKLAMMER struct_member_list RGKLAMMER      { $$:= $3; }
    ;

struct_member_list
    : /* Empty */                                { $$:= TPNode.CreateList(ntTemporary); }
    | struct_member_list struct_member           { $$:= $1; $$.Append($2); }
    ;

struct_member
    : typespec ident_array SEMICOLON             { $$:= TPNode.Create(ntField); $$.Name:= $2.Name; $$.typ:= $1; $$.typ.SetArraySpec($2);}
    ;

ident
    : ID                                         { $$:= TPNode.Create(ntIdentifier); $$.Name:= yytext; }
    ;

ident_array
    : ident array_dimensions                     { $$:= TPNode.CreateList(ntIdentifier); $$.Name:= $1.Name; $$.AppendList($2); }     
    | ident                                      {  }
    ;

array_dimensions
    : array_dimensions LECKKLAMMER array_dim RECKKLAMMER       { $$:= TPNode.CreateList(ntTemporary); $$.AppendList($1); $$.Append($3); }
    | LECKKLAMMER array_dim RECKKLAMMER                        { $$:= TPNode.CreateList(ntTemporary); $$.Append($2); }
    ;

array_dim
    : NUMBER                                                   { $$:= TPNode.Create(ntValueNumber); $$.Name:= yytext; }
    |                                                          { $$:= TPNode.Create(ntValueNumber); $$.Name:= ''; }
    ;

immediate
    : IID                                     { $$:= TPNode.Create(ntValueIID); $$.GUID:= StringToGUID('{'+yytext+'}'); }
    | NUMBER                                  { $$:= TPNode.Create(ntValueNumber); $$.Name:= yytext; }
    | CSTRING                                 { $$:= TPNode.Create(ntValueStr); $$.Name:= yytext; }
    | ident                                   { $$:= TPNode.Create(ntValueRef); $$.Name:= yytext; }
    ;

%%

{$I yyinclude.pp}
{$I lexer.inc}

end.

